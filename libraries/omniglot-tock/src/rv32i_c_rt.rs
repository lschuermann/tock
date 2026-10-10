use core::cell::Cell;
use core::ffi::{CStr, c_void};
use core::marker::PhantomData;
use core::mem::MaybeUninit;

use omniglot::abi::calling_convention::Stacked;
use omniglot::abi::calling_convention::{AREG0, AREG1, AREG2, AREG3, AREG4, AREG5, AREG6, AREG7};
use omniglot::abi::rv32i_c::Rv32iCABI;
use omniglot::alloc_tracker::AllocTracker;
use omniglot::foreign_memory::og_copy::OGCopy;
use omniglot::id::OGID;
use omniglot::markers::{AccessScope, AllocScope};
use omniglot::rt::rv32i_c::{Rv32iCBaseRt, Rv32iCInvokeRes, Rv32iCRt};
use omniglot::rt::{CallbackContext, CallbackReturn, OGRuntime};
use omniglot::{OGError, OGResult};

/// Memory protection for foreign code.
pub trait OGProtection {
    /// Restrict user mode to the memory foreign code may access. Called
    /// before every switch to foreign code.
    fn enable(&self);

    /// Called after foreign code returned, faulted, or trapped into a
    /// callback.
    ///
    /// ### Safety
    ///
    /// No user-mode code may run until the next [`OGProtection::enable`].
    unsafe fn disable(&self);
}

const MCAUSE_INSTRUCTION_ACCESS_FAULT: usize = 1;
const MCAUSE_ILLEGAL_INSTRUCTION: usize = 2;
const MCAUSE_ENV_CALL_UMODE: usize = 8;

#[repr(C)]
pub struct CallbackTrampolineFnReturn {
    reg0: usize,
    reg1: usize,
}

// Use 8 arguments, as that's how many are passed in registers on RISC-V.
type CallbackTrampolineFn = extern "C" fn(
    usize,
    usize,
    usize,
    usize,
    usize,
    usize,
    usize,
    usize,
) -> CallbackTrampolineFnReturn;

// Only read by the RISC-V assembly.
#[cfg_attr(
    not(any(target_arch = "riscv32", target_arch = "riscv64")),
    allow(dead_code)
)]
#[derive(Clone, Debug)]
pub struct TockRv32iCRtCallbackAsmContext {
    foreign_stack_ptr: *mut (),
    runtime: *mut TockRv32iCRtAsmState,
    ret_a0: usize,
    ret_a1: usize,
}

#[derive(Debug, Clone)]
pub struct TockRv32iCRtCallbackContext {
    pub arg_regs: [usize; 8],
}

impl CallbackContext for TockRv32iCRtCallbackContext {
    fn get_argument_register(&self, reg: usize) -> Option<usize> {
        self.arg_regs.get(reg).copied()
    }
}

#[derive(Debug, Clone)]
pub struct TockRv32iCRtCallbackReturn {
    pub ret_regs: [usize; 2],
}

impl CallbackReturn for TockRv32iCRtCallbackReturn {
    fn set_return_register(&mut self, reg: usize, value: usize) -> bool {
        if let Some(r) = self.ret_regs.get_mut(reg) {
            *r = value;
            true
        } else {
            false
        }
    }
}

// Only used by the RISC-V assembly.
#[cfg_attr(
    not(any(target_arch = "riscv32", target_arch = "riscv64")),
    allow(dead_code)
)]
const CALLBACK_CONTEXT_PLUS_POINTER_SIZE: usize =
    core::mem::size_of::<TockRv32iCRtCallbackAsmContext>()
        + core::mem::size_of::<*mut TockRv32iCRtCallbackAsmContext>();

#[cfg_attr(
    not(any(target_arch = "riscv32", target_arch = "riscv64")),
    allow(dead_code)
)]
const CALLBACK_CONTEXT_PLUS_POINTER_STACKED_SIZE: usize =
    (CALLBACK_CONTEXT_PLUS_POINTER_SIZE + 15) & !15;

#[derive(Clone, Debug)]
pub struct TockRv32iCRtAllocations {
    ram_region_start: *mut (),
    ram_region_length: usize,
    flash_region_start: *mut (),
    flash_region_length: usize,
}

impl TockRv32iCRtAllocations {
    fn is_valid(&self, ptr: *const (), len: usize) -> bool {
        let is_valid_flash = (ptr as usize) >= (self.flash_region_start as usize)
            && ((ptr as usize)
                .checked_add(len)
                .map(|end| end <= (self.flash_region_start as usize) + self.flash_region_length)
                .unwrap_or(false));

        is_valid_flash || self.is_valid_mut(ptr as *mut (), len)
    }

    fn is_valid_mut(&self, ptr: *mut (), len: usize) -> bool {
        (ptr as usize) >= (self.ram_region_start as usize)
            && ((ptr as usize)
                .checked_add(len)
                .map(|end| end <= (self.ram_region_start as usize) + self.ram_region_length)
                .unwrap_or(false))
    }
}

#[derive(Debug)]
pub struct TockRv32iCCallbackDescriptor<'a> {
    // filled in with an illegal instruction opcode (0x00000000)
    springboard: u32,
    wrapper: unsafe extern "C" fn(
        *mut c_void,
        &TockRv32iCRtCallbackContext,
        &mut TockRv32iCRtCallbackReturn,
        *mut (),
        *mut (),
    ),
    context: *mut c_void,
    _lt: PhantomData<&'a mut c_void>,
}

impl TockRv32iCCallbackDescriptor<'_> {
    /// ### Safety
    ///
    /// `alloc_scope` and `access_scope` must point to the scopes the wrapper
    /// expects (those of the runtime that created this descriptor).
    unsafe fn invoke(
        &self,
        callback_ctx: &TockRv32iCRtCallbackContext,
        callback_ret: &mut TockRv32iCRtCallbackReturn,
        alloc_scope: *mut (),
        access_scope: *mut (),
    ) {
        (self.wrapper)(
            self.context,
            callback_ctx,
            callback_ret,
            alloc_scope,
            access_scope,
        )
    }
}

#[derive(Debug)]
pub enum TockRv32iCRtAllocChain<'a> {
    BaseAllocations(TockRv32iCRtAllocations),
    CallbackDescriptor(
        TockRv32iCCallbackDescriptor<'a>,
        &'a TockRv32iCRtAllocChain<'a>,
    ),
    Cons(&'a TockRv32iCRtAllocChain<'a>),
}

impl<'a> TockRv32iCRtAllocChain<'a> {
    fn get_base_allocations(&self) -> &TockRv32iCRtAllocations {
        let mut cur = self;
        loop {
            match cur {
                TockRv32iCRtAllocChain::BaseAllocations(base) => {
                    return base;
                }
                TockRv32iCRtAllocChain::CallbackDescriptor(_, pred) => {
                    cur = pred;
                }
                TockRv32iCRtAllocChain::Cons(pred) => {
                    cur = pred;
                }
            }
        }
    }
}

unsafe impl AllocTracker for TockRv32iCRtAllocChain<'_> {
    fn is_valid(&self, ptr: *const (), len: usize) -> bool {
        self.get_base_allocations().is_valid(ptr, len)
    }

    fn is_valid_mut(&self, ptr: *mut (), len: usize) -> bool {
        self.get_base_allocations().is_valid_mut(ptr, len)
    }
}

#[repr(usize)]
enum TockRv32iCInvokeErr {
    NoError,
    NotCalled,
    /// Foreign code faulted, or the kernel refused to continue it; see
    /// [`TockRv32iCRt::take_fault`].
    Fault,
}

/// Why a call into foreign code ended without returning.
#[derive(Clone, Copy, Debug)]
pub struct ForeignFault {
    pub mcause: usize,
    pub mtval: usize,
    pub mepc: usize,
    /// Foreign stack pointer at the fault.
    pub sp: usize,
    /// Set if the kernel ended the call itself, because of an access fault
    /// it would not emulate.
    pub refused: Option<&'static str>,
}

// Depending on the size of the return value, it will be either passed as a
// pointer on the stack as the first argument, or be written to a0 and a1. In
// either case, this InvokeRes type is passed by reference (potentially on the
// stack), such that we can even encode values that exceed the available two
// return registers. If a return value was passed by invisible reference, we
// will be passed a pointer to that:
#[repr(C)]
pub struct TockRv32iCInvokeResInner {
    error: TockRv32iCInvokeErr,
    a0: usize,
    a1: usize,
    sp: *const (),
}

#[repr(C)]
pub struct TockRv32iCInvokeRes<RT: Rv32iCBaseRt, T> {
    inner: TockRv32iCInvokeResInner,
    _t: PhantomData<T>,
    _rt: PhantomData<RT>,
}

impl<RT: Rv32iCBaseRt, T> TockRv32iCInvokeRes<RT, T> {
    fn encode_ogerror(&self) -> OGResult<()> {
        match self.inner.error {
            TockRv32iCInvokeErr::NotCalled => panic!(
                "Attempted to use / query {} without it being used by an invoke call!",
                core::any::type_name::<Self>()
            ),

            TockRv32iCInvokeErr::NoError => Ok(()),

            TockRv32iCInvokeErr::Fault => Err(OGError::InternalError),
        }
    }
}

unsafe impl<RT: Rv32iCBaseRt, T> Rv32iCInvokeRes<RT, T> for TockRv32iCInvokeRes<RT, T> {
    fn new() -> Self {
        // Required invariant by our assembly. The invoke functions require a
        // reference to this type being passed in, which means that this
        // assertion is guaranteed to evaluate before any assembly that relies
        // on this invariant is being run:
        let _: () = assert!(core::mem::offset_of!(Self, inner) == 0);

        TockRv32iCInvokeRes {
            inner: TockRv32iCInvokeResInner {
                error: TockRv32iCInvokeErr::NotCalled,
                a0: 0,
                a1: 0,
                sp: core::ptr::null(),
            },
            _t: PhantomData,
            _rt: PhantomData,
        }
    }

    fn into_result_registers(self, _rt: &RT) -> OGResult<OGCopy<T>> {
        self.encode_ogerror()?;

        // Basic assumptions in this method:
        // - sizeof(usize) == sizeof(u64)
        // - little endian
        assert!(core::mem::size_of::<usize>() == core::mem::size_of::<u32>());
        const { assert!(cfg!(target_endian = "little")) };

        // This function must not be called on types larger than two pointers
        // (64 bit), as those cannot possibly be encoded in the two available
        // 32-bit return registers:
        assert!(core::mem::size_of::<T>() <= 2 * core::mem::size_of::<*const ()>());

        // Allocate space to construct the final (unvalidated) T from
        // the register values. During copy, we treat the memory of T
        // as integers:
        let mut ret_uninit: MaybeUninit<T> = MaybeUninit::uninit();

        // TODO: currently, we only support power-of-two return values.
        // It is not immediately obvious how values that are, e.g.,
        // 9 byte in size would be encoded into registers.
        let a0_bytes = u32::to_le_bytes(self.inner.a0 as u32);
        let a1_bytes = u32::to_le_bytes(self.inner.a1 as u32);
        let ret_bytes = [
            a0_bytes[0],
            a0_bytes[1],
            a0_bytes[2],
            a0_bytes[3],
            a1_bytes[0],
            a1_bytes[1],
            a1_bytes[2],
            a1_bytes[3],
        ];

        ret_uninit
            .as_bytes_mut()
            .write_copy_of_slice(&ret_bytes[..core::mem::size_of::<T>()]);

        OGResult::Ok(ret_uninit.into())
    }

    unsafe fn into_result_stacked(self, _rt: &RT, stacked_res: *mut T) -> OGResult<OGCopy<T>> {
        self.encode_ogerror()?;

        // Allocate space to construct the final (unvalidated) T from
        // the register values. During copy, we treat the memory of T
        // as integers:
        let mut ret_uninit: MaybeUninit<T> = MaybeUninit::uninit();

        // Now, we simply to a memcpy from our pointer. We trust the caller
        // that is allocated, non-aliased over any Rust struct, not being
        // mutated and accessible to us. We cast it into a layout-compatible
        // MaybeUninit pointer:
        unsafe {
            core::ptr::copy_nonoverlapping(stacked_res as *const T, ret_uninit.as_mut_ptr(), 1)
        };

        OGResult::Ok(ret_uninit.into())
    }
}

#[repr(C)]
pub struct TockRv32iCRtAsmState {
    // Foreign stack pointer, read by the protection-domain switch assembly
    // and used as a base to copy stacked arguments & continue execution from:
    foreign_stack_ptr: Cell<*mut ()>,

    // Foreign stack bottom (inclusive). Last usable stack address:
    foreign_stack_bottom: *mut (),

    // TODO: doc
    ram_region_start: *mut (),
    ram_region_length: usize,

    // Allocation scope active across an invocation of generic_invoke,
    // set in the `execute` hook:
    active_alloc_scope: Cell<*mut ()>,

    // Store a reference to the memory protection to disable it for
    // callbacks. Needs to be cast back to the concrete type once it's known.
    prot: *const (),

    // Loads and stores that faulted in foreign code are emulated if this
    // allows them (see `set_access_emulation`).
    access_emulation: Cell<Option<&'static dyn AccessEmulation>>,

    // Why the kernel ended the current call, set just before it does so.
    refusal: Cell<Option<&'static str>>,

    // The last fault, until taken.
    fault: Cell<Option<ForeignFault>>,
}

/// Emulation of the loads and stores of foreign code that the memory
/// protection denies; see [`TockRv32iCRt::set_access_emulation`].
pub trait AccessEmulation {
    /// Whether to perform an access of `width` bytes at `addr` for foreign
    /// code: `store` is the value for a store, `None` for a load.
    fn allow(&self, addr: usize, width: usize, store: Option<usize>) -> bool;

    /// Where the kernel can read the foreign instruction at `pc`, to decode
    /// it. Override for code the kernel cannot read at the address it runs
    /// at (e.g. SRAM that is executed through an instruction-bus alias).
    fn code_address(&self, pc: usize) -> usize {
        pc
    }
}

#[repr(C)]
pub struct TockRv32iCRt<ID: OGID, P: OGProtection + 'static> {
    // This struct is used both in the protection-domain switch assembly,
    // and in regular Rust code. However, we want to avoid hard-coding offsets
    // into this struct in assembly, and instead use ::core::ptr::offset_of!
    // to resolve offsets of relevant fields at compile. Unfortunately, that is
    // not possible, in general, for a generic type without knowing the generic
    // argument. Instead, we move all assembly-relevant state into a separate
    // struct `TockRv32iCRtAsmState`, which does not have generic parameters.
    // We ensure that this struct is placed at the very beginning of the
    // `TockRv32iCRt` type, for every possible combination of generic
    // parameters, through an assertion in its constructor.
    asm_state: TockRv32iCRtAsmState,

    fntab_addr: *const *const (),
    fntab_length: usize,

    prot: &'static P,

    _id: PhantomData<ID>,
}

/// Where foreign code and its memory are. The code is in place, and the RAM
/// is initialized (`.data` copied, `.bss` zeroed).
pub struct ForeignMemory {
    /// The code and read-only data, (start, length).
    pub code: (*const (), usize),
    /// The RAM, (start, length). The foreign stack grows down from
    /// `stack_top` towards its start.
    pub ram: (*mut (), usize),
    pub stack_top: *mut (),
    /// The function table, (start, length in pointers), which the kernel
    /// reads to find foreign functions.
    pub fntab: (*const *const (), usize),
}

/// A new runtime, with its initial allocation and access scopes.
pub type NewTockRv32iCRt<ID, P> = (
    TockRv32iCRt<ID, P>,
    AllocScope<'static, TockRv32iCRtAllocChain<'static>, ID>,
    AccessScope<ID>,
);

impl<ID: OGID, P: OGProtection + 'static> TockRv32iCRt<ID, P> {
    /// Create a runtime for the foreign code in `mem`, which `prot`
    /// confines.
    ///
    /// ### Safety
    ///
    /// `mem` must describe memory reserved for the foreign code, with
    /// `stack_top` inside `ram` and a function table of foreign functions in
    /// `code`; and `ogid` must be unique to this runtime.
    pub unsafe fn new(prot: &'static P, mem: ForeignMemory, ogid: ID) -> NewTockRv32iCRt<ID, P> {
        // See the TockRv32iCRt type definition for an explanation of this
        // const assertion. It is required to allow us to index into fields
        // of the nested `TockRv32iCRtAllocChain` struct from within assembly.
        //
        // Unfortunately, we cannot make this into a const assertion, as
        // constants are instantiated outside of the `impl` block.
        let _: () = assert!(core::mem::offset_of!(Self, asm_state) == 0);

        let (ram_region_start, ram_region_length) = mem.ram;
        let rt = TockRv32iCRt {
            asm_state: TockRv32iCRtAsmState {
                foreign_stack_ptr: Cell::new(mem.stack_top),
                foreign_stack_bottom: ram_region_start,
                ram_region_start,
                ram_region_length,
                active_alloc_scope: Cell::new(core::ptr::null_mut()),
                prot: prot as *const P as *const (),
                access_emulation: Cell::new(None),
                refusal: Cell::new(None),
                fault: Cell::new(None),
            },

            fntab_addr: mem.fntab.0,
            fntab_length: mem.fntab.1,

            prot,

            _id: PhantomData::<ID>,
        };

        let alloc_scope = unsafe {
            AllocScope::new(
                TockRv32iCRtAllocChain::BaseAllocations(TockRv32iCRtAllocations {
                    ram_region_start,
                    ram_region_length,
                    flash_region_start: mem.code.0 as *mut (),
                    flash_region_length: mem.code.1,
                }),
                ogid.get_imprint(),
            )
        };

        (rt, alloc_scope, unsafe {
            AccessScope::new(ogid.get_imprint())
        })
    }

    /// Emulate the loads and stores of foreign code that fault on the memory
    /// protection, if `emulation` allows them: the kernel performs the
    /// access and foreign code continues after the instruction. Any other
    /// access fault ends the call with an error.
    pub fn set_access_emulation(&self, emulation: &'static dyn AccessEmulation) {
        self.asm_state.access_emulation.set(Some(emulation));
    }

    /// The fault that ended the last call that returned an error, if not yet
    /// taken.
    pub fn take_fault(&self) -> Option<ForeignFault> {
        self.asm_state.fault.take()
    }

    fn execute_protected<R, F: FnOnce() -> R>(&self, f: F) -> R {
        self.prot.enable();

        let res = f();

        // ### Safety
        //
        // Foreign code has returned (or faulted) at this point, so no
        // user-mode code is executing that relies on the protection being
        // enabled.
        unsafe { self.prot.disable() };

        res
    }

    /// Host stub, so that the crate builds for documentation and tests.
    ///
    /// ### Safety
    ///
    /// Never call this.
    #[cfg(not(any(target_arch = "riscv32", target_arch = "riscv64")))]
    unsafe extern "C" fn generic_invoke() {
        unimplemented!();

        // To avoid complaints about functions only the assembly calls:
        #[allow(unreachable_code)]
        {
            let _ = (Self::encode_return, Self::callback_handler, emulate_access);
        }
    }

    /// Called from the trap handler for foreign code: handle a callback if
    /// the trap is one, and return 1; otherwise return 0.
    ///
    /// ### Safety
    ///
    /// Only the trap handler may call this, with the foreign argument
    /// registers and a `TockRv32iCRtCallbackAsmContext` for this runtime,
    /// while an invocation of it is active.
    unsafe extern "C" fn callback_handler(
        // Argument registers:
        a0: usize,
        a1: usize,
        a2: usize,
        a3: usize,
        a4: usize,
        a5: usize,
        a6: usize,
        a7: usize,
        // Stacked:
        callback_asm_ctx_ptr: *mut TockRv32iCRtCallbackAsmContext,
    ) -> usize {
        let mcause: usize;
        core::arch::asm!(
            "csrr {mcause_reg}, mcause",
            options(pure, nomem),
            mcause_reg = out(reg) mcause
        );

        let mepc: usize;
        core::arch::asm!(
            "csrr {mepc_reg}, mepc",
            options(pure, nomem),
            mepc_reg = out(reg) mepc
        );

        // ecall -> function return
        let ecall_function_return = mcause == MCAUSE_ENV_CALL_UMODE;

        // instruction access fault at return springboard -> function return
        let iaf_function_return = mcause == MCAUSE_INSTRUCTION_ACCESS_FAULT
            && mepc == og_tock_rv32i_c_rt_ret_springboard as *const () as usize;

        let function_return = ecall_function_return || iaf_function_return;

        // callbacks must be triggered through an instruction access fault or an
        // ILLEGAL_INSTRUCTION exception:
        let callback_fault =
            mcause == MCAUSE_INSTRUCTION_ACCESS_FAULT || mcause == MCAUSE_ILLEGAL_INSTRUCTION;

        if function_return || !callback_fault {
            // Either a function return or other non-callback fault, return:
            return 0;
        }

        // This _is_ a callback, handle it!

        let callback_asm_ctx = &mut *callback_asm_ctx_ptr;
        let runtime = &*callback_asm_ctx.runtime;

        // Disable the memory protection:
        let prot = &*(runtime.prot as *const P);
        prot.disable();

        let alloc_scope = &*(runtime.active_alloc_scope.get()
            as *mut AllocScope<'_, <Self as OGRuntime>::AllocTracker<'_>, <Self as OGRuntime>::ID>);

        // Check if the faulting instruction coincides with an alloc_scope
        // chain callback descriptor's illegal instruction placeholder:
        let mut cur = alloc_scope.tracker();

        let callback_desc = loop {
            match cur {
                TockRv32iCRtAllocChain::BaseAllocations(_) => {
                    // No callback found:
                    break None;
                }
                TockRv32iCRtAllocChain::CallbackDescriptor(desc, pred) => {
                    // Check if this callback has a matching springboard:
                    let springboard_ptr = unsafe {
                        (desc as *const TockRv32iCCallbackDescriptor as *const ()).byte_offset(
                            core::mem::offset_of!(TockRv32iCCallbackDescriptor, springboard)
                                as isize,
                        )
                    };

                    if mepc == springboard_ptr as usize {
                        // Springboard matches this callback:
                        break Some(desc);
                    } else {
                        // Check the predecessor:
                        cur = pred;
                    }
                }
                TockRv32iCRtAllocChain::Cons(pred) => {
                    cur = pred;
                }
            }
        };

        let Some(desc) = callback_desc else {
            // This is not a callback invocation, proceed returning to the kernel:
            return 0;
        };

        // Construct a CallbackContext from the arguments to this function:
        let callback_ctx = TockRv32iCRtCallbackContext {
            arg_regs: [a0, a1, a2, a3, a4, a5, a6, a7],
        };

        // Construct a default CallbackReturn:
        let mut callback_ret = TockRv32iCRtCallbackReturn { ret_regs: [0; 2] };

        // Execute the interrupt handler function.
        //
        // TODO: In the future, we should transition this out of the trap
        // handler to allow for nested domain switches.
        let mut inner_alloc_scope: AllocScope<'_, TockRv32iCRtAllocChain<'_>, ID> = AllocScope::new(
            TockRv32iCRtAllocChain::Cons(alloc_scope.tracker()),
            alloc_scope.id_imprint(),
        );

        // Safe, as this should only be triggered by foreign code, when the only
        // existing AccessScope<ID> is already borrowed by the trampoline:
        let mut access_scope = AccessScope::<ID>::new(alloc_scope.id_imprint());
        desc.invoke(
            &callback_ctx,
            &mut callback_ret,
            &mut inner_alloc_scope as *mut _ as *mut (),
            &mut access_scope as *mut _ as *mut (),
        );

        callback_asm_ctx.ret_a0 = callback_ret.ret_regs[0];
        callback_asm_ctx.ret_a1 = callback_ret.ret_regs[1];

        // Re-enable the memory protection:
        prot.enable();

        // This was a callback!
        1
    }

    #[cfg(any(target_arch = "riscv32", target_arch = "riscv64"))]
    #[unsafe(naked)]
    unsafe extern "C" fn generic_invoke() {
        core::arch::naked_asm!(
            "
                // When entering this symbol, we are supposed to invoke a
                // foreign function in an isolated protection domain (switching
                // to user-mode and thus engaging the PMP).
                //
                // At this stage, the PMP has already been set up (through the
                // call to `execute`). This is an unsafe symbol, and callers
                // of this symbol must transitively guarantee that this function
                // is only invoked in the `execute` closure, and this is the
                // only function in that closure which may ever attempt to
                // modify the PMP state or its enforcement).
                //
                // This symbol is effectively indirectly aliased to a bunch of
                // extern \"C\" functions, which cause the Rust compiler / LLVM
                // to place the function arguments in the correct registers on
                // our Rust stack. We must thus avoid clobbering all such state
                // until we invoke the function.
                //
                // The RT::invoke `#[unsafe(naked)]` wrapper functions also load some
                // const-generic data and other information into a set of
                // well-defined registers; specifically
                // - t0: &TockRv32iCRtAsmState
                // - t1: function pointer to execute
                // - t2: &mut TockRv32iCRtInvokeResInner
                // - t3: amount of bytes spilled on the stack, to copy
                // - t4: <this function symbol>
                //
                // As this symbol follows the C ABI / calling convention, and we
                // cannot rely on the foreign code to preserve saved registers,
                // we must do so here.
                //
                // Following that, we need to copy the stack-spilled arguments
                // onto the foreign stack and switch to user-mode.
                //
                // The trap handler may return to this code because either
                //
                // - we received an interrupt while executing foreign code.
                //   In this case, disable the interrupt, and resume execution
                //   of foreign code, being careful to not clobber any
                //   registers.
                //
                // - we received a system call (return instruction), or a trap.
                //   It is not possible for us to modify the foreign binary and
                //   insert a system call instruction with execute permissions.
                //   Thus, when memory protection is engaged, we rely on the
                //   the foreign code to attempt to execute a system call inst.,
                //   which then causes a fault at a well-known address. We then
                //   intepret this fault as an attempted system call too.
                //
                //   We define an analog springboard for callbacks, using the
                //   `unimp` mnemonic (to distinguish it from system calls when
                //   not engaging memory protection.
                //
                //   All other traps are faults and should require
                //   re-initialization of the Omniglot runtime.

                // First, save the current stack pointer in a temporary
                // register. We start copying foreign arguments from this point
                // onward in a bit.
                mv  t5, sp

                // Now, save all callee-saved registers, non-clobberable
                // reigsters (e.g., fp, gp), and other important state on the
                // stack. The stack layout is set up to be compatible with the
                // assumptions of the Tock rv32i trap handler. We also reserve
                // space for caller-saved registers of foreign code, for when we
                // need to disable interrupts with a Rust function (exposing a
                // C ABI). Doing this here, and writing beyond this space
                // prevents stack overflows later on.
                //
                // ```
                //  40*4(sp): <- original stack pointer
                //  39*4(sp):
                //  38*4(sp):
                // ^^^^^^^^^^ Other Interrupt-Saved Registers ^^^^^^^^^^^^^^^^^^
                //  37*4(sp): x4  / tp (we swap to Rust tp)
                //  36*4(sp): x3  / gp (we swap to Rust gp)
                //  35*4(sp): x2  / sp (not caller-saved, but we swap to Rust sp)
                // vvvvvvvvvv Foreign Caller-Saved Registers vvvvvvvvvvvvvvvvvvv
                //  34*4(sp): x31 / t6
                //  33*4(sp): x30 / t5
                //  32*4(sp): x29 / t4
                //  31*4(sp): x28 / t3
                //  30*4(sp): x17 / a7
                //  29*4(sp): x16 / a6
                //  28*4(sp): x15 / a5
                //  27*4(sp): x14 / a4
                //  26*4(sp): x13 / a3
                //  25*4(sp): x12 / a2
                //  24*4(sp): x11 / a1
                //  23*4(sp): x10 / a0
                //  22*4(sp): x7  / t2
                //  21*4(sp): x6  / t1
                //  20*4(sp): x5  / t0
                //  19*4(sp): x1  / ra
                // ^^^^^^^^^^ Foreign Caller-Saved Registers ^^^^^^^^^^^^^^^^^^^
                // vvvvvvvvvv Kernel Callee-Saved Registers vvvvvvvvvvvvvvvvvvvv
                //  18*4(sp): x27 / s11
                //  17*4(sp): x26 / s10
                //  16*4(sp): x25 / s9
                //  15*4(sp): x24 / s8
                //  14*4(sp): x23 / s7
                //  13*4(sp): x22 / s6
                //  12*4(sp): x21 / s5
                //  11*4(sp): x20 / s4
                //  10*4(sp): x19 / s3
                //   9*4(sp): x18 / s2
                //   8*4(sp): x9  / s1
                //   7*4(sp): x8  / s0 / fp
                //   6*4(sp): x4  / tp
                //   5*4(sp): x3  / gp
                // ^^^^^^^^^^ Kernel Callee-Saved Registers ^^^^^^^^^^^^^^^^^^^^
                // vvvvvvvvvv Kernel Caller-Saved Registers vvvvvvvvvvvvvvvvvvvv
                //   4*4(sp): &mut TockRv32iCRtInvokeResInner (x7 / t2)
                //   3*4(sp): &TockRv32iCRtAsmState (x5 / t0)
                //   2*4(sp): x1  / ra (not callee-saved, but we clobber)
                // vvvvvvvvvv Trap Handler Context vvvvvvvvvvvvvvvvvvvvvvvvvvvvv
                //   1*4(sp): custom trap handler address
                //   0*4(sp): scratch space, s1 written to by trap handler
                //            <- new stack pointer
                // ```
                //
                // We don't need to save the stack pointer, as it will be
                // preserved in `mscratch` below.

                addi sp, sp, -40*4  // Move the stack pointer down to make room.

                // Save all registers according to the above memory map:
                sw   x27, 18*4(sp)  // s11
                sw   x26, 17*4(sp)  // s10
                sw   x25, 16*4(sp)  // s9
                sw   x24, 15*4(sp)  // s8
                sw   x23, 14*4(sp)  // s7
                sw   x22, 13*4(sp)  // s6
                sw   x21, 12*4(sp)  // s5
                sw   x20, 11*4(sp)  // s4
                sw   x19, 10*4(sp)  // s3
                sw   x18,  9*4(sp)  // s2
                sw    x9,  8*4(sp)  // s1
                sw    x8,  7*4(sp)  // s0 / fp
                sw    x4,  6*4(sp)  // tp
                sw    x3,  5*4(sp)  // gp
                sw    x7,  4*4(sp)  // t2
                sw    x5,  3*4(sp)  // t0
                sw    x1,  2*4(sp)  // ra

                // At this point, we are free to clobber saved registers. We
                // embrace using registers like `s0` and `s1`, as they fit into
                // compressed loads and stores.

                // Load the address of `_start_og_trap` into `1*4(sp)`. We swap
                // our stack pointer into the mscratch CSR and the trap handler
                // will load and jump to the address at this offset.
                la    s0, 600f      // s0 = _start_og_trap
                sw    s0, 1*4(sp)   // 1*4(sp) = s0

                // sw x0, 0*4(sp)   // Reserved as scratch space for trap handler

                // Now, copy the stacked arguments. For this we need to:
                //
                // 1. load the current foreign stack pointer,
                // 2. subtract the amount of bytes occupied by stacked arguments,
                // 3. align the new stack pointer downward to a 16-byte boundary,
                // 4. check whether the new stack pointer would overflow,
                // 5. copy `t3` bytes from our current stack pointer to the
                //    foreign stack.
                //
                // Load the foreign stack pointer (fsp) and the bottom of the
                // stack from our runtime:
                lw    s0, {rtas_foreign_stack_ptr_offset}(t0)
                lw    s1, {rtas_foreign_stack_bottom_offset}(t0)

                // Check if our subtraction would underflow:
                bltu  s0, t3, 200f  // If fsp < stack_spill, overflow!

                // Move the stack downward by `t3` (`stack_spill`) and align.
                // Aligning downward to the next 16-byte boundary cannot
                // possibly underflow:
                sub   s0, s0, t3    // fsp -= stack_spill
                andi  s0, s0, -16   // fsp -= fsp % 16

                // Just because the above operation did not wrap around does not
                // mean that we did not overflow our stack. Check that we're not
                // lower than stack_bottom:
                bgeu  s0, s1, 300f  // If fsp >= stack_bottom, no overflow!
                unimp

              200: // _spill_stack_overflow
                unimp               // TODO: error handling!

              300: // _no_spill_stack_overflow
                // The foreign stack is now properly aligned. Copy stack_spill
                // (t3) bytes from our original stack pointer (t5) to fsp (s0).
                //
                // We decrement both the original sp and fsp one word at a time.
                // While we can clobber `t5`, we need to retain fsp (`s0`).
                // Instead, use a copy in `s1`:
                mv    s1, s0        // fsp' = fsp

                // To make sure we don't overshoot our word-copy loop, we round
                // up the stack spill to a multiple of 4 bytes (word size).
                // While this really should always be word-aligned, we're better
                // safe than sorry. In the worst case, we'll copy an extra
                // <= 3 bytes.
                addi  t3, t3, 3
                andi  t3, t3, -8

              400: // _stack_copy
                // Copy the stack, implemented as a `while (cond) copy; loop`
                beq   t3, x0, 500f  // If to copy == 0, jump to _stack_copied
                lw    s2, 0(t5)     // Load a word from our original stack
                sw    s2, 0(s1)     // Store it onto fsp'
                addi  t5, t5, 4     // original sp += 4 (one word)
                addi  s1, s1, 4     // fsp'        += 4 (one word)
                addi  t3, t3, -4    // to copy     -= 4 (one word)
                j     400b          // loop!

              500: // _stack_copied
                // From here on we can't allow the CPU to take interrupts
                // anymore, as we re-route traps to `_start_og_trap` below (by
                // writing our stack pointer into the mscratch CSR), and we rely
                // on certain CSRs to not be modified or used in their
                // intermediate states (e.g., mepc).
                //
                // We atomically switch to user-mode and re-enable interrupts
                // using the `mret` instruction below.
                //
                // If interrupts are disabled _after_ setting mscratch, this
                // result in the race condition of
                // [PR 2308](https://github.com/tock/tock/pull/2308)

                // Therefore, clear the following bits in mstatus first:
                //   0x00000008 -> bit 3 -> MIE (disabling interrupts here)
                // + 0x00001800 -> bits 11,12 -> MPP (switch to usermode on mret)
                li    s1, 0x00001808
                csrc  mstatus, s1         // clear bits in mstatus

                // Afterwards, set the following bits in mstatus:
                //   0x00000080 -> bit 7 -> MPIE (enable interrupts on mret)
                li    s1, 0x00000080
                csrs  mstatus, s1         // set bits in mstatus

                // Execute `_start_og_trap` on a trap by setting the mscratch
                // trap handler address to our current stack pointer. This stack
                // pointer, at `1*4(sp)`, holds the address of `_start_og_trap`.
                //
                // Upon a trap, the global trap handler (_start_trap) will swap
                // `s0` with the `mscratch` CSR and, if it contains a non-zero
                // address, jump to the address that is now at `1*4(s0)`. This
                // allows us to hook a custom trap handler that saves all
                // userspace state:
                //
                csrw  mscratch, sp        // Store `sp` in mscratch CSR. Discard
                                          // the prior value (was zero)

                // We have to set the mepc CSR with the PC we want the app to
                // start executing at. This has been loaded into register `t1`
                // by the `invoke` wrapper for us:
                csrw  mepc, t1            // Set mepc to the function to run.

                // Switch to the application's stack pointer, which is aligned
                // to a 16-byte boundary (as required by the RISC-V C ABI) and
                // has all spilled arugments copied onto:
                mv    sp, s0

                // All other argument registers have not been clobbered so far.
                // Set our return address to the return springboard:
                la    ra, {ret_springboard_sym}

                // Clear all Rust state that the function should not have access
                // to. This is not strictly necessary under all threat models,
                // but it's a good way to test that we're actually restoring all
                // of them:
                //mv    x3, x0        // gp
                //mv    x4, x0        // tp
                //mv    x5, x0        // t0
                //mv    x6, x0        // t1
                //mv    x7, x0        // t2
                //mv    x8, x0        // s0 / fp
                //mv    x9, x0        // s1
                //mv   x18, x0        // s2
                //mv   x19, x0        // s3
                //mv   x20, x0        // s4
                //mv   x21, x0        // s5
                //mv   x22, x0        // s6
                //mv   x23, x0        // s7
                //mv   x24, x0        // s8
                //mv   x25, x0        // s9
                //mv   x26, x0        // s10
                //mv   x27, x0        // s11
                //mv   x28, x0        // t3
                //mv   x29, x0        // t4
                //mv   x30, x0        // t5
                //mv   x31, x0        // t6

                // Execute the foreign function, re-enabling interrupts.
                mret

                // The global trap handler will jump to this address when
                // catching a trap while the foreign function is executing
                // (address loaded into the mscratch CSR).
                //
                // This custom trap handler is responsible for saving
                // application state, clearing the custom trap handler
                // (mscratch = 0), and restoring the kernel context.

              600: // _start_og_trap
                // At this point all we know is that we entered the trap handler
                // from an app. We don't know _why_ we got a trap, it could be
                // from an interrupt, syscall, or fault (or maybe something
                // else). Therefore we have to be very careful not to overwrite
                // any registers before we have saved them.
                //
                // The global trap handler has swapped the functions's `s0` into
                // the mscratch CSR, which now contains the address of our stack
                // pointer. The global trap handler further clobbered `s1`,
                // which now contains the address of `_start_og_trap`. The
                // function's `s1` is saved at `0*4(s0)`.
                //
                // Thus we can clobber `s1` to inspect the source of our trap /
                // interrupt, and branch accordingly.
                csrr  s1, mcause

                // If mcause is greater than or equal to zero this was not an
                // interrupt (i.e. the most significant bit is not 1). In this
                // case, jump to _start_og_trap_continue.
                bge   s1, x0, 700f

                // This was an interrupt! We save all callee-saved registers
                // and call the function to disable interrupts. We then proceed
                // executing the application.

                // First, save the foreign stack pointer onto our stack:
                sw    x2, 35*4(s0)

                // Now, we can restore the Rust stack pointer, and get an
                // additional register to clobber (s0):
                mv    sp, s0

                // We reset mscratch to 0 (kernel trap handler mode), to ensure
                // that all faults in the interrupt disable function are handled
                // as kernel faults. This also restores the function's s0.
                csrrw s0, mscratch, zero

                // Now, continue to save the remaining registers. Using `sp`
                // will allow all of these to be compressed instructions:
                sw    x4, 37*4(sp) // tp
                sw    x3, 36*4(sp) // gp
                // sw x2, 35*4(sp) // sp, saved above
                sw   x31, 34*4(sp) // t6
                sw   x30, 33*4(sp) // t5
                sw   x29, 32*4(sp) // t4
                sw   x28, 31*4(sp) // t3
                sw   x17, 30*4(sp) // a7
                sw   x16, 29*4(sp) // a6
                sw   x15, 28*4(sp) // a5
                sw   x14, 27*4(sp) // a4
                sw   x13, 26*4(sp) // a3
                sw   x12, 25*4(sp) // a2
                sw   x11, 24*4(sp) // a1
                sw   x10, 23*4(sp) // a0
                sw    x7, 22*4(sp) // t2
                sw    x6, 21*4(sp) // t1
                sw    x5, 20*4(sp) // t0
                sw    x1, 19*4(sp) // ra

                // Restore some important context for Rust, namely the thread-
                // and global-pointers:
                lw    x4,  6*4(sp) // tp
                lw    x3,  5*4(sp) // gp

                // Disable the interrupt. This requires `mcause` (currently in
                // `s1`) to be loaded into `a0`:
                // Before running any Rust code in a trap handler context,
                // set this hart's `_trap_handler_active` flag (see the
                // `_start_trap` documentation in the riscv arch crate). Only
                // caller-saved temporaries not holding foreign state are
                // clobbered here.
                la    t0, _trap_handler_active
                csrr  t1, mhartid
                slli  t1, t1, 2
                add   t0, t0, t1
                li    t1, 1
                sw    t1, 0(t0)

                // The chip records and masks the interrupt, as the
                // foreign code resumes below (unlike a process, which a
                // trap always leaves):
                mv   a0, s1     // a0 = s1 (mcause)
                jal  ra, _disable_interrupt_trap_rust_from_foreign

                // Reset this hart's `_trap_handler_active` flag:
                la    t0, _trap_handler_active
                csrr  t1, mhartid
                slli  t1, t1, 2
                add   t0, t0, t1
                sw    x0, 0(t0)

                // Restore registers from the stack:
                lw    x1, 19*4(sp) // ra
                lw    x5, 20*4(sp) // t0
                lw    x6, 21*4(sp) // t1
                lw    x7, 22*4(sp) // t2
                lw   x10, 23*4(sp) // a0
                lw   x11, 24*4(sp) // a1
                lw   x12, 25*4(sp) // a2
                lw   x13, 26*4(sp) // a3
                lw   x14, 27*4(sp) // a4
                lw   x15, 28*4(sp) // a5
                lw   x16, 29*4(sp) // a6
                lw   x17, 30*4(sp) // a7
                lw   x28, 31*4(sp) // t3
                lw   x29, 32*4(sp) // t4
                lw   x30, 33*4(sp) // t5
                lw   x31, 34*4(sp) // t6
                // lw x2, 35*4(sp) // sp, load last as we overwrite our pointer
                lw    x3, 36*4(sp) // gp
                lw    x4, 37*4(sp) // tp

                // Reset the trap handler by switching our kernel stack into
                // `mscratch` again. We discard its current value, which must
                // be zero (kernel trap handler mode).
                csrw  mscratch, sp

                // Restore the function's s1, as it was clobbered by the trap
                // handler:
                lw    x9, 0*4(sp)

                // Finally, load back the functions's stack pointer,
                // the last register:
                lw    x2, 35*4(sp) // sp

                // Return to the function:
                mret

              700: // _start_og_trap_continue
                // Load and store access faults (mcause 5 and 7) may be
                // emulated. Test `s1` (mcause) without a scratch register:
                addi  s1, s1, -5
                beqz  s1, 900f
                addi  s1, s1, -2
                beqz  s1, 900f
                addi  s1, s1, 7

                // This was not an interrupt, it may be a return/fault (handled
                // identically), or a callback.
                //
                // For fault / return we need to extract all required
                // information, restore the kernel trap handler, kernel state,
                // and then hand off to a final Rust function that encodes the
                // return value into the provided
                // `&mut TockRv32iCRtInvokeResInner`.
                //
                // Handling a callback is simpler. In that case, we can simply
                // invoke a C-ABI callback handler and assume that the app
                // has already saved all required registers. We thus call this
                // function here, in the context of the trap handler.
                //
                // To do so, we need to restore a couple of registers and
                // populate a CallbackAsmContext struct that we pass to the
                // handler. We already have space for this on the Rust
                // stack.

                // First, save the foreign stack pointer onto our stack and
                // create a copy in t0 for code below:
                sw    x2, 35*4(s0)
                mv    t0, x2

                // Now, we can restore the Rust stack pointer:
                mv    sp, s0

                // We reset mscratch to 0 (kernel trap handler mode), to ensure
                // that all faults in the callback handler function are handled
                // as kernel faults. This also restores the function's s0.
                csrrw s0, mscratch, zero

                // Now, continue to save the remaining registers. Using `sp`
                // will allow all of these to be compressed instructions:
                sw   x11, 24*4(sp) // a1
                sw   x10, 23*4(sp) // a0
                sw    x4, 37*4(sp) // tp
                sw    x3, 36*4(sp) // gp
                sw    x1, 19*4(sp) // ra

                // Restore some important context for Rust, namely the thread-
                // and global-pointers:
                lw    x4,  6*4(sp) // tp
                lw    x3,  5*4(sp) // gp

                // Push a callback context struct onto the stack, place
                // a pointer to it at the new stack offset, and populate it:
                // lw t0, ({callback_ctx_ptr_size} + 35*4)(sp) // foreign stack pointer, t0
                addi  sp, sp, -{callback_ctx_ptr_size}
                sw    t0, ({callback_ctx_foreign_stack_ptr_offset} + 4)(sp)
                lw    t0, ({callback_ctx_ptr_size} + 3*4)(sp)
                sw    t0, ({callback_ctx_runtime_offset} + 4)(sp)

                addi  t0, sp, 4
                sw    t0, 0*4(sp)

                // Try to handle this as a callback. We leave a0 -- a7 as
                // populated by foreign code. This function will pick up
                // the CallbackAsmContext struct located on the stack, and
                // pointed to by the current stack pointer (first non-register
                // argument).
                // Before running any Rust code in a trap handler context,
                // set this hart's `_trap_handler_active` flag (see the
                // `_start_trap` documentation in the riscv arch crate). Only
                // caller-saved temporaries not holding foreign state are
                // clobbered here.
                la    t0, _trap_handler_active
                csrr  t1, mhartid
                slli  t1, t1, 2
                add   t0, t0, t1
                li    t1, 1
                sw    t1, 0(t0)

                jal   ra, {callback_handler}

                // Reset this hart's `_trap_handler_active` flag:
                la    t0, _trap_handler_active
                csrr  t1, mhartid
                slli  t1, t1, 2
                add   t0, t0, t1
                sw    x0, 0(t0)

                // Check if this callback was successfully handled.
                beqz  a0, 800f     // not a callback, return.

                // Return from the callback. For this, we must restore the saved registers
                // above and load the return value registers prepared by the callback handler.
                //
                // Load the return values into a0 and a1:
                lw    a0, ({callback_ctx_ret_a0_offset} + 4)(sp)
                lw    a1, ({callback_ctx_ret_a1_offset} + 4)(sp)

                // Pop the CallbackAsmContext stack frame:
                addi  sp, sp, {callback_ctx_ptr_size}

                // Restore the other saved foreign function registers:
                lw    x4, 37*4(sp) // foreign tp
                lw    x3, 36*4(sp) // foreign gp
                lw    x1, 19*4(sp) // foreign ra

                // Load the app's return address register (in ra / x1) into mepc:
                csrw  mepc, x1     // foreign ra, return to requested return address

                // Reset the trap handler by switching our kernel stack into
                // `mscratch` again. We discard its current value, which must
                // be zero (kernel trap handler mode).
                csrw  mscratch, sp

                // Restore the function's s1, as it was clobbered by the trap
                // handler:
                lw    x9, 0*4(sp)

                // Finally, load back the functions's stack pointer,
                // the last register:
                lw    x2, 35*4(sp) // sp

                // Return to the function:
                mret

              800: // _return_from_og
                // This was not a valid callback. We need to return to the
                // surrounding Rust code, and return from the trap handler.
                //
                // Instead of returning through a branch, jump, or jump and
                // link (which would be wrong, since we require a tail-call),
                // we use the return from the trap handler to implement this
                // call. This also guarantees atomicity of the CSRs we read,
                // as traps are non-reentrant.
                //
                // We already restored the kernel trap handler above.
                //
                // Pop the CallbackAsmContext stack frame:
                addi  sp, sp, {callback_ctx_ptr_size}

              810: // _return_from_og_frame
                // Restore saved application registers:
                lw   x11, 24*4(sp) // a1 (return value)
                lw   x10, 23*4(sp) // a0 (return value)

                // Need to set mstatus.MPP to 0b11 so that we stay in machine
                // mode upon returning from this interrupt context.
                //
                // We use `a2` as a scratch register, as we are allowed to
                // clobber it here, and it fits into a compressed load
                // instruction.
                //
                li    a2, 0x1800    // Load 0b11 to the MPP bits location in a2
                csrs  mstatus, a2   // mstatus |= a2

                // In addition to the trap handler's clobbers (namely swapping
                // the function's s0 into mscratch, restoring our stack pointer
                // into s0, and saving the function's s1 onto 0*4(s0)), we also
                // have the mcause CSR loaded into `s1`. We move it into `a5`,
                // as expected by the function encoding the return register.
                mv    a5, s1        // a5 = s1 (mcause, as loaded above)

                // We want to keep the function's `sp` for diagnostics purposes,
                // and thus store it into `a4` (to be passed to the function
                // encoding the return value).
                lw    a4, 35*4(sp)  // a4 = foreign stack pointer

                // Load the remaining CSRs to be passed as an argument, `mtval`:
                csrr  a6, mtval     // a6 = mtval CSR

                // And `mepc`. For `mepc`, also load the address of the
                // return-value encoding function, to perform a tail-call by
                // returning from the interrupt context:
                la    a7, {encode_ret_sym}
                csrrw a7, mepc, a7

                // Pass the runtime and invoke res pointers as arguments:
                lw    a2,  3*4(sp)  // a2 (&TockRv32iCRtAsmState, was t0)
                lw    a3,  4*4(sp)  // a3 (&mut TockRv32iCRtInvokeResInner, was t2)

                // Restore the return address, for our tail-call:
                lw    ra,  2*4(sp)  // ra

                // Restore all other caller-saved kernel registers:
                // lw x3,  5*4(sp)  // gp, loaded above
                // lw x4,  6*4(sp)  // tp, loaded above
                lw    x8,  7*4(sp)  // s0 / fp
                lw    x9,  8*4(sp)  // s1
                lw   x18,  9*4(sp)  // s2
                lw   x19, 10*4(sp)  // s3
                lw   x20, 11*4(sp)  // s4
                lw   x21, 12*4(sp)  // s5
                lw   x22, 13*4(sp)  // s6
                lw   x23, 14*4(sp)  // s7
                lw   x24, 15*4(sp)  // s8
                lw   x25, 16*4(sp)  // s9
                lw   x26, 17*4(sp)  // s10
                lw   x27, 18*4(sp)  // s11

                // The two proper return registers (a0 and a1) say unmodified
                // and are passed through to the tail-call function:
                // mv a0, a0        // pass a0, nop
                // mv a0, a0        // pass a1, nop

                addi sp, sp, 40*4   // Reset kernel stack pointer

                // Return from the trap handler, re-entering machine mode and
                // continuing execution at the tail-called function to encode
                // the return value.
                mret

              900: // _emulate_access
                // A load or store faulted. All foreign registers are live,
                // except s0 (in mscratch) and s1 (at 0*4(s0)). Save all of
                // them into a frame below the kernel's, indexed by register
                // number, for the Rust handler to read and modify.
                sw    x2, 35*4(s0)  // foreign sp
                mv    sp, s0
                csrrw s0, mscratch, zero // s0 = foreign s0, kernel trap mode
                addi  sp, sp, -32*4
                sw    x1,   1*4(sp)
                sw    x3,   3*4(sp)
                sw    x4,   4*4(sp)
                sw    x5,   5*4(sp)
                sw    x6,   6*4(sp)
                sw    x7,   7*4(sp)
                sw    x8,   8*4(sp)
                sw    x10, 10*4(sp)
                sw    x11, 11*4(sp)
                sw    x12, 12*4(sp)
                sw    x13, 13*4(sp)
                sw    x14, 14*4(sp)
                sw    x15, 15*4(sp)
                sw    x16, 16*4(sp)
                sw    x17, 17*4(sp)
                sw    x18, 18*4(sp)
                sw    x19, 19*4(sp)
                sw    x20, 20*4(sp)
                sw    x21, 21*4(sp)
                sw    x22, 22*4(sp)
                sw    x23, 23*4(sp)
                sw    x24, 24*4(sp)
                sw    x25, 25*4(sp)
                sw    x26, 26*4(sp)
                sw    x27, 27*4(sp)
                sw    x28, 28*4(sp)
                sw    x29, 29*4(sp)
                sw    x30, 30*4(sp)
                sw    x31, 31*4(sp)
                lw    s1, (32+35)*4(sp) // foreign sp
                sw    s1,   2*4(sp)
                lw    s1, (32+0)*4(sp)  // foreign s1
                sw    s1,   9*4(sp)
                sw    x0,   0*4(sp)

                // Kernel gp and tp:
                lw    x3, (32+5)*4(sp)
                lw    x4, (32+6)*4(sp)

                la    t0, _trap_handler_active
                csrr  t1, mhartid
                slli  t1, t1, 2
                add   t0, t0, t1
                li    t1, 1
                sw    t1, 0(t0)

                // Returns the address to resume foreign code at, or 0 if
                // the access was refused:
                mv    a0, sp
                csrr  a1, mtval
                csrr  a2, mepc
                lw    a3, (32+3)*4(sp) // &TockRv32iCRtAsmState
                jal   ra, {emulate_access}

                la    t0, _trap_handler_active
                csrr  t1, mhartid
                slli  t1, t1, 2
                add   t0, t0, t1
                sw    x0, 0(t0)

                beqz  a0, 950f      // refused: end the call
                csrw  mepc, a0
                // Back to our trap handler for foreign code:
                addi  t0, sp, 32*4
                csrw  mscratch, t0

                lw    x1,   1*4(sp)
                lw    x3,   3*4(sp)
                lw    x4,   4*4(sp)
                lw    x5,   5*4(sp)
                lw    x6,   6*4(sp)
                lw    x7,   7*4(sp)
                lw    x8,   8*4(sp)
                lw    x9,   9*4(sp)
                lw    x10, 10*4(sp)
                lw    x11, 11*4(sp)
                lw    x12, 12*4(sp)
                lw    x13, 13*4(sp)
                lw    x14, 14*4(sp)
                lw    x15, 15*4(sp)
                lw    x16, 16*4(sp)
                lw    x17, 17*4(sp)
                lw    x18, 18*4(sp)
                lw    x19, 19*4(sp)
                lw    x20, 20*4(sp)
                lw    x21, 21*4(sp)
                lw    x22, 22*4(sp)
                lw    x23, 23*4(sp)
                lw    x24, 24*4(sp)
                lw    x25, 25*4(sp)
                lw    x26, 26*4(sp)
                lw    x27, 27*4(sp)
                lw    x28, 28*4(sp)
                lw    x29, 29*4(sp)
                lw    x30, 30*4(sp)
                lw    x31, 31*4(sp)
                lw    x2,   2*4(sp)
                mret

              950: // _emulation_refused
                // End the call as for a fault. mcause, mtval and mepc still
                // describe the access. Move the foreign a0 and a1 to where
                // _return_from_og expects them, drop the register frame, and
                // continue there with mcause in s1:
                lw    t0, 10*4(sp)
                sw    t0, (32+23)*4(sp)
                lw    t0, 11*4(sp)
                sw    t0, (32+24)*4(sp)
                addi  sp, sp, 32*4
                csrr  s1, mcause
                j     810b
            ",
            // Function & springboard symbols:
            ret_springboard_sym = sym og_tock_rv32i_c_rt_ret_springboard,
            encode_ret_sym = sym Self::encode_return,
            callback_handler = sym Self::callback_handler,
            emulate_access = sym emulate_access,
            // Runtime ASM state offsets:
            rtas_foreign_stack_ptr_offset = const core::mem::offset_of!(TockRv32iCRtAsmState, foreign_stack_ptr),
            rtas_foreign_stack_bottom_offset = const core::mem::offset_of!(TockRv32iCRtAsmState, foreign_stack_bottom),
            // Callback context + pointer stack frame size:
            callback_ctx_ptr_size = const CALLBACK_CONTEXT_PLUS_POINTER_STACKED_SIZE,
            callback_ctx_foreign_stack_ptr_offset = const core::mem::offset_of!(
                TockRv32iCRtCallbackAsmContext, foreign_stack_ptr),
            callback_ctx_runtime_offset = const core::mem::offset_of!(
                TockRv32iCRtCallbackAsmContext, runtime),
            callback_ctx_ret_a0_offset = const core::mem::offset_of!(
                TockRv32iCRtCallbackAsmContext, ret_a0),
            callback_ctx_ret_a1_offset = const core::mem::offset_of!(
                TockRv32iCRtCallbackAsmContext, ret_a1),
        );
    }

    extern "C" fn encode_return(
        a0: usize,
        a1: usize,
        a2_rt: &Self,
        a3_invoke_res: &mut TockRv32iCInvokeResInner,
        a4_fsp: *const (),
        a5_mcause: usize,
        a6_mtval: usize,
        a7_mepc: usize,
    ) {
        // Determine whether the function faulted, returned to the kernel using
        // a regular `ecall` instruction, or tried to return, or tried to issue
        // a callback.
        if a5_mcause == MCAUSE_ENV_CALL_UMODE
            || (a5_mcause == MCAUSE_INSTRUCTION_ACCESS_FAULT
                && a7_mepc == og_tock_rv32i_c_rt_ret_springboard as *const () as usize)
        {
            // Function returned "normally", so we encode that:
            a3_invoke_res.error = TockRv32iCInvokeErr::NoError;
            a3_invoke_res.a0 = a0;
            a3_invoke_res.a1 = a1;
            a3_invoke_res.sp = a4_fsp;
        } else {
            let state = &a2_rt.asm_state;
            state.fault.set(Some(ForeignFault {
                mcause: a5_mcause,
                mtval: a6_mtval,
                mepc: a7_mepc,
                sp: a4_fsp as usize,
                refused: state.refusal.take(),
            }));
            a3_invoke_res.error = TockRv32iCInvokeErr::Fault;
        }
    }

    fn setup_callback_int<'a, C, F, R>(
        &self,
        callback: &'a mut C,
        alloc_scope: &mut AllocScope<
            '_,
            <Self as OGRuntime>::AllocTracker<'_>,
            <Self as OGRuntime>::ID,
        >,
        fun: F,
    ) -> OGResult<R>
    where
        C: FnMut(
            &<Self as OGRuntime>::CallbackContext,
            &mut <Self as OGRuntime>::CallbackReturn,
            *mut (),
            *mut (),
        ),
        F: for<'b> FnOnce(
            *const <Self as OGRuntime>::CallbackTrampolineFn,
            &'b mut AllocScope<'_, <Self as OGRuntime>::AllocTracker<'_>, <Self as OGRuntime>::ID>,
        ) -> R,
    {
        struct Context<'a, ClosureTy> {
            closure: &'a mut ClosureTy,
        }

        /// ### Safety
        ///
        /// `ctx_ptr` must point to the `Context` of this callback's closure,
        /// and the scopes to those of this runtime.
        unsafe extern "C" fn callback_wrapper<
            'a,
            ClosureTy: FnMut(
                    &TockRv32iCRtCallbackContext,
                    &mut TockRv32iCRtCallbackReturn,
                    *mut (),
                    *mut (),
                ) + 'a,
        >(
            ctx_ptr: *mut c_void,
            callback_ctx: &TockRv32iCRtCallbackContext,
            callback_ret: &mut TockRv32iCRtCallbackReturn,
            alloc_scope: *mut (),
            access_scope: *mut (),
        ) {
            let ctx: &mut Context<'a, ClosureTy> =
                unsafe { &mut *(ctx_ptr as *mut Context<'a, ClosureTy>) };

            // For now, we assume that the functoin doesn't unwind:
            (ctx.closure)(callback_ctx, callback_ret, alloc_scope, access_scope)
        }

        // Ensure that the context pointer is compatible in size and
        // layout to a c_void pointer:
        assert_eq!(
            core::mem::size_of::<*mut c_void>(),
            core::mem::size_of::<*mut Context<'a, C>>()
        );
        assert_eq!(
            core::mem::align_of::<*mut c_void>(),
            core::mem::align_of::<*mut Context<'a, C>>()
        );

        let mut ctx: Context<'a, C> = Context { closure: callback };

        // TODO: does this need to be pinned?
        let mut inner_alloc_scope = unsafe {
            AllocScope::new(
                TockRv32iCRtAllocChain::CallbackDescriptor(
                    TockRv32iCCallbackDescriptor {
                        springboard: 0x00000000, // RISC-V unimp
                        wrapper: callback_wrapper::<C>,
                        context: &mut ctx as *mut _ as *mut c_void,
                        _lt: PhantomData::<&'a mut c_void>,
                    },
                    alloc_scope.tracker(),
                ),
                alloc_scope.id_imprint(),
            )
        };

        let springboard_ptr = unsafe {
            (inner_alloc_scope.tracker() as *const TockRv32iCRtAllocChain as *const ())
                .byte_offset(
                    core::mem::offset_of!(TockRv32iCRtAllocChain, CallbackDescriptor.0) as isize,
                )
                .byte_offset(
                    core::mem::offset_of!(TockRv32iCCallbackDescriptor, springboard) as isize,
                )
        };

        let res = fun(
            springboard_ptr as *const CallbackTrampolineFn,
            &mut inner_alloc_scope,
        );

        Ok(res)
    }
}

unsafe impl<ID: OGID, P: OGProtection + 'static> OGRuntime for TockRv32iCRt<ID, P> {
    type ID = ID;
    type AllocTracker<'a> = TockRv32iCRtAllocChain<'a>;
    type ABI = Rv32iCABI;
    type CallbackTrampolineFn = CallbackTrampolineFn;
    type CallbackContext = TockRv32iCRtCallbackContext;
    type CallbackReturn = TockRv32iCRtCallbackReturn;

    // We don't have any symbol table state, as the Tock OG binary
    // already contains a symbol table that we can use.
    type SymbolTableState<const SYMTAB_SIZE: usize, const FIXED_OFFSET_SYMTAB_SIZE: usize> = ();

    fn resolve_symbols<const SYMTAB_SIZE: usize, const FIXED_OFFSET_SYMTAB_SIZE: usize>(
        &self,
        _symbol_table: &'static [&'static CStr; SYMTAB_SIZE],
        fixed_offset_symbol_table: &'static [Option<&'static CStr>; FIXED_OFFSET_SYMTAB_SIZE],
    ) -> Option<Self::SymbolTableState<SYMTAB_SIZE, FIXED_OFFSET_SYMTAB_SIZE>> {
        // Check whether the binary's symbol table is large enough to contain
        // all symbols that could possbily be referenced by the fixed offset
        // symbol table (i.e., binary symtab size >= FIXED_OFFSET_SYMTAB_SIZE),
        // and that all symbols are contained in the binary (TODO!).
        if fixed_offset_symbol_table.len() > self.fntab_length {
            None
        } else {
            Some(())
        }
    }

    fn lookup_symbol<const SYMTAB_SIZE: usize, const FIXED_OFFSET_SYMTAB_SIZE: usize>(
        &self,
        _compact_symtab_index: usize,
        fixed_offset_symtab_index: usize,
        _symtabstate: &Self::SymbolTableState<SYMTAB_SIZE, FIXED_OFFSET_SYMTAB_SIZE>,
    ) -> Option<*const ()> {
        if fixed_offset_symtab_index < self.fntab_length {
            Some(unsafe { *self.fntab_addr.add(fixed_offset_symtab_index) })
        } else {
            None
        }
    }

    fn setup_callback<'a, C, F, R>(
        &self,
        callback: &'a mut C,
        alloc_scope: &mut AllocScope<'_, Self::AllocTracker<'_>, Self::ID>,
        fun: F,
    ) -> OGResult<R>
    where
        C: FnMut(
            &Self::CallbackContext,
            &mut Self::CallbackReturn,
            &mut AllocScope<'_, Self::AllocTracker<'_>, Self::ID>,
            &mut AccessScope<Self::ID>,
        ),
        F: for<'b> FnOnce(
            *const Self::CallbackTrampolineFn,
            &'b mut AllocScope<'_, Self::AllocTracker<'_>, Self::ID>,
        ) -> R,
    {
        let typecast_callback =
            &mut |callback_ctx: &TockRv32iCRtCallbackContext,
                  callback_ret: &mut TockRv32iCRtCallbackReturn,
                  alloc_scope_ptr: *mut (),
                  access_scope_ptr: *mut ()| {
                let alloc_scope = unsafe {
                    &mut *(alloc_scope_ptr as *mut AllocScope<'_, Self::AllocTracker<'_>, Self::ID>)
                };

                let access_scope =
                    unsafe { &mut *(access_scope_ptr as *mut AccessScope<Self::ID>) };

                callback(callback_ctx, callback_ret, alloc_scope, access_scope);
            };

        // We need to erase the type-dependence of the closure argument on `ID`,
        // as that creates life-time issues when the `MockRtAllocChain` is
        // parameterized over it:
        self.setup_callback_int(typecast_callback, alloc_scope, fun)
    }

    fn execute<R, F: FnOnce() -> R>(
        &self,
        _target_symbol: *const (),
        alloc_scope: &mut AllocScope<'_, Self::AllocTracker<'_>, Self::ID>,
        _access_scope: &mut AccessScope<Self::ID>,
        f: F,
    ) -> R {
        // Store a reference to the alloc_scope in the Runtime struct,
        // such that it can be retrieved later in a callback. After the function
        // finished executing, restore the old value:
        let prev_active_alloc_scope = self.asm_state.active_alloc_scope.get();
        self.asm_state
            .active_alloc_scope
            .set(alloc_scope as *mut _ as *mut ());
        // panic!("Other context: {:?}", self.asm_state.active_alloc_scope.get());

        let res = self.execute_protected(f);

        // Restore the previous alloc scope:
        self.asm_state
            .active_alloc_scope
            .set(prev_active_alloc_scope);

        res
    }

    // We provide only the required implementations and rely on default
    // implementations for all "convenience" allocation methods. These are as
    // efficient as it gets in our case anyways.
    fn allocate_stacked_untracked_mut<F, R>(
        &self,
        requested_layout: core::alloc::Layout,
        fun: F,
    ) -> OGResult<R>
    where
        F: FnOnce(*mut ()) -> R,
    {
        let mut fsp = self.asm_state.foreign_stack_ptr.get() as usize;
        let original_fsp = fsp;

        // Move the stack pointer downward by the requested size. We always use
        // saturating_sub() to avoid underflows:
        fsp = fsp.saturating_sub(requested_layout.size());

        // Now, adjust the foreign stack pointer downward to the required
        // alignment. The saturating_sub should be optimized away here:
        fsp = fsp.saturating_sub(original_fsp % requested_layout.align());

        // Check that we did not produce a stack overflow. If that happened, we
        // must return before saving this stack pointer, or writing to the
        // pointer.
        if fsp < self.asm_state.foreign_stack_bottom as usize {
            return Err(OGError::AllocNoMem);
        }

        // Save the new stack pointer:
        self.asm_state.foreign_stack_ptr.set(fsp as *mut ());

        // Call the closure with our pointer:
        let res = fun(fsp as *mut ());

        // Finally, restore the previous stack pointer:
        self.asm_state
            .foreign_stack_ptr
            .set(original_fsp as *mut ());

        // Fin:
        Ok(res)
    }

    fn allocate_stacked_mut<F, R>(
        &self,
        layout: core::alloc::Layout,
        alloc_scope: &mut AllocScope<'_, Self::AllocTracker<'_>, ID>,
        fun: F,
    ) -> OGResult<R>
    where
        F: for<'b> FnOnce(*mut (), &'b mut AllocScope<'_, Self::AllocTracker<'_>, Self::ID>) -> R,
    {
        self.allocate_stacked_untracked_mut(layout, move |ptr| {
            // We don't need to track this allocation separately. However, for
            // eval to be sensible and reflect the case where we actually move
            // allocations, place a Cons element on the alloc chain:
            let mut inner_alloc_scope: AllocScope<'_, TockRv32iCRtAllocChain<'_>, ID> = unsafe {
                AllocScope::new(
                    TockRv32iCRtAllocChain::Cons(alloc_scope.tracker()),
                    alloc_scope.id_imprint(),
                )
            };

            // The inner alloc scope will be popped from the stack once we leave
            // this closure:
            fun(ptr, &mut inner_alloc_scope)
        })
    }
}

macro_rules! invoke_impl_rtloc_register {
    ($regtype:ident, $rtloc:expr, $fnptrloc:expr, $resptrloc:expr, $marker:expr) => {
        impl<ID: OGID, P: OGProtection + 'static> Rv32iCRt<0, $regtype<Rv32iCABI>> for TockRv32iCRt<ID, P> {
            #[unsafe(naked)]
            unsafe extern "C" fn invoke() {
                core::arch::naked_asm!(
                    concat!("
                    // Load required parameters in non-argument registers and
                    // continue execution in the generic protection-domain
                    // switch routine:
                    mv   t0, ", $rtloc, "       // Load runtime pointer
                    mv   t1, ", $fnptrloc, "    // Load function pointer
                    mv   t2, ", $resptrloc, "   // Load the InvokeRes pointer
                    li   t3, 0                  // Load the stack-spill immediate
                    li   t5, ", $marker, "      // Load a marker indicating the source of this call
                    la   t4, {invoke_sym}       // Load the generic_invoke function
                    jr   t4                     // Tail-call into the invoke fn
                    "),
                    invoke_sym = sym Self::generic_invoke,
               );
            }
        }
    };
}

invoke_impl_rtloc_register!(AREG0, "a0", "a1", "a2", "0");
invoke_impl_rtloc_register!(AREG1, "a1", "a2", "a3", "1");
invoke_impl_rtloc_register!(AREG2, "a2", "a3", "a4", "2");
invoke_impl_rtloc_register!(AREG3, "a3", "a4", "a5", "3");
invoke_impl_rtloc_register!(AREG4, "a4", "a5", "a6", "4");
invoke_impl_rtloc_register!(AREG5, "a5", "a6", "a7", "5");

impl<ID: OGID, P: OGProtection + 'static> Rv32iCRt<0, AREG6<Rv32iCABI>> for TockRv32iCRt<ID, P> {
    #[unsafe(naked)]
    unsafe extern "C" fn invoke() {
        core::arch::naked_asm!(
             concat!("
            // Load required parameters in non-argument registers and
            // continue execution in the generic protection-domain
            // switch routine:
            mv   t0, a6                 // Load runtime pointer
            mv   t1, a7                 // Load function pointer
            lw   t2, 0*4(sp)            // Load the InvokeRes pointer
            li   t3, 0                  // Load the stack-spill immediate
            li   t5, 6                  // Load a marker indicating the source of this call
            la   t4, {invoke_sym}       // Load the generic_invoke function
            jr   t4                     // Tail-call into the invoke fn
            "),
             invoke_sym = sym Self::generic_invoke,
        );
    }
}

impl<ID: OGID, P: OGProtection + 'static> Rv32iCRt<0, AREG7<Rv32iCABI>> for TockRv32iCRt<ID, P> {
    #[unsafe(naked)]
    unsafe extern "C" fn invoke() {
        core::arch::naked_asm!(
             concat!("
            // Load required parameters in non-argument registers and
            // continue execution in the generic protection-domain
            // switch routine:
            mv   t0, a7                 // Load runtime pointer
            lw   t1, 0*4(sp)            // Load function pointer
            lw   t2, 1*4(sp)            // Load the InvokeRes pointer
            li   t3, 0                  // Load the stack-spill immediate
            li   t5, 7                  // Load a marker indicating the source of this call
            la   t4, {invoke_sym}       // Load the generic_invoke function
            jr   t4                     // Tail-call into the invoke fn
            "),
             invoke_sym = sym Self::generic_invoke,
        );
    }
}

impl<const STACK_SPILL: usize, const RT_STACK_OFFSET: usize, ID: OGID, P: OGProtection + 'static>
    Rv32iCRt<STACK_SPILL, Stacked<RT_STACK_OFFSET, Rv32iCABI>> for TockRv32iCRt<ID, P>
{
    #[unsafe(naked)]
    unsafe extern "C" fn invoke() {
        core::arch::naked_asm!(
            "
            // Load required parameters in non-argument registers and
            // continue execution in the generic protection-domain
            // switch routine:
            lw   t0, ({rt_off} + 0)(sp) // Load runtime pointer
            lw   t1, ({rt_off} + 4)(sp) // Load function pointer
            lw   t2, ({rt_off} + 8)(sp) // Load the InvokeRes pointer
            li   t3, {stack_spill}      // Copy the stack-spill immediate
            li   t5, 8                  // Load a marker indicating the source of this call
            la   t4, {invoke_sym}       // Load the generic_invoke function
            jr   t4                     // Tail-call into the invoke fn
            ",
            stack_spill = const STACK_SPILL,
            rt_off = const RT_STACK_OFFSET,
            invoke_sym = sym Self::generic_invoke,
        );
    }
}

impl<ID: OGID, P: OGProtection + 'static> Rv32iCBaseRt for TockRv32iCRt<ID, P> {
    type InvokeRes<T> = TockRv32iCInvokeRes<Self, T>;
}

/// Emulate the load or store at `mepc` that faulted at `mtval`, on the
/// foreign registers in `regs` (indexed by register number). Returns the
/// address of the next instruction, or 0 to end the call with a fault if the
/// access cannot or may not be emulated.
extern "C" fn emulate_access(
    regs: &mut [usize; 32],
    mtval: usize,
    mepc: usize,
    rt: &TockRv32iCRtAsmState,
) -> usize {
    match emulate_access_int(regs, mtval, mepc, rt) {
        Ok(next) => next,
        Err(why) => {
            rt.refusal.set(Some(why));
            0
        }
    }
}

fn emulate_access_int(
    regs: &mut [usize; 32],
    mtval: usize,
    mepc: usize,
    rt: &TockRv32iCRtAsmState,
) -> Result<usize, &'static str> {
    let Some(emulation) = rt.access_emulation.get() else {
        return Err("no emulation");
    };
    // ### Safety
    //
    // Foreign code just executed the instruction at `pc`, and the board
    // says where the kernel can read it.
    let halfword =
        |pc: usize| unsafe { core::ptr::read_volatile(emulation.code_address(pc) as *const u16) };

    // Decode: (is_store, funct3 (width, signedness), data register, base
    // register, offset, instruction length).
    let lo = halfword(mepc) as u32;
    let (store, funct3, rd, rs1, imm, len) = if lo & 3 == 3 {
        let insn = lo | (halfword(mepc + 2) as u32) << 16;
        let funct3 = (insn >> 12) & 7;
        let rs1 = (insn >> 15) & 31;
        match insn & 0x7f {
            0x03 => (false, funct3, (insn >> 7) & 31, rs1, (insn as i32) >> 20, 4),
            0x23 => (
                true,
                funct3,
                (insn >> 20) & 31,
                rs1,
                (((insn as i32) >> 25) << 5) | ((insn >> 7) & 31) as i32,
                4,
            ),
            _ => return Err("not a load or store"),
        }
    } else {
        // C.LW / C.SW: rd'/rs2' in [4:2], rs1' in [9:7],
        // offset[5:3] in [12:10], [2] in [6], [6] in [5].
        let imm = (((lo >> 6) & 1) << 2 | ((lo >> 10) & 7) << 3 | ((lo >> 5) & 1) << 6) as i32;
        let reg = ((lo >> 2) & 7) + 8;
        let rs1 = ((lo >> 7) & 7) + 8;
        match (lo & 3, lo >> 13) {
            (0, 0b010) => (false, 0b010, reg, rs1, imm, 2),
            (0, 0b110) => (true, 0b010, reg, rs1, imm, 2),
            _ => return Err("not a load or store"),
        }
    };

    let addr = regs[rs1 as usize].wrapping_add(imm as usize);
    if addr != mtval {
        return Err("address mismatch");
    }
    let width = 1usize << (funct3 & 3);
    let value = store.then(|| regs[rd as usize]);
    if width > 4 || !addr.is_multiple_of(width) || !emulation.allow(addr, width, value) {
        return Err("access not allowed");
    }

    // ### Safety
    //
    // The board allowed this access, on its behalf.
    unsafe {
        if store {
            let v = regs[rd as usize];
            match width {
                1 => core::ptr::write_volatile(addr as *mut u8, v as u8),
                2 => core::ptr::write_volatile(addr as *mut u16, v as u16),
                _ => core::ptr::write_volatile(addr as *mut u32, v as u32),
            }
        } else {
            let v = match funct3 {
                0b000 => core::ptr::read_volatile(addr as *const i8) as i32 as usize,
                0b001 => core::ptr::read_volatile(addr as *const i16) as i32 as usize,
                0b100 => core::ptr::read_volatile(addr as *const u8) as usize,
                0b101 => core::ptr::read_volatile(addr as *const u16) as usize,
                _ => core::ptr::read_volatile(addr as *const u32) as usize,
            };
            if rd != 0 {
                regs[rd as usize] = v;
            }
        }
    }
    Ok(mepc + len)
}

extern "C" {
    fn og_tock_rv32i_c_rt_ret_springboard();
}

#[cfg(any(target_arch = "riscv32", target_arch = "riscv64"))]
core::arch::global_asm!(
    "
      .global og_tock_rv32i_c_rt_ret_springboard
      og_tock_rv32i_c_rt_ret_springboard:
        // Return to machine-mode with an environment call or an instruction
        // access fault from a well-known address:
        ecall
    "
);
