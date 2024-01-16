use core::cell::Cell;
use core::marker::PhantomData;

use kernel::platform::mpu::{self, MPU};

use crate::abi::{rv32i::Riscv32iCABI, EncapfnABI};
use crate::binary::EncapfnBinary;
use crate::branding::EFID;
use crate::types::{AccessScope, AllocScope, AllocTracker, EFAllocation};
use crate::{EFError, EncapfnRt};

pub const ENCAPFN_HEADER_MAGIC_OFFSET: usize = 0;
pub const ENCAPFN_HEADER_RTHDR_PTR_OFFSET: usize = 4;
pub const ENCAPFN_HEADER_INIT_PTR_OFFSET: usize = 8;
pub const ENCAPFN_HEADER_FNTAB_PTR_OFFSET: usize = 12;
pub const ENCAPFN_HEADER_LEN: usize = 16;
pub const ENCAPFN_HEADER_MAGIC: u32 = 0x43535643;

pub struct EncapfnTockRv32iCRt<'m, ID: EFID, M: MPU> {
    binary: EncapfnBinary,
    ram_region_start: *mut u8,
    _ram_region_len: usize,
    rthdr_offset: usize,
    init_offset: usize,
    fntab_offset: usize,

    _stack_top: *mut u8,
    stack_ptr: Cell<*mut u8>,

    mpu: &'m M,

    /// Configuration data for the MPU
    mpu_config: M::MpuConfig,

    _id: PhantomData<ID>,
}

#[derive(Clone, Copy)]
pub struct EncapfnTockRv32iCRtAllocTracker {
    ram_region_start: *mut u8,
    ram_region_len: usize,
}

impl AllocTracker for EncapfnTockRv32iCRtAllocTracker {}

impl<'m, ID: EFID, M: MPU> EncapfnTockRv32iCRt<'m, ID, M> {
    /// Create a new containerized service instance from a binary loaded into an
    /// accessible memory region.
    ///
    /// This method assumes that the kernel has (at least read) access to the
    /// full binary. Callers need to ensure that any memory protection systems
    /// are set up accordingly.
    ///
    // TODO: switch to raw slices
    //
    // TODO: make the loading process work with PIC binaries
    pub unsafe fn new(
        mpu: &'m M,
        binary: EncapfnBinary,
        ram_region_start: *mut u8,
        ram_region_len: usize,
        _id: ID,
    ) -> Result<
        (
            Self,
            AllocScope<EncapfnTockRv32iCRtAllocTracker, ID>,
            AccessScope<ID>,
        ),
        (),
    > {
        // Each containerized service must start with a header indicating
        // relevant data to the loader (Tock). Check that the binary can at
        // least fit the header, ensure that the magic bytes match, and perform
        // other sanity checks.
        //
        // Containerized service binary header layout:
        //
        // 0             2             4             6             8
        // +---------------------------+---------------------------+
        // | 0x43535643 (CSVC) MAGIC   | Runtime Header Offset     |
        // +---------------------------+---------------------------+
        // | Function Table Offset     |
        // +---------------------------+
        //
        // We will try to load these sections into the provided RAM region, with
        // a layout as follows:
        //
        // +---------------------------+ <- `ram_region_start`
        // | Loader-initialized data   | -\
        // | - (optional) padding      |  |
        // | - .data                   |  |
        // | - .bss                    |  |
        // +---------------------------+  |
        // | Rust "remote memory"  |   |  |
        // | stack allocator       |   |  |
        // |                       v   |  |- R/W permissions for
        // +---------------------------+  |  containerized service
        // | Return trampoline stack   |  |
        // | frame                     |  |
        // +---------------------------+  |
        // | Containerized         |   |  |
        // | service stack         |   |  |
        // |                       v   | -/
        // +---------------------------+ <- `ram_region_start` + `ram_region_len`
        //
        // The entire containerized service binary will further be made
        // available with read-execute permissions

        // Make sure we have at least enough data to parse the header:
        if binary.binary_length < ENCAPFN_HEADER_LEN {
            return Err(());
        }

        // We generally try to avoid retaining Rust slices to the containerized
        // service binary (to avoid unsoundness, in case this memory should
        // change). However, for parsing the header, we can create an ephemeral
        // slice given that we verified the length:
        let header_slice = core::slice::from_raw_parts(binary.binary_start, ENCAPFN_HEADER_LEN);

        #[inline(always)]
        fn extract_header_word(header_slice: &[u8], offset: usize) -> u32 {
            let word_slice = &header_slice[offset..offset + core::mem::size_of::<u32>()];
            u32::from_ne_bytes([word_slice[0], word_slice[1], word_slice[2], word_slice[3]])
        }

        // Read the header fields in native endianness. First, check the magic:
        if extract_header_word(header_slice, ENCAPFN_HEADER_MAGIC_OFFSET) != ENCAPFN_HEADER_MAGIC {
            return Err(());
        }

        let rthdr_offset =
            extract_header_word(header_slice, ENCAPFN_HEADER_RTHDR_PTR_OFFSET) as usize;
        if rthdr_offset > binary.binary_length - core::mem::size_of::<u32>() {
            return Err(());
        }

        let init_offset =
            extract_header_word(header_slice, ENCAPFN_HEADER_INIT_PTR_OFFSET) as usize;
        if init_offset > binary.binary_length - core::mem::size_of::<u32>() {
            return Err(());
        }

        let fntab_offset =
            extract_header_word(header_slice, ENCAPFN_HEADER_FNTAB_PTR_OFFSET) as usize;
        if fntab_offset > binary.binary_length - core::mem::size_of::<u32>() {
            return Err(());
        }

        let mut mpu_config = mpu.new_config().ok_or(())?;

        // Allocate a region for the binary (R/X) and a region for the RAM (R/W)
        // chip.mpu().allocate_region(0 as *const u8, 4, 4, mpu::Permissions::ReadOnly, &mut mpu_config).unwrap();
        mpu.allocate_region(
            binary.binary_start,
            binary.binary_length,
            binary.binary_length,
            mpu::Permissions::ReadExecuteOnly,
            &mut mpu_config,
        )
        .unwrap();
        mpu.allocate_region(
            ram_region_start,
            ram_region_len,
            ram_region_len,
            mpu::Permissions::ReadWriteOnly,
            &mut mpu_config,
        )
        .unwrap();
        // chip.mpu()
        //     .allocate_region(
        //         0x40000000 as *const u8,
        //         0x10000000,
        //         0x10000000,
        //         mpu::Permissions::ReadWriteOnly,
        //         &mut mpu_config,
        //     )
        //     .unwrap();

        let stack_top = unsafe { ram_region_start.offset(ram_region_len as isize) };

        // All header fields parsed, we can construct the `EncapfnRt` struct and
        // try to initialize the service:
        let encapfn = EncapfnTockRv32iCRt {
            binary,
            ram_region_start,
            _ram_region_len: ram_region_len,
            rthdr_offset,
            init_offset,
            fntab_offset,

            _stack_top: stack_top,
            stack_ptr: Cell::new(stack_top),

            mpu,
            mpu_config,

            _id: PhantomData,
        };

        encapfn.init()?;

        Ok((
            encapfn,
            unsafe {
                AllocScope::new(EncapfnTockRv32iCRtAllocTracker {
                    ram_region_start,
                    ram_region_len,
                })
            },
            unsafe { AccessScope::new() },
        ))
    }

    fn init(&self) -> Result<(), ()> {
        // panic!("Init offset: {}", self.init_offset);
        let (a0, _) = self.invoke_service(
            unsafe { self.binary.binary_start.add(self.init_offset) } as *const fn(),
            [
                unsafe { self.binary.binary_start.add(self.rthdr_offset) } as usize,
                0,
                0,
                0,
                0,
                0,
                0,
                0,
            ],
            &mut unsafe { AccessScope::new() },
        )?;

        if a0 != 0 {
            panic!("Init failed: {}", a0);
        } else {
            Ok(())
        }
    }
}

impl<'m, ID: EFID, M: MPU> EncapfnRt for EncapfnTockRv32iCRt<'m, ID, M> {
    type TargetABI = Riscv32iCABI;
    type ID = ID;
    type AllocTracker = EncapfnTockRv32iCRtAllocTracker;

    fn allocate_stacked<F, R>(&self, size: usize, align: usize, fun: F) -> Result<R, EFError>
    where
        F: FnOnce(*mut u8) -> R,
    {
        // TODO: horribly unsafe
        let original_stack_ptr = self.stack_ptr.get();
        let size_stack_ptr = original_stack_ptr.wrapping_sub(size);
        let align_stack_ptr =
            ((size_stack_ptr as usize) - (size_stack_ptr as usize % align)) as *mut u8;

        if (align_stack_ptr as usize) < (self.ram_region_start as usize) {
            Err(EFError::AllocNoMem)
        } else {
            self.stack_ptr.set(align_stack_ptr);
            let res = fun(align_stack_ptr);
            self.stack_ptr.set(original_stack_ptr);
            Ok(res)
        }
    }

    fn allocate_stacked_t<'a, T: Sized, F, R>(
        &self,
        alloc_scope: &'a mut AllocScope<Self::AllocTracker, ID>,
        fun: F,
    ) -> Result<R, EFError>
    where
        F: FnOnce(EFAllocation<'_, ID, T>, &mut AllocScope<Self::AllocTracker, ID>) -> R,
    {
        self.allocate_stacked(
            core::mem::size_of::<T>(),
            core::mem::align_of::<T>(),
            |allocated_ptr| {
                fun(
                    unsafe { EFAllocation::from_allocated_ptr(allocated_ptr) },
                    &mut unsafe { AllocScope::new(*alloc_scope.tracker()) },
                )
            },
        )
    }

    fn allocate_stacked_array<'a, const N: usize, T: Sized, F, R>(
        &self,
        alloc_scope: &'a mut AllocScope<Self::AllocTracker, ID>,
        fun: F,
    ) -> Result<R, EFError>
    where
        F: FnOnce(*mut [T; N], &mut AllocScope<Self::AllocTracker, ID>) -> R,
    {
        self.allocate_stacked(
            core::mem::size_of::<T>() * N,
            core::mem::align_of::<T>(),
            |allocated_ptr| {
                fun(allocated_ptr as *mut [T; N], &mut unsafe {
                    AllocScope::new(*alloc_scope.tracker())
                })
            },
        )
    }

    fn allocate_stacked_slice<'a, T, F, R>(
        &self,
        len: usize,
        alloc_scope: &'a mut AllocScope<Self::AllocTracker, ID>,
        fun: F,
    ) -> Result<R, EFError>
    where
        F: FnOnce(*mut [T], &mut AllocScope<Self::AllocTracker, ID>) -> R,
    {
        self.allocate_stacked(
            core::mem::size_of::<T>() * len,
            core::mem::align_of::<T>(),
            |allocated_ptr| {
                fun(
                    unsafe { core::slice::from_raw_parts_mut(allocated_ptr as *mut T, len) }
                        as *mut [T],
                    &mut unsafe { AllocScope::new(*alloc_scope.tracker()) },
                )
            },
        )
    }

    fn resolve_function_pointer(&self, function_index: usize) -> Option<*const fn()> {
        const FNPTR_SIZE: usize = core::mem::size_of::<*const fn()>();

        let fnptr_offset = self.fntab_offset + (function_index * FNPTR_SIZE);
        if fnptr_offset + FNPTR_SIZE <= self.binary.binary_length {
            let fnptr_slice = unsafe {
                core::slice::from_raw_parts(
                    (self.binary.binary_start as usize + fnptr_offset) as *const u8,
                    FNPTR_SIZE,
                )
            };
            Some(u32::from_ne_bytes([
                fnptr_slice[0],
                fnptr_slice[1],
                fnptr_slice[2],
                fnptr_slice[3],
            ]) as *const fn())
        } else {
            None
        }
    }

    #[cfg(any(not(target_arch = "riscv32"), not(target_os = "none")))]
    fn invoke_service<'a>(
        &self,
        _fun: *const fn(),
        _params: <Self::TargetABI as EncapfnABI>::ParametersContainer,
        _access_scope: &mut AccessScope<ID>,
    ) -> Result<(usize, usize), ()> {
        // Avoid unused code warnings when compiling for non-rv32i
        let _ = self.mpu;
        let _ = self.mpu_config;
        unimplemented!()
    }

    #[cfg(all(target_arch = "riscv32", target_os = "none"))]
    fn invoke_service<'a>(
        &self,
        fun: *const fn(),
        params: <Self::TargetABI as EncapfnABI>::ParametersContainer,
        access_scope: &mut AccessScope<ID>,
    ) -> Result<(usize, usize), ()> {
        use core::arch::asm;

        let [mut a0, mut a1, a2, a3, a4, a5, a6, a7] = params;

        // Make sure that the stack is always aligned to a multiple of 4 words
        // (16 bytes). Try to move it downward to achieve alignment. If this
        // doesn't fit, return an error accordingly:
        let original_stack_ptr = self.stack_ptr.get();
        let aligned_stack_ptr =
            ((original_stack_ptr as usize) - (original_stack_ptr as usize % 16)) as *mut u8;
        if (aligned_stack_ptr as usize) < (self.ram_region_start as usize) {
            return Err(());
        }

        self.mpu.configure_mpu(&self.mpu_config);
        self.mpu.enable_app_mpu();

        let mut service_pc: u32 = fun as u32;
        let mut service_sp: u32 = aligned_stack_ptr as u32;
        let mut service_mcause: u32;
        let mut service_mtval: u32;
        let mut return_to_kernel_addr: u32;

        unsafe {
            // We need to ensure that the compiler does not reorder
            // kernel memory writes to after the userspace context switch
            // to ensure we provide a consistent memory view of
            // application-accessible buffers.
            //
            // The compiler will not be able to reorder memory accesses
            // beyond this point, as the "nomem" option on the asm!-block
            // is not set, hence the compiler has to assume the assembly
            // will issue arbitrary memory accesses (acting as a compiler
            // fence).
            asm!("
                // Before switching to the service we need to save some kernel
                // registers to the kernel stack, specifically ones which we can't
                // mark as clobbered in the asm!() block. We set up the stack such
                // that the `_start_trap` handler will invoke our context-specific
                // trap handler by jumping to `_service_trap_continue` below in this
                // block. Finally, activate this trap handler by storing the stack
                // pointer in the `mscratch` CSR.
                //
                // It may happen that the trap handler is invoked because of an
                // interrupt, in which case we'll have to save all caller-saved
                // registers of the service's register file. To avoid the risk of
                // nested trap handlers while saving this register file (and
                // a nested trap handler then clobbering our CSRs), we pre-allocate
                // space for those registers before even scheduling the service.
                //
                // Here is a memory map of our intended stack layout, to make it
                // easier to keep track:
                //
                // ```
                // 28*4(sp):          <- original stack pointer
                // 27*4(sp):
                // 26*4(sp): x31 (t6)
                // 25*4(sp): x30 (t5)
                // 24*4(sp): x29 (t4)
                // 23*4(sp): x28 (t3)
                // 22*4(sp): x17 (a7)
                // 21*4(sp): x16 (a6)
                // 20*4(sp): x15 (a5)
                // 19*4(sp): x14 (a4)
                // 18*4(sp): x13 (a3)
                // 17*4(sp): x12 (a2)
                // 16*4(sp): x11 (a1)
                // 15*4(sp): x10 (a0)
                // 14*4(sp): x7 (t2)
                // 13*4(sp): x6 (t1)
                // 12*4(sp): x5 (t0)
                // 11*4(sp): x4 (tp)
                // 10*4(sp): x3 (gp)
                //  9*4(sp): x2 (sp)
                //  8*4(sp): x1 (ra)
                //  7*4(sp): pc (from mepc, saved in s3)
                //
                // ^-^-^-^-^-^ pre-allocated caller-saved registers ^-^-^-^-^-^-^-^-^-
                //
                //  6*4(sp): x9 (s1)
                //  5*4(sp): x8 (fp)
                //  4*4(sp): x4 (tp)
                //  3*4(sp): x3 (gp)
                //  2*4(sp): previous (kernel-mode) mscratch trap handler address
                //
                // v-v-v-v-v-v mandated stack layout by `_start_trap` v-v-v-v-v-v-v-v-
                //
                //  1*4(sp): scratch word (written with `s3` in `_start_trap`)
                //  0*4(sp): _service_trap_continue (100) (address to jump to on trap)
                // ```
                //
                // We make sure that the new stack pointer is 16 byte aligned, as per
                // RISC-V calling convention.

                addi sp, sp, -28*4  // Move the stack pointer down to make room.

                // Save all registers on the kernel stack that cannot be clobbered
                // by an asm!() block. These are mostly registers which have a
                // designated purpose (e.g. stack pointer) or are used internally
                // by LLVM.
                sw   x9,  6*4(sp)   // s1 (used internally by LLVM)
                sw   x8,  5*4(sp)   // fp (can't be clobbered / used as an operand)
                sw   x4,  4*4(sp)   // tp (can't be clobbered / used as an operand)
                sw   x3,  3*4(sp)   // gp (can't be clobbered / used as an operand)
                //   x2             // sp -> saved in mscratch CSR below

                // From here on we can't allow the CPU to take interrupts anymore,
                // as it would schedule our trap handler with an inconsistent
                // register file.
                //
                // If this is executed _after_ setting mscratch, this result
                // in the race condition of
                // [PR 2308](https://github.com/tock/tock/pull/2308)

                // Therefore, clear the following bits in mstatus first:
                //   0x00000008 -> bit 3 -> MIE (disabling interrupts here)
                // + 0x00001800 -> bits 11,12 -> MPP (switch to usermode on mret)
                li    t5, 0x00001808
                csrrc t4, mstatus, t5  // clear bits in mstatus, read prev in t4

                // Afterwards, if interrupts were enabled in machine mode
                // previously, re-enable them on the context switch into the
                // service:
                //
                //   0x00000008
                // & 0xXXXXXXX?
                // ------------
                //   0x0000000R << 4   (R = 0 or 8 depending on MIE above)
                //   ---------------
                //   0x000000R0     -> bit 7 -> MPIE (enable interrupts on mret)
                //
                // By ANDing first and shifting afterwards, we can use
                // compressed instructions for `li` as the immediate is small
                // enough.

                li    t5, 0x00000008
                and   t5, t5, t4       // only set bit 7 if MIE was set
                slli  t5, t5, 4        // move bit 3 to bit 7 in t4
                csrrs x0, mstatus, t5  // set bits in mstatus, don't care about read

                // Indicate that the trap handler should jump to the
                // `_service_trap_continue` on a trap.
                //
                // In asm!() we can't use the shorthand `li` pseudo-instruction, as it
                // complains about _app_trap_continue (100) not being a constant in
                // the required range.
                lui  t5, %hi(200f)
                addi t5, t5, %lo(200f)
                sw   t5, 0*4(sp)

                // Swap out the (kernel-mode) dynamic dispatch trap-handler stack
                // for our current `sp`. This will point the `_start_trap` handler to
                // the `_app_trap_continue` symbol for handling traps arriving while
                // in userspace.
                //
                // We further save the kernel-mode trap handler stack pointer on our
                // current stack, to restore it when we're switching back into kernel
                // mode.
                csrrw t5, mscratch, sp
                sw    t5, 2*4(sp)

                // We have to set the mepc CSR with the PC we want the service to start
                // executing at:
                csrw mepc, s2

                // Set the return address to point to _service_return_to_kernel. This
                // is an `ecall` instruction, which will either execute (when not
                // enforcing memory protection) and subsequenty jump to the trap
                // handler, or will cause an Instruction Access Fault and thus also
                // cause the trap handler to execute:
                lui  ra, %hi(100f)
                addi ra, ra, %lo(100f)

                // The function call arguments are already loaded into registers
                // a0 -- a7 by the inline asm!() block.

                // Load the service stack pointer:
                mv   sp, s3

                // Call mret to jump to where mepc points, switch to user mode, and
                // start running the service.
                mret

                // The following instruction should NEVER be executed on systems which
                // have proper memory protection for containerized services. This
                // address is set as the return address for the executed service
                // routine. We expect the application attempting to jump to this
                // address to trigger an Instruction Access Fault. The Rust function
                // surrounding this asm!() block can then inspect mcause and mtval
                // to determine whether this was a general fault, or an attempt at
                // returning from the foreign function.
                //
                // While we could theoretically use any inaccessible address for this,
                // we specifically point to this `ecall` instruction to also support
                // services running without memory isolation, for instance during
                // development:
              100: // _service_return_to_kernel
                ecall

                // This is where the trap handler jumps back to after the service
                // stops executing. We will still be in the trap handler context, and
                // have to switch back to kernel mode using `mret` accordingly.
              200: // _service_trap_continue

                // At this point all we know is that we entered the trap handler
                // from the service. We don't know _why_ we got a trap, it could be
                // from an interrupt, syscall, or fault (or maybe something else).
                // Therefore we have to be very careful not to overwrite any
                // registers before we determined the strategy to handle this trap:
                //
                // - In the case of an interrupt, we want to disable it and return
                //   to the service with an unmodified register file.
                //
                // - In the case of an instruction access fault at address 0x0,
                //   we assume that the service has finished executing. We want
                //   to return to the kernel stack and process the return value.
                //
                // - For any other trap, return to the kernel and interpret this as a
                //   service fault. We need to re-initialize the server to bring it
                //   into a consistent state again.
                //
                // We arrive at this userspace-specific trap handler through a
                // dispatch by the generic `_start_trap` handler. The generic
                // handler has made the following modifications to the register
                // file:
                //
                // - `s2` now points to our current stack frame, as previously
                //   saved to the `mscratch` CSR. The `mscratch` CSR contains the
                //   app's `s2` register value.
                //
                // - the `s3` register is clobbered, its original value stored at
                //   `1*4(s2)`.

                // Load the mcause CSR to determine why the app stopped executing:
                csrr s3, mcause

                // Check if this was an interrupt, and if it was, then we need to
                // disable the interrupt before returning from this trap handler so
                // that it does not fire again. If mcause is greater than or equal
                // to zero this was not an interrupt (i.e. the most significant bit
                // is not 1).
                bge  s3, zero, 300f // jump to _service_trap_continue_fault

                // This was an interrupt. We need to invoke a kernel-function to
                // disable interrupts. While this is possible from within the trap
                // handler context, we still need to preserve the service register
                // file. Furthermore, this function may perform allocations on the
                // kernel stack, which may overrun it. Thus, to prevent this trap
                // handler from being scheduled because of a kernel stack overflow, we
                // need to swap out the trap handler again.

                // First, save all caller-saved registers (and a few others which we
                // don't want to leak from C to Rust, such as gp and tp) into the
                // pre-allocated regions on the stack. This should not be able to
                // cause a trap:
                sw   x31, 26*4(s2)
                sw   x30, 25*4(s2)
                sw   x29, 24*4(s2)
                sw   x28, 23*4(s2)
                sw   x17, 22*4(s2)
                sw   x16, 21*4(s2)
                sw   x15, 20*4(s2)
                sw   x14, 19*4(s2)
                sw   x13, 18*4(s2)
                sw   x12, 17*4(s2)
                sw   x11, 16*4(s2)
                sw   x10, 15*4(s2)
                sw    x7, 14*4(s2)
                sw    x6, 13*4(s2)
                sw    x5, 12*4(s2)
                sw    x4, 11*4(s2)
                sw    x3, 10*4(s2)
                sw    x2,  9*4(s2)
                sw    x1,  8*4(s2)

                // We saved the service's stack pointer (sp), so load our proper stack
                // pointer (currently in s2) into that register:
                mv    sp, s2

                // With these registers saved, we can clobber one to extract the
                // service's `pc` register from the mepc CSR and save that as well.
                // This CSR may be overwritten in the kernel-mode trap handler.
                csrr t0, mepc    // TODO: use a saved register instead of t0
                sw   t0, 7*4(sp)

                // All registers saved in their pre-allocated caller-saved register slots!

                // Then, swap the kernel dynamic-dispatch trap handler stack back into
                // mscratch. This provides us with the service's `s2`, which we retain
                // in that register:
                lw   t0, 2*4(sp)
                csrrw s2, mscratch, t0

                // Load the previously extracted mcause into `a0`, passing that as an
                // argument to disable interrupts from within Rust:
                mv   a0, s3
                jal  ra, _disable_interrupt_trap_rust_from_app

                // With interrupts disabled, we can exit the trap handler and return
                // to the service. For this, restore the application context:
                lw   x31, 26*4(sp)
                lw   x30, 25*4(sp)
                lw   x29, 24*4(sp)
                lw   x28, 23*4(sp)
                lw   x17, 22*4(sp)
                lw   x16, 21*4(sp)
                lw   x15, 20*4(sp)
                lw   x14, 19*4(sp)
                lw   x13, 18*4(sp)
                lw   x12, 17*4(sp)
                lw   x11, 16*4(sp)
                lw   x10, 15*4(sp)
                lw    x7, 14*4(sp)
                lw    x6, 13*4(sp)
                lw    x5, 12*4(sp)
                lw    x4, 11*4(sp)
                lw    x3, 10*4(sp)
                                   // don't overwrite sp / x2 just yet
                lw    x1,  8*4(sp)

                // Load the service PC from its stored offset into mepc, which we will
                // jump to on `mret`:
                lw    s3,  7*4(sp)
                csrw  mepc, s3

                // Re-insert our stack pointer (sp) into the mscratch CSR, swapping
                // out the kernel one. We re-save the mscratch value, as it
                // potentially have changed.
                csrrw s3, mscratch, sp
                sw    s3, 2*4(sp)

                // Load the original s3 from the offset it was placed in by the
                // `_start_trap` handler:
                lw    s3,  1*4(sp)

                // Finally, replace our stack pointer (sp) by the original value of
                // the `sp` / `x2` register, as saved on the stack:
                lw    sp, 9*4(sp)

                // Return to the service:
                mret

              300: // _service_trap_continue_fault

                // A non-interrupt fault occured. Switch back to the kernel. By
                // retaining mcause and mtval in registers, we allow the surrounding
                // Rust function to interpret this fault as either a function return
                // or a proper fault, in response to which we need to return an error
                // and reinitialize the service.

                // Restore the original kernel dynamic-dispatch trap handler
                // stack into the mscratch CSR. The `_start_trap` handler has
                // loaded the service's `s2` into mscratch register, but we can
                // safely discard its value at this point:
                lw   s3, 2*4(s2)
                csrw mscratch, s3

                // Switch back to the kernel stack and store the service stack pointer
                // in s3:
                mv   s3, sp
                mv   sp, s2

                // To continue executing this assembly block after returning from the
                // trap handler, load _service_return_to_kernel into mepc, and
                // retaining the app's previous PC in `s2`.
                lui  s2, %hi(400f)
                addi s2, s2, %lo(400f)
                csrrw s2, mepc, s2

                // Extract a few required CSRs:
                csrr s4, mcause
                csrr s5, mtval

                // Need to set mstatus.MPP to 0b11 so that we stay in machine mode.
                li   t0, 0x1800   // Load 0b11 to the MPP bits location in t1
                csrrs x0, mstatus, t0

                // Use mret to exit the trap handler and return to the context
                // switching code.
                mret

              400: // _service_return_to_kernel

                // Restore the kernel registers before resuming kernel code.
                lw   x9,  6*4(sp)  // s1 (used internally by LLVM)
                lw   x8,  5*4(sp)  // fp (can't be clobbered / used as an operand)
                lw   x4,  4*4(sp)  // tp (can't be clobbered / used as an operand)
                lw   x3,  3*4(sp)  // gp (can't be clobbered / used as an operand)
                //   x2            // sp -> loaded from mscratch by the trap handler

                // Load the local _service_return_to_kernel address into a
                // register for comparison with mtval. If those are identical,
                // and mcause indicates an Instruction Access Fault, the service
                // attempted to return to the kernel.
                lui  s6, %hi(100b)
                addi s6, s6, %lo(100b)

                addi sp, sp, 28*4   // Reset kernel stack pointer
            ",

                // Function argument registers.
                //
                // Unmodified in the assembly and simply passed into the
                // function. The first two (`a0` and `a1`) will further be
                // written with the return value.
                inout("x10") a0, inout("x11") a1,
                //
                // The rest are not read back by Rust:
                in("x12") a2, lateout("x12") _,
                in("x13") a3, lateout("x13") _,
                in("x14") a4, lateout("x14") _,
                in("x15") a5, lateout("x15") _,
                in("x16") a6, lateout("x16") _,
                in("x17") a7, lateout("x17") _,

                // `service_pc` / `s2` carries the desired function pointer to switch
                // to, and will contain the `mepc` PC register value when the function
                // has stopped executing due to a fault / return. Inspect this value
                // to interpret an Instruction Access Violation as a "syscall":
                inout("x18") service_pc,   // s2

                // The service's stack pointer (`service_sp`). After function
                // execution, the service's SP register will further be provided in
                // this register:
                inout("x19") service_sp,   // s3

                // Trap handler's `mcause` and `mtval` CSR, captured in the trap
                // handler switching back from service to kernel:
                out("x20") service_mcause, // s4
                out("x21") service_mtval,  // s5

                // Reference-address for the app return address. If we get an
                // instruction access fault (mcause = 1) with an mepc equal to
                // this address, interpret this as an attempted "return to
                // kernel" / syscall. Otherwise, handle it as a fault.
                out("x22") return_to_kernel_addr, // s6

                // Clobber all registers (except those marked as inputs or outputs
                // above, or ones we can't clobber). We don't trust the service to
                // respect the RISC-V ABI, so this includes callee-saved registers:
                out("x1") _,  out("x5") _, out("x6") _, out("x7") _,
                out("x23") _, out("x24") _, out("x25") _, out("x26") _,
                out("x27") _, out("x28") _, out("x29") _, out("x30") _,
                out("x31") _,

                // Thus, we arrive at the follwing clobbered / non-clobbered
                // & saved registers:
                //
                //  x0: N/A   x1: CLB   x2: SVD   x3: SVD
                //  x4: SVD   x5: CLB   x6: CLB   x7: CLB
                //  x8: SVD   x9: SVD  x10: CLB  x11: CLB
                // x12: CLB  x13: CLB  x14: CLB  x15: CLB
                // x16: CLB  x17: CLB  x18: CLB  x19: CLB
                // x20: CLB  x21: CLB  x22: CLB  x23: CLB
                // x24: CLB  x25: CLB  x26: CLB  x27: CLB
                // x28: CLB  x29: CLB  x30: CLB  x31: CLB
            );
        }

        self.mpu.disable_app_mpu();

        // if !init {
        // panic!(
        //         "Service returned! a0: {:08x}, pc: {:p}, sp: {:p}, mcause: \
        //          {:08x}, mtval: {:08x}, rettokern: {:08x}, PMPConfig: {}, \
        //          stack_top: {:p}",
        //         a0,
        //         service_pc as *const u8,
        //         service_sp as *const u8,
        //         service_mcause,
        //         service_mtval,
        //      return_to_kernel_addr,
        //         self.mpu_config,
        //     self.stack_top,
        // );
        // }

        // Reset the stack pointer:
        self.stack_ptr.set(original_stack_ptr);

        // Check whether we returned from the service because it voluntarily
        // ceased control (e.g. `ecall` INSN or Instruction Access Fault when
        // trying to jump to `ecall`), or whether it faulted:
        if service_mcause == 8 || (service_mcause == 1 && service_mtval == return_to_kernel_addr) {
            // The service voluntarily ceased control by returning to the
            // address specified in `ra`.
            Ok((a0, a1))
        } else {
            // The service faulted. TODO: do something sensible instead of
            // panicing.
            panic!(
                "Service faulted! a0: {:08x}, pc: {:p}, sp: {:p}, mcause: \
                 {:08x}, mtval: {:08x}, rettokern: {:08x}, PMPConfig: {}",
                a0,
                service_pc as *const u8,
                service_sp as *const u8,
                service_mcause,
                service_mtval,
                return_to_kernel_addr,
                self.mpu_config
            );

            // Err(())
        }
    }
}
