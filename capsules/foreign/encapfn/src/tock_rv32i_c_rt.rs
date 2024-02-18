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
                // Before switching to the function we need to save some kernel
                // registers to the kernel stack, specifically ones which we can't
                // mark as clobbered in the asm!() block. We set up the stack such
                // that the `_start_trap` handler will invoke our context-specific
                // trap handler by jumping to `_function_trap_continue` below in this
                // block. Finally, activate this trap handler by storing the stack
                // pointer in the `mscratch` CSR.
                //
                // It may happen that the trap handler is invoked because of an
                // interrupt, in which case we'll have to save all caller-saved
                // registers of the function's register file. To avoid the risk of
                // nested trap handlers while saving this register file (and
                // a nested trap handler then clobbering our CSRs), we pre-allocate
                // space for those registers before even scheduling the function.
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
                //  7*4(sp): pc (from mepc, saved in s3) // TODO: remove maybe?
                //
                // ^-^-^-^-^-^ pre-allocated caller-saved registers ^-^-^-^-^-^-^-^-^-
                //
                //  6*4(sp): x9 (s1)
                //  5*4(sp): x8 (fp)
                //  4*4(sp): x4 (tp)
                //  3*4(sp): x3 (gp)
                //  2*4(sp): // TODO remove
                //
                // v-v-v-v-v-v mandated stack layout by `_start_trap` v-v-v-v-v-v-v-v-
                //
                //  1*4(sp): // TODO remove
                //  0*4(sp): // temporary scratch space for the function's stack pointer
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
                //   x2             // sp -> saved in KERNEL_STACK_POINTER below

                la t0, {kernel_stack_ptr_sym} // Addr of KERNEL_STACK_POINTER
                sw sp, 0*4(t0)                // Save current stack pointer

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
                // function:
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

                li    t0, 0x00000008
                and   t0, t0, t4       // only set bit 7 if MIE was set
                slli  t0, t0, 4        // move bit 3 to bit 7 in t4
                csrrs x0, mstatus, t0  // set bits in mstatus, don't care about read

                // Switch the trap handler to _function_return_to_kernel
                la t0, 200f
                csrw mscratch, t0

                // We have to set the mepc CSR with the PC we want the function to start
                // executing at:
                csrw mepc, s2

                // Set the return address to point to _function_return_to_kernel. This
                // is an `ecall` instruction, which will either execute (when not
                // enforcing memory protection) and subsequenty jump to the trap
                // handler, or will cause an Instruction Access Fault and thus also
                // cause the trap handler to execute:
                la ra, 100f

                // The function call arguments are already loaded into registers
                // a0 -- a7 by the inline asm!() block.

                // Load the function stack pointer:
                mv sp, s3

                // Call mret to jump to where mepc points, switch to user mode, and
                // start running the function.
                mret

                // The following instruction should NEVER be executed on systems which
                // have proper memory protection for containerized functions. This
                // address is set as the return address for the executed function
                // routine. We expect the application attempting to jump to this
                // address to trigger an Instruction Access Fault. The Rust function
                // surrounding this asm!() block can then inspect mcause and mtval
                // to determine whether this was a general fault, or an attempt at
                // returning from the foreign function.
                //
                // While we could theoretically use any inaccessible address for this,
                // we specifically point to this `ecall` instruction to also support
                // functions running without memory isolation, for instance during
                // development:
              100: // _function_return_to_kernel
                ecall

                // This is where the trap handler jumps back to after the function
                // stops executing. We will still be in the trap handler context, and
                // have to switch back to kernel mode using `mret` accordingly.
              200: // _start_function_trap

                // At this point all we know is that we entered the trap handler
                // from the function. We don't know _why_ we got a trap, it could be
                // from an interrupt, syscall, or fault (or maybe something else).
                // Therefore we have to be very careful not to overwrite any
                // registers before we determined the strategy to handle this trap:
                //
                // - In the case of an interrupt, we want to disable it and return
                //   to the function with an unmodified register file.
                //
                // - In the case of an instruction access fault at address 0x0,
                //   we assume that the function has finished executing. We want
                //   to return to the kernel stack and process the return value.
                //
                // - For any other trap, return to the kernel and interpret this as a
                //   function fault. We need to re-initialize the server to bring it
                //   into a consistent state again.
                //
                // We arrive at this userspace-specific trap handler through a
                // dispatch by the global `_start_trap` handler. The generic
                // handler has made the following modifications to the register
                // file:
                //
                // - the function's t0 has been swapped into mscratch. We're
                //   free to clobber t0, as the address of _start_function_trap
                //   (prior value of mscratch is well known to us).

                // Load the mcause CSR to determine why the app stopped executing:
                csrr t0, mcause

                // Check if this was an interrupt, and if it was, then we need to
                // disable the interrupt before returning from this trap handler so
                // that it does not fire again. If mcause is greater than or equal
                // to zero this was not an interrupt (i.e. the most significant bit
                // is not 1).
                bge  t0, zero, 300f // jump to _function_trap_continue_fault

                // This was an interrupt. We need to invoke a kernel-function to
                // disable interrupts. While this is possible from within the trap
                // handler context, we still need to preserve the function register
                // file. Furthermore, this function may perform allocations on the
                // kernel stack, which may overrun it. Thus, to prevent this trap
                // handler from being scheduled because of a kernel stack overflow, we
                // need to swap out the trap handler again.

                // First, restore the kernel stack pointer. We still need access
                // to mcause later on, but reading from a CSR is cheap. Thus
                // overwrite t0:
                la t0, {kernel_stack_ptr_addr}
                lw t0, 0*4(t0)

                // First, save all caller-saved registers (and a few others which we
                // don't want to leak from C to Rust, such as gp and tp) into the
                // pre-allocated regions on the stack. This should not be able to
                // cause a trap:
                sw   x31, 26*4(t0) // t6
                sw   x30, 25*4(t0) // t5
                sw   x29, 24*4(t0) // t4
                sw   x28, 23*4(t0) // t3
                sw   x17, 22*4(t0) // a7
                sw   x16, 21*4(t0) // a6
                sw   x15, 20*4(t0) // a5
                sw   x14, 19*4(t0) // a4
                sw   x13, 18*4(t0) // a3
                sw   x12, 17*4(t0) // a2
                sw   x11, 16*4(t0) // a1
                sw   x10, 15*4(t0) // a0
                sw    x7, 14*4(t0) // t2
                sw    x6, 13*4(t0) // t1
                // -- x5  12*4(t0) -> t0, currently in mscratch, stored below
                sw    x4, 11*4(t0) // tp
                sw    x3, 10*4(t0) // gp
                sw    x2,  9*4(t0) // sp
                sw    x1,  8*4(t0) // ra

                // We saved the function's sp, restore the kernel stack pointer
                // into that register:
                mv sp, t0

                // Before we switch to the Rust function to disable interrupts, we
                // want to restore the kernel trap handler. This will ensure that
                // we still properly panic on a trap caused by this function.
                //
                // For this, clear mscratch and store the function's t0. We don't
                // clobber t0, as that contains mcause and is needed below:
                csrrw t1, mscratch, zero
                sw t1, 12*4(sp) // t0

                // All registers saved in their pre-allocated caller-saved
                // register slots!
                //
                // Load mcause into `a0`, passing that as an argument to disable
                // interrupts from within Rust. Because we're operating from
                // within an interrupt trap handler context, we assume that all
                // other traps within this function are faults and result in a
                // panic. Hence we should never return here with a clobbered
                // mepc CSR.
                csrr a0, mscratch
                jal  ra, _disable_interrupt_trap_rust_from_app

                // With interrupts disabled, we can exit the trap handler and return
                // to the function. For this, restore the application context:
                lw   x31, 26*4(sp) // t6
                lw   x30, 25*4(sp) // t5
                lw   x29, 24*4(sp) // t4
                lw   x28, 23*4(sp) // t3
                lw   x17, 22*4(sp) // a7
                lw   x16, 21*4(sp) // a6
                lw   x15, 20*4(sp) // a5
                lw   x14, 19*4(sp) // a4
                lw   x13, 18*4(sp) // a3
                lw   x12, 17*4(sp) // a2
                lw   x11, 16*4(sp) // a1
                lw   x10, 15*4(sp) // a0
                lw    x7, 14*4(sp) // t2
                lw    x6, 13*4(sp) // t1
                lw -- x5  12*4(sp) -> t0, restored below
                lw    x4, 11*4(sp) // tp
                lw    x3, 10*4(sp) // gp
                // -- x2 - 9*4(sp) -> sp, restored below
                lw    x1,  8*4(sp) // ra

                // Restore our trap handler:
                la t0, {kernel_stack_ptr_addr}
                csrw mscratch, t0

                // Load the function's t0:
                lw t0, 12*4(sp)

                // Load the function's stack pointer. Do this last, as we
                // overwite our pointer We don't need to re-save the kernel
                // stack pointer, it is unchanged:
                lw sp, 9*4(sp)

                // Return to the function:
                mret

              300: // _function_trap_continue_fault

                // A non-interrupt fault occured. Switch back to the kernel. By
                // retaining mcause and mtval in registers, we allow the surrounding
                // Rust function to interpret this fault as either a function return
                // or a proper fault, in response to which we need to return an error
                // and reinitialize the function.

                // When we arrive here, we have the function's t0 in the
                // mscratch CSR, and the value of mcause in t0.
                //
                // Keep mcause in t0, as we need it in the surrounding Rust
                // function.

                // Restore the kernel trap handler. We aren't interested in the
                // app's t0, which is currently stored there.
                csrw mscratch, zero

                // To continue executing this assembly block after returning from the
                // trap handler, load _function_return_to_kernel into mepc, and
                // retaining the app's previous PC in `t1`:
                la t1, 400f
                csrrw t1, mepc, t1

                // Switch back to the kernel stack while retaining the
                // function's stack pointer in t2:
                mv t2, sp                         // Store the function's sp in t1
                la sp, {kernel_stack_ptr_addr}    // Load addr of KERNEL_STACK_POINTER
                lw sp, 0*4(sp)                    // Load the kernel stack pointer

                // Extract a few required CSRs:
                csrr t3, mtval

                // Need to set mstatus.MPP to 0b11 so that we stay in machine mode.
                li   t4, 0x1800   // Load 0b11 to the MPP bits location in t1
                csrs mstatus, t0  // mstatus |= 0x00001800

                // Use mret to exit the trap handler and return to the context
                // switching code.
                mret

              400: // _function_return_to_kernel

                // Restore the kernel registers before resuming kernel code.
                lw   x9,  6*4(sp)  // s1 (used internally by LLVM)
                lw   x8,  5*4(sp)  // fp (can't be clobbered / used as an operand)
                lw   x4,  4*4(sp)  // tp (can't be clobbered / used as an operand)
                lw   x3,  3*4(sp)  // gp (can't be clobbered / used as an operand)
                //   x2            // sp -> loaded from mscratch by the trap handler

                // Load the local _function_return_to_kernel address into a
                // register for comparison with mtval. If those are identical,
                // and mcause indicates an Instruction Access Fault, the function
                // attempted to return to the kernel.
                la t4, 100b

                addi sp, sp, 28*4   // Reset kernel stack pointer
            ",

		sym kernel_stack_ptr_addr = core::mem::addr_of!(rv32i::syscall::KERNEL_STACK_PTR),

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

                // `function_pc` / `t1` carries the desired function pointer to switch
                // to, and will contain the `mepc` PC register value when the function
                // has stopped executing due to a fault / return. Inspect this value
                // to interpret an Instruction Access Violation as a "syscall":
                inout("x6") function_pc,   // t1

                // The function's stack pointer (`function_sp`). After function
                // execution, the function's SP register will further be provided in
                // this register:
                inout("x7") function_sp,   // t2

                // Trap handler's `mcause` and `mtval` CSR, captured in the trap
                // handler switching back from function to kernel:
                out("x5") function_mcause, // t0
                out("x28") function_mtval,  // t3

                // Reference-address for the app return address. If we get an
                // instruction access fault (mcause = 1) with an mepc equal to
                // this address, interpret this as an attempted "return to
                // kernel" / syscall. Otherwise, handle it as a fault.
                out("x29") return_to_kernel_addr, // t4

                // Clobber all registers (except those marked as inputs or outputs
                // above, or ones we can't clobber). We don't trust the function to
                // respect the RISC-V ABI, so this includes callee-saved registers:
                out("x1") _ /*   ra */, out("x18") _ /*  s2 */,
	        out("x19") _ /*  s3 */, out("x20") _ /*  s4 */,
	        out("x21") _ /*  s5 */, out("x22") _ /*  s6 */,
	        out("x23") _ /*  s7 */, out("x24") _ /*  s8 */,
	        out("x25") _ /*  s9 */, out("x26") _ /* s10 */,
	        out("x27") _ /* s11 */, out("x30") _ /*  t5 */,
                out("x31") _ /*  t6 */,
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
        if function_mcause == 8 || (function_mcause == 1 && function_mtval == return_to_kernel_addr) {
            // The function voluntarily ceased control by returning to the
            // address specified in `ra`.
            Ok((a0, a1))
        } else {
            // The function faulted. TODO: do something sensible instead of
            // panicing.
            panic!(
                "Function faulted! a0: {:08x}, pc: {:p}, sp: {:p}, mcause: \
                 {:08x}, mtval: {:08x}, rettokern: {:08x}, PMPConfig: {}",
                a0,
                function_pc as *const u8,
                function_sp as *const u8,
                function_mcause,
                function_mtval,
                return_to_kernel_addr,
                self.mpu_config
            );

            // Err(())
        }
    }
}
