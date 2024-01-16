use core::cell::Cell;
use core::marker::PhantomData;

use kernel::platform::mpu::{self, MPU};

use crate::abi::{armv7m::ArmV7MCABI, EncapfnABI};
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

#[cfg(any(not(target_arch = "arm"), not(target_os = "none")))]
pub unsafe extern "C" fn return_to_kernel_springboard() {}

#[cfg(all(
    target_arch = "arm",
    target_feature = "v7",
    target_feature = "thumb-mode",
    target_os = "none"
))]
#[naked]
pub unsafe extern "C" fn return_to_kernel_springboard() {
    core::arch::asm!("svc 0", options(noreturn));
}

pub struct EncapfnTockCortexMCRt<'m, ID: EFID, M: MPU> {
    binary: EncapfnBinary,
    ram_region_start: *mut u8,
    _ram_region_len: usize,
    rthdr_offset: usize,
    init_offset: usize,
    fntab_offset: usize,

    stack_top: *mut u8,
    stack_ptr: Cell<*mut u8>,

    mpu: &'m M,

    /// Configuration data for the MPU
    mpu_config: M::MpuConfig,

    _id: PhantomData<ID>,
}

#[derive(Clone, Copy)]
pub struct EncapfnTockCortexMCRtAllocTracker {
    ram_region_start: *mut u8,
    ram_region_len: usize,
}

impl AllocTracker for EncapfnTockCortexMCRtAllocTracker {}

impl<'m, ID: EFID, M: MPU> EncapfnTockCortexMCRt<'m, ID, M> {
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
            AllocScope<EncapfnTockCortexMCRtAllocTracker, ID>,
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
        // panic!("{:p} {:08x}", binary.binary_start, binary.binary_length);

        // THIS IS A HACK TO SATISFY THE MPU CONSTRAINTS.
        // mpu.allocate_region(
        //     binary.binary_start,
        //     binary.binary_length,
        //     binary.binary_length,
        //     mpu::Permissions::ReadExecuteOnly,
        //     &mut mpu_config,
        // )
        // .unwrap();
        mpu.allocate_region(
            0x00040000 as *const u8,
            0x00010000,
            0x00010000,
            mpu::Permissions::ReadExecuteOnly,
            &mut mpu_config,
        )
        .unwrap();

        // mpu.allocate_region(
        //     ram_region_start,
        //     ram_region_len,
        //     ram_region_len,
        //     mpu::Permissions::ReadWriteOnly,
        //     &mut mpu_config,
        // )
        // .unwrap();
        mpu.allocate_region(
            0x20020000 as *const u8,
            0x00020000,
            0x00020000,
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
        let encapfn = EncapfnTockCortexMCRt {
            binary,
            ram_region_start,
            _ram_region_len: ram_region_len,
            rthdr_offset,
            init_offset,
            fntab_offset,

            stack_top: stack_top,
            stack_ptr: Cell::new(stack_top),

            mpu,
            mpu_config,

            _id: PhantomData,
        };

        encapfn.init()?;

        Ok((
            encapfn,
            unsafe {
                AllocScope::new(EncapfnTockCortexMCRtAllocTracker {
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

impl<'m, ID: EFID, M: MPU> EncapfnRt for EncapfnTockCortexMCRt<'m, ID, M> {
    type TargetABI = ArmV7MCABI;
    type ID = ID;
    type AllocTracker = EncapfnTockCortexMCRtAllocTracker;

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

    #[cfg(any(not(target_arch = "arm"), not(target_os = "none")))]
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

    #[cfg(all(
        target_arch = "arm",
        target_feature = "v7",
        target_feature = "thumb-mode",
        target_os = "none"
    ))]
    fn invoke_service<'a>(
        &self,
        fun: *const fn(),
        params: <Self::TargetABI as EncapfnABI>::ParametersContainer,
        access_scope: &mut AccessScope<ID>,
    ) -> Result<(usize, usize), ()> {
        use core::arch::asm;

        let [r0, r1, r2, r3] = params;

        let lr_springboard: usize = (return_to_kernel_springboard as usize) | 1;

        // The SVC hardware stack frame is 8 words in size, and must be 4-byte
        // aligned. Perform this alignment. The modulo operation should get
        // optimized to a simple AND instruction:
        let original_stack_ptr = self.stack_ptr.get();
        let aligned_stack_ptr = (original_stack_ptr as usize)
            .saturating_sub(original_stack_ptr as usize % 4)
            as *mut u8;

        // Now, add the 8-word SVC frame to the stack ptr:
        let mut svc_frame_stack_ptr = (original_stack_ptr as usize)
            .saturating_sub(8 * core::mem::size_of::<usize>())
            as *mut u8;

        // Finally, make sure that the stack pointer is still contained in the
        // process' RAM region.
        if (svc_frame_stack_ptr as usize) < (self.ram_region_start as usize) {
            return Err(());
        }

        unsafe {
            // Now, set the process state (PC, register arguments, etc.)  by writing
            // to the SVC frame. The stack pointer has guaranteed 4-byte alignment,
            // so we can treat it as a `usize` pointer:
            // let springboard = return_to_kernel_springboard as *const fn();
            let stack_bottom = svc_frame_stack_ptr as *mut usize;
            core::ptr::write(stack_bottom.offset(7), 0); //............. -> APSR
            core::ptr::write(stack_bottom.offset(6), fun as usize | 1); // -> PC
            core::ptr::write(stack_bottom.offset(5), lr_springboard); //.. -> LR
            core::ptr::write(stack_bottom.offset(3), params[3]); //..... -> R3
            core::ptr::write(stack_bottom.offset(2), params[2]); //..... -> R2
            core::ptr::write(stack_bottom.offset(1), params[1]); //..... -> R1
            core::ptr::write(stack_bottom.offset(0), params[0]); //..... -> R0
        }

        self.mpu.configure_mpu(&self.mpu_config);
        self.mpu.enable_app_mpu();

        // let mut service_pc: u32 = fun as u32;
        // let mut service_sp: u32 = aligned_stack_ptr as u32;
        // let mut service_mcause: u32;
        // let mut service_mtval: u32;
        // let mut return_to_kernel_addr: u32;

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
                // Rust `asm!()` macro (as of May 2021) will not let us mark r6, r7 and r9
                // as clobbers. r6 and r9 is used internally by LLVM, and r7 is used for
                // the frame pointer. However, in the process of restoring and saving the
                // process's registers, we do in fact clobber r6, r7 and r9. So, we work
                // around this by doing our own manual saving of r6 using r2, r7 using r3,
                // r9 using r12, and then mark those as clobbered.
                mov r2, r6                        // r2 = r6
                mov r3, r7                        // r3 = r7
                mov r12, r9                       // r12 = r9

                // The arguments passed in are:
                // - `r0` is the bottom of the user stack
                // - `r1` is a reference to `CortexMStoredState.regs`

                // Load bottom of stack into Process Stack Pointer.
                msr psp, r0                       // PSP = r0

              100:
                // Generate a SVC exception to handle the context switch from
                // kernel to userspace. It doesn't matter which SVC number we
                // use here as it is not used in the exception handler. Data
                // being returned from a syscall is transferred on the app's
                // stack.
                svc 0xff

                // When execution returns here we have switched back to the
                // kernel from the application. This can be because of a
                // SYSTICK timer interrupt, hard fault, context switch, or
                // another generic interrupt. We stop the function execution for
                // all except the latter -- this function executes synchronous
                // to kernel code, so we'll handle interrupts when the function
                // returns. The SYSTICK timer can be used to prevent functions
                // from blocking the kernel for too long (at the price of
                // wrecking their execution state).
                //
                // Currently, we can't distiguish the SYSTICK interrupt from
                // another generic interrupt -- thus we assume that a generic
                // interrupt happened:
                ldr r0, ={syscall_fired}
                ldr r0, [r0, #0]
                cmp r0, #0
                bne 200f // a syscall happened, return to kernel

                ldr r0, ={app_hard_fault}
                ldr r0, [r0, #0]
                cmp r0, #0
                beq 100b // neither syscall nor hard fault, return to app

              200:
                // Update the user stack pointer with the current value after
                // the application has executed.
                mrs r0, PSP                       // r0 = PSP

                // Need to restore r6, r7 and r12 since we clobbered them when switching to
                // and from the app.
                mov r6, r2                        // r6 = r2
                mov r7, r3                        // r7 = r3
                mov r9, r12                       // r9 = r12
            ",

             inout("r0") svc_frame_stack_ptr,

             // Clobber all registers (except those marked as inputs or
             // outputs above, or ones we can't clobber). We don't trust the
             // service to respect the RISC-V ABI, so this includes
             // callee-saved registers:
             out("r1") _, out("r2") _, out("r3") _, out("r4") _,
             out("r5") _, out("r8") _, out("r9") _, out("r10") _,
             out("r11") _, out("r12") _,

             syscall_fired = sym cortexm::syscall::SYSCALL_FIRED,
             app_hard_fault = sym cortexm::syscall::APP_HARD_FAULT,
            );
        }

        self.mpu.disable_app_mpu();

        if (svc_frame_stack_ptr as usize) < (self.ram_region_start as usize)
            || (svc_frame_stack_ptr as usize).saturating_add(8) > (self.stack_top as usize)
            || (svc_frame_stack_ptr as usize % 4 != 0)
        {
            panic!(
                "Illegal return stack pointer: {:p} {:p} {:p}",
                svc_frame_stack_ptr, self.ram_region_start, self.stack_top,
            );
        }

        // svc_frame_stack_ptr is properly aligned, so cast to *const usize
        let stack_bottom = svc_frame_stack_ptr as *const usize;
        let pc = unsafe { core::ptr::read(stack_bottom.offset(6)) };

        // Now we can check whether the application returned because of a
        // fault. We further need to reset the status bits of `APP_HARD_FAULT`
        // and `SYSCALL_FIRED`, as expected by the process implementation:
        let app_fault = unsafe { core::ptr::read_volatile(&cortexm::syscall::APP_HARD_FAULT) };
        unsafe {
            core::ptr::write_volatile(&mut cortexm::syscall::APP_HARD_FAULT, 0);
        }
        let syscall_fired = unsafe { core::ptr::read_volatile(&cortexm::syscall::SYSCALL_FIRED) };
        unsafe {
            core::ptr::write_volatile(&mut cortexm::syscall::SYSCALL_FIRED, 0);
        }

        if syscall_fired == 1 || (app_fault == 1 && (pc | 1) == lr_springboard) {
            let (r0, r1) = unsafe {
                (
                    core::ptr::read(stack_bottom.offset(0)),
                    core::ptr::read(stack_bottom.offset(1)),
                )
            };
            Ok((r0, r1))
        } else {
            panic!(
                "Abnormal return: {} {} {:p} 0x{:08x}",
                app_fault == 1,
                syscall_fired == 1,
                stack_bottom,
                pc
            );
        }
    }
}
