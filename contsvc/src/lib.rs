#![no_std]

use core::ops::Range;

#[derive(Copy, Clone, Debug)]
pub struct ContSvcBinary {
    pub tbf_start: Option<*const u8>,
    pub binary_start: *const u8,
    pub binary_length: usize,
}

impl ContSvcBinary {
    // TODO: change to raw pointer slice, remove 'static lifetime
    // requirement in parse_tbf_header_lengths
    pub fn find(svc_name: &str, app_flash: &'static [u8]) -> Result<Self, ()> {
        let mut remaining_flash = app_flash;

        loop {
            // Get the first eight bytes of flash to check if there is another
            // app.
            let test_header_slice = match remaining_flash.get(0..8) {
                Some(s) => s,
                None => {
                    // Not enough flash to test for another app. This just means
                    // we are at the end of flash, and there are no more apps to
                    // load.
                    return Err(());
                }
            };

            // Pass the first eight bytes to tbfheader to parse out the length of
            // the tbf header and app. We then use those values to see if we have
            // enough flash remaining to parse the remainder of the header.
            let (version, header_length, entry_length) =
                match tock_tbf::parse::parse_tbf_header_lengths(
                    test_header_slice.try_into().or(Err(()))?,
                ) {
                    Ok((v, hl, el)) => (v, hl, el),
                    Err(tock_tbf::types::InitialTbfParseError::InvalidHeader(entry_length)) => {
                        // If we could not parse the header, then we want to skip over
                        // this app and look for the next one.
                        (0, 0, entry_length)
                    }
                    Err(tock_tbf::types::InitialTbfParseError::UnableToParse) => {
                        // Since Tock apps use a linked list, it is very possible the
                        // header we started to parse is intentionally invalid to signal
                        // the end of apps. This is ok and just means we have finished
                        // loading apps.
                        return Err(());
                    }
                };

            // Now we can get a slice which only encompasses the length of flash
            // described by this tbf header.  We will either parse this as an actual
            // app, or skip over this region.
            let entry_flash = remaining_flash.get(0..entry_length as usize).ok_or(())?;

            // Advance the flash slice for process discovery beyond this last entry.
            // This will be the start of where we look for a new process since Tock
            // processes are allocated back-to-back in flash.
            remaining_flash = remaining_flash.get(entry_flash.len()..).ok_or(())?;

            if header_length > 0 {
                // If we found an actual app header, try to create a `Process`
                // object. We also need to shrink the amount of remaining memory
                // based on whatever is assigned to the new process if one is
                // created.

                // Get a slice for just the app header.
                let header_flash = entry_flash.get(0..header_length as usize).ok_or(())?;

                // Parse the full TBF header to see if this is a valid app. If the
                // header can't parse, we will error right here.
                if let Ok(tbf_header) = tock_tbf::parse::parse_tbf_header(header_flash, version) {
                    let process_name = tbf_header.get_package_name().unwrap();

                    // If the app is enabled, it's a real app and not what we are looking for.
                    if tbf_header.enabled() {
                        continue;
                    }

                    if svc_name != process_name {
                        continue;
                    }

                    return Ok(ContSvcBinary {
                        tbf_start: Some(entry_flash.as_ptr() as *const u8),
                        binary_start: unsafe {
                            entry_flash
                                .as_ptr()
                                .offset(tbf_header.get_protected_size() as isize)
                        },
                        binary_length: entry_length as usize
                            - tbf_header.get_protected_size() as usize,
                    });
                }
            };
        }
    }
}

pub const CONTSVC_HEADER_MAGIC_OFFSET: usize = 0;
pub const CONTSVC_HEADER_RTHDR_PTR_OFFSET: usize = 4;
pub const CONTSVC_HEADER_FNTAB_PTR_OFFSET: usize = 8;
pub const CONTSVC_HEADER_LEN: usize = 12;
pub const CONTSVC_HEADER_MAGIC: u32 = 0x43535643;

#[derive(Debug)]
pub struct ContSvc {
    binary: ContSvcBinary,
    ram_region_start: *mut u8,
    ram_region_len: usize,
    rthdr_offset: usize,
    fntab_offset: usize,

    stack_top: *mut u8,

    /// Configuration data for the MPU
    mpu_config: MapCell<<<C as Chip>::MPU as MPU>::MpuConfig>,

    /// MPU regions are saved as a pointer-size pair.
    mpu_regions: [Cell<Option<mpu::Region>>; 6],
}

impl ContSvc {
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
        binary: ContSvcBinary,
        ram_region_start: *mut u8,
        ram_region_len: usize,
    ) -> Result<Self, ()> {
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
        // available with read-execute permissions.

        // Make sure we have at least enough data to parse the header:
        if binary.binary_length < CONTSVC_HEADER_LEN {
            return Err(());
        }

        // We generally try to avoid retaining Rust slices to the containerized
        // service binary (to avoid unsoundness, in case this memory should
        // change). However, for parsing the header, we can create an ephemeral
        // slice given that we verified the length:
        let header_slice = core::slice::from_raw_parts(binary.binary_start, CONTSVC_HEADER_LEN);

        #[inline(always)]
        fn extract_header_word(header_slice: &[u8], offset: usize) -> u32 {
            let word_slice = &header_slice[offset..offset + core::mem::size_of::<u32>()];
            u32::from_ne_bytes([word_slice[0], word_slice[1], word_slice[2], word_slice[3]])
        }

        // Read the header fields in native endianness. First, check the magic:
        if extract_header_word(header_slice, CONTSVC_HEADER_MAGIC_OFFSET) != CONTSVC_HEADER_MAGIC {
            return Err(());
        }

        let rthdr_offset =
            extract_header_word(header_slice, CONTSVC_HEADER_RTHDR_PTR_OFFSET) as usize;
        if rthdr_offset > binary.binary_length - core::mem::size_of::<u32>() {
            return Err(());
        }

        let fntab_offset =
            extract_header_word(header_slice, CONTSVC_HEADER_FNTAB_PTR_OFFSET) as usize;
        if fntab_offset > binary.binary_length - core::mem::size_of::<u32>() {
            return Err(());
        }

        // All header fields parsed, we can construct the `ContSvc` struct and
        // try to initialize the service:
        let contsvc = ContSvc {
            binary,
            ram_region_start,
            ram_region_len,
            rthdr_offset,
            fntab_offset,

            stack_top: unsafe { ram_region_start.offset(ram_region_len as isize) },
        };

        kernel::debug!("ContSvc: {:x?}", contsvc);

        contsvc.init()?;

        Ok(contsvc)
    }

    // TODO: make private
    pub fn invoke_service(
        &self,
        fun: *const fn(),
        mut a0: usize,
        mut a1: usize,
        mut a2: usize,
        mut a3: usize,
        mut a4: usize,
        mut a5: usize,
        mut a6: usize,
        mut a7: usize,
    ) {
        use core::arch::asm;

        let mut service_pc: u32 = fun as u32;
        let mut service_sp: u32 = self.stack_top as u32;
        let mut service_mcause: u32;
        let mut service_mtval: u32;

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
          // 27*4(s2):
          // 26*4(s2): scratch word (for service's `s2` register w/ interrupt)
          // 25*4(s2): x31 (t6)
          // 24*4(s2): x30 (t5)
          // 23*4(s2): x29 (t4)
          // 22*4(s2): x28 (t3)
          // 21*4(s2): x17 (a7)
          // 20*4(s2): x16 (a6)
          // 19*4(s2): x15 (a5)
          // 18*4(s2): x14 (a4)
          // 17*4(s2): x13 (a3)
          // 16*4(s2): x12 (a2)
          // 15*4(s2): x11 (a1)
          // 14*4(s2): x10 (a0)
          // 13*4(s2): x7 (t2)
          // 12*4(s2): x6 (t1)
          // 11*4(s2): x5 (t0)
          // 10*4(s2): x4 (tp)
          //  9*4(s2): x3 (gp)
          //  8*4(s2): x1 (ra)
          //  7*4(s2): pc (from mepc, saved in s3)
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
          sw   x9,  7*4(sp)   // s1 (used internally by LLVM)
          sw   x8,  6*4(sp)   // fp (can't be clobbered / used as an operand)
          sw   x4,  5*4(sp)   // tp (can't be clobbered / used as an operand)
          sw   x3,  4*4(sp)   // gp (can't be clobbered / used as an operand)
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
          li t5, 0x00001808
          csrrc x0, 0x300, t5    // clear bits in mstatus, don't care about read

          // Afterwards, set the following bits in mstatus:
          //   0x00000080 -> bit 7 -> MPIE (enable interrupts on mret)
          li t5, 0x00000080
          csrrs x0, 0x300, t5    // set bits in mstatus, don't care about read

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
          sw   x31, 25*4(s2)
          sw   x30, 24*4(s2)
          sw   x29, 23*4(s2)
          sw   x28, 22*4(s2)
          sw   x17, 21*4(s2)
          sw   x16, 20*4(s2)
          sw   x15, 19*4(s2)
          sw   x14, 18*4(s2)
          sw   x13, 17*4(s2)
          sw   x12, 16*4(s2)
          sw   x11, 15*4(s2)
          sw   x10, 14*4(s2)
          sw    x7, 13*4(s2)
          sw    x6, 12*4(s2)
          sw    x5, 11*4(s2)
          sw    x4, 10*4(s2)
          sw    x3,  9*4(s2)
          sw    x1,  8*4(s2)

          // With these registers saved, we can clobber one to extract the
          // service's `pc` register from the mepc CSR and save that as well.
          // This CSR may be overwritten in the kernel-mode trap handler.
          csrr t0, mepc
          sw   t0, 7*4(s2)
     
          // Then, swap the kernel dynamic-dispatch trap handler stack back into
          // mscratch, and save its previous contents (the service's `s2`
          // register) in a scratch-word on the stack.
          lw   t0, 2*4(s2)
          csrrw t0, mscratch, t0
          sw   t0, 26*4(s2)

          // Load the previously extracted mcause into `a0`, passing that as an
          // argument to disable interrupts from within Rust:
          mv   a0, s3
          jal  ra, _disable_interrupt_trap_rust_from_app

          // With interrupts disabled, we can exit the trap handler and return
          // to the service. For this, restore the application context:
          lw   x31, 25*4(s2)
          lw   x30, 24*4(s2)
          lw   x29, 23*4(s2)
          lw   x28, 22*4(s2)
          lw   x17, 21*4(s2)
          lw   x16, 20*4(s2)
          lw   x15, 19*4(s2)
          lw   x14, 18*4(s2)
          lw   x13, 17*4(s2)
          lw   x12, 16*4(s2)
          lw   x11, 15*4(s2)
          lw   x10, 14*4(s2)
          lw    x7, 13*4(s2)
          lw    x6, 12*4(s2)
          lw    x5, 11*4(s2)
          lw    x4, 10*4(s2)
          lw    x3,  9*4(s2)
          lw    x1,  8*4(s2)

          // Load the service PC from its stored offset into mepc, which we will
          // jump to on `mret`:
          lw    s3,  7*4(s2)
          csrw  mepc, s3

          // Re-insert our stack pointer (s2) into the mscratch CSR, swapping it
          // out for the kernel one. We re-save the mscratch value, as it could
          // potentially have changed.
          csrrw s3, mscratch, s2
          sw    s3, 2*4(sp)

          // Load the original s3 from the offset it was placed in by the
          // `_start_trap` handler:
          lw    s3,  1*4(s2)

          // Finally, replace our stack pointer (s2) by the original value of
          // the s2 register, as saved on the stack:
          lw    s2, 26*4(s2)

          // Return to the service:
          mret

        300: // _service_trap_continue_fault

          // A non-interrupt fault occured. Switch back to the kernel. By
          // retaining mcause and mtval in registers, we allow the surrounding
          // Rust function to interpret this fault as either a function return
          // or a proper fault, in response to which we need to return an error
          // and reinitialize the service.

          // Restore the original kernel dynamic-dispatch trap handler stack
          // into the mscratch CSR. The `_start_trap` handler has loaded the
          // service's `s2` into mscratch register, but we can safely discard
          // its value at this point:
          lw   s3, 2*4(sp)
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
          csrr t0, 0x300    // CSR=0x300=mstatus
          li   t1, 0x1800   // Load 0b11 to the MPP bits location in t1
          or   t0, t0, t1   // Set the MPP bits to one
          csrw 0x300, t0    // CSR=0x300=mstatus

          // Use mret to exit the trap handler and return to the context
          // switching code.
          mret

        400: // _service_return_to_kernel          

          // Restore the kernel registers before resuming kernel code.
          lw   x9,  7*4(sp)  // s1 (used internally by LLVM)
          lw   x8,  6*4(sp)  // fp (can't be clobbered / used as an operand)
          lw   x4,  5*4(sp)  // tp (can't be clobbered / used as an operand)
          lw   x3,  4*4(sp)  // gp (can't be clobbered / used as an operand)
          //   x2            // sp -> loaded from mscratch by the trap handler

          addi sp, sp, 28*4   // Reset kernel stack pointer
          ",

            inout("a0") a0, inout("a1") a1, inout("a2") a2, inout("a3") a3,
                inout("a4") a4, inout("a5") a5, inout("a6") a6, inout("a7") a7,

            // mcause & mtval from the trap handler are provided through
            // registers:
            inout("s2") service_pc,
            inout("s3") service_sp,
            out("s4") service_mcause,
            out("s5") service_mtval,

                // Clobber all registers which can be marked as clobbered, except
                // those marked as inputs or outputs above. We don't trust the
            // service to respect the RISC-V ABI, so this includes callee-saved
            // registers:
                out("x1") _,  out("x5") _, out("x6") _, out("x7") _,
                out("x22") _, out("x23") _, out("x24") _, out("x25") _, out("x26") _,
                out("x27") _, out("x28") _, out("x29") _, out("x30") _, out("x31") _,
              );
        }

        panic!("Back in kernel! A0: {}", a0);
    }

    pub fn init(&self) -> Result<(), ()> {
        // unimplemented!();
        Ok(())
    }
}
