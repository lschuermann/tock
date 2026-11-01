// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

//! [`SbiBoundary`] for running a supervisor-mode guest on a RISC-V hart.
//!
//! The guest (e.g., Linux) runs in S-mode, with its own virtual memory
//! (`satp`), and has its own traps and interrupts delegated to it. Tock is not
//! involved in any of this, except for:
//!
//! - **Memory protection:** the guest gets a single PMP region (through
//!   Tock's [`MPU`] abstraction) covering its memory. PMP checks apply to all
//!   S-mode accesses, including page table walks.
//!
//! - **Per-guest trap configuration:** delegation (`medeleg`, `mideleg`),
//!   counter access (`mcounteren`), and address translation (`satp`) are only
//!   set up while the guest executes. They are reset when switching back to
//!   the kernel, as U-mode Tock processes are subject to them as well (e.g., a
//!   delegated U-mode `ecall` would otherwise end up in the guest's trap
//!   handler, and a non-Bare `satp` would translate a process's addresses).
//!
//! - **Interrupts:** the guest's supervisor-level interrupts are virtual (see
//!   [`SbiGuestInterrupts`]), and are injected into `mip` on every switch to
//!   the guest. Machine-level interrupts (Tock's own) always trap to Tock,
//!   preempting the guest.
//!
//! Supervisor-only CSRs (`stvec`, `sepc`, `sscratch`, ...) and the
//! supervisor fields of `mstatus` are not used by the rest of Tock, and are
//! thus left as-is. This only supports a single guest per hart.

use kernel::debug;
use kernel::platform::mpu::{self, MPU};
use kernel::utilities::cells::MapCell;
use kernel::utilities::registers::interfaces::{Readable, Writeable};
use riscv_csr::csr::ReadWriteRiscvCsr;

use super::{SbiBoundary, SbiGuestInterrupts, SbiRet, SbiTrap, sip};
use crate::csr::{CSR, mcause};
use crate::syscall::{
    self, MSTATUS_MPP_SUPERVISOR, R_A0, R_A1, R_A2, R_A3, R_A4, R_A5, R_A6, R_A7, RiscvStoredState,
};

/// The `mstatus.MPP` field.
const MSTATUS_MPP_MASK: usize = 0b11 << 11;

// CSRs that are only used for running SBI guests.
const MEDELEG: ReadWriteRiscvCsr<usize, (), 0x302> = ReadWriteRiscvCsr::new();
const MIDELEG: ReadWriteRiscvCsr<usize, (), 0x303> = ReadWriteRiscvCsr::new();
const MCOUNTEREN: ReadWriteRiscvCsr<usize, (), 0x306> = ReadWriteRiscvCsr::new();
const SATP: ReadWriteRiscvCsr<usize, (), 0x180> = ReadWriteRiscvCsr::new();
const MENVCFG: ReadWriteRiscvCsr<usize, (), 0x30a> = ReadWriteRiscvCsr::new();
const MENVCFGH: ReadWriteRiscvCsr<usize, (), 0x31a> = ReadWriteRiscvCsr::new();

/// `menvcfg.STCE` (bit 63, in `menvcfgh` on RV32): enables the Sstc
/// extension's `stimecmp`, which makes `mip.STIP` read-only.
const MENVCFG_STCE: usize = 1 << (crate::XLEN - 1);

/// Exceptions delegated to the guest.
///
/// These are instruction address misaligned (0), illegal instruction (2),
/// breakpoint (3), environment call from U-mode (8), and instruction, load,
/// and store/AMO page faults (12, 13, 15). Environment calls from S-mode (9, SBI calls) and access faults (1, 5, 7,
/// PMP violations) trap to Tock.
const GUEST_MEDELEG: usize =
    (1 << 0) | (1 << 2) | (1 << 3) | (1 << 8) | (1 << 12) | (1 << 13) | (1 << 15);

/// Allow the guest to read the `cycle`, `time`, and `instret` counters.
const GUEST_MCOUNTEREN: usize = 0b111;

/// State of an S-mode guest while it is not executing.
#[derive(Default)]
pub struct RiscvSbiGuestState {
    /// General purpose registers, PC, and the last trap's `mcause` and
    /// `mtval`.
    regs: RiscvStoredState,
    /// The guest's address translation configuration.
    satp: usize,
    /// The supervisor-level interrupts the guest has enabled (`sie`).
    sie: usize,
    /// The privilege mode (`mstatus.MPP` encoding) to resume the guest in:
    /// the guest may be interrupted while executing in either S-mode or
    /// U-mode (e.g., Linux userspace).
    mpp: usize,
}

/// Runs an S-mode guest in a fixed memory region.
pub struct RiscvSbiBoundary<'a, M: MPU> {
    mpu: &'a M,
    mpu_config: MapCell<M::MpuConfig>,
    entry: usize,
    hartid: usize,
    dtb: usize,
}

impl<'a, M: MPU> RiscvSbiBoundary<'a, M> {
    /// Create a boundary for a guest that owns the memory region
    /// `[memory_start, memory_start + memory_size)`, and starts executing at
    /// `entry`, with `a0 = hartid` and `a1 = dtb` (the Linux boot protocol).
    ///
    /// Returns `None` if the memory region cannot be protected with a single
    /// MPU region.
    pub fn new(
        mpu: &'a M,
        memory_start: *const u8,
        memory_size: usize,
        entry: usize,
        hartid: usize,
        dtb: usize,
    ) -> Option<Self> {
        let mut config = mpu.new_config()?;
        let region = mpu.allocate_region(
            memory_start,
            memory_size,
            memory_size,
            mpu::Permissions::ReadWriteExecute,
            &mut config,
        )?;
        // The guest must get exactly the requested region, as it assumes it
        // can use all of it (e.g., as described in its device tree).
        if region.start_address() != memory_start || region.size() != memory_size {
            return None;
        }

        // The guest's timer interrupt is raised by Tock (through the SBI timer
        // extension), so it must be writable: disable Sstc.
        if crate::XLEN == 64 {
            MENVCFG.read_and_clear_bits(MENVCFG_STCE);
        } else {
            MENVCFGH.read_and_clear_bits(MENVCFG_STCE);
        }

        Some(RiscvSbiBoundary {
            mpu,
            mpu_config: MapCell::new(config),
            entry,
            hartid,
            dtb,
        })
    }
}

impl<M: MPU> SbiBoundary for RiscvSbiBoundary<'_, M> {
    type StoredState = RiscvSbiGuestState;

    fn initial_state(&self) -> RiscvSbiGuestState {
        let mut state = RiscvSbiGuestState {
            mpp: MSTATUS_MPP_SUPERVISOR,
            ..Default::default()
        };
        state.regs.pc = self.entry;
        state.regs.regs[R_A0] = self.hartid;
        state.regs.regs[R_A1] = self.dtb;
        state
    }

    unsafe fn switch_to_guest(
        &self,
        state: &mut RiscvSbiGuestState,
        interrupts: &SbiGuestInterrupts,
    ) -> SbiTrap {
        // Restrict the guest to its memory region.
        self.mpu_config.map(|config| {
            // ### Safety
            //
            // The configuration only grants access to the guest's own memory
            // region, which the board dedicated to this guest.
            unsafe { self.mpu.configure_mpu(config) };
        });
        self.mpu.enable_app_mpu();

        // Set up the guest's trap delegation, counters, address translation,
        // and supervisor-level interrupts.
        MEDELEG.set(GUEST_MEDELEG);
        MIDELEG.set(sip::ALL);
        let kernel_mcounteren = MCOUNTEREN.get();
        MCOUNTEREN.set(GUEST_MCOUNTEREN);
        SATP.set(state.satp);
        CSR.mie.read_and_set_bits(state.sie);
        CSR.mip.read_and_set_bits(interrupts.pending() & sip::ALL);

        // ### Safety
        //
        // We configured and enabled memory protection for the guest above, and
        // switch to supervisor or user mode, both below machine mode.
        let trap_mstatus =
            unsafe { syscall::switch_to_lower_privilege(&mut state.regs, state.mpp) };
        state.mpp = trap_mstatus & MSTATUS_MPP_MASK;

        // Back in the kernel: save the guest's state, and restore the
        // configuration for the kernel and Tock processes.
        state.satp = SATP.get();
        SATP.set(0);
        state.sie = CSR.mie.read_and_clear_bits(sip::ALL) & sip::ALL;
        // The guest can set and clear its own software interrupt, all other
        // supervisor-level interrupts are raised by Tock.
        let mip = CSR.mip.read_and_clear_bits(sip::ALL);
        interrupts.clear(sip::SSIP);
        interrupts.set(mip & sip::SSIP);
        MCOUNTEREN.set(kernel_mcounteren);
        MIDELEG.set(0);
        MEDELEG.set(0);

        // ### Safety
        //
        // We always re-enable memory protection before switching to the guest.
        unsafe { self.mpu.disable_app_mpu() };

        let regs = &state.regs;
        match mcause::Trap::from(regs.mcause) {
            // A machine-level interrupt (only those trap to Tock).
            mcause::Trap::Interrupt(_) => SbiTrap::Interrupted,
            mcause::Trap::Exception(mcause::Exception::SupervisorEnvCall) => SbiTrap::Ecall {
                eid: regs.regs[R_A7],
                fid: regs.regs[R_A6],
                args: [
                    regs.regs[R_A0],
                    regs.regs[R_A1],
                    regs.regs[R_A2],
                    regs.regs[R_A3],
                    regs.regs[R_A4],
                    regs.regs[R_A5],
                ],
            },
            mcause::Trap::Exception(exception) => {
                debug!(
                    "SBI guest fault: {:?} at pc {:#x} (mtval {:#x})",
                    exception, regs.pc, regs.mtval
                );
                SbiTrap::Fault
            }
        }
    }

    fn set_ecall_return(&self, state: &mut RiscvSbiGuestState, ret: SbiRet) {
        state.regs.regs[R_A0] = ret.error as usize;
        state.regs.regs[R_A1] = ret.value;
        // Advance past the `ecall` instruction.
        state.regs.pc = state.regs.pc.wrapping_add(4);
    }
}
