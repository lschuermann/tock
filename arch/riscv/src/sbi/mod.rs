// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

//! RISC-V Supervisor Binary Interface (SBI) processes.
//!
//! An [`SbiProcess`] runs a guest (e.g., a supervisor-mode operating system)
//! whose only interface to the Tock kernel is the [RISC-V SBI][sbi]. Such a
//! process does not use Tock's system call ABI and does not have grants,
//! upcalls, or process buffers. It therefore only implements
//! [`Schedulable`], and can co-exist with regular Tock processes in the same
//! kernel.
//!
//! Switching to and from the guest is abstracted by [`SbiBoundary`], and SBI
//! calls are serviced by an [`SbiHandler`], which takes the role of system call
//! drivers for Tock processes.
//!
//! [`boundary::RiscvSbiBoundary`] runs a real S-mode guest (e.g., Linux),
//! and [`basic::BasicSbi`] implements the SBI extensions such a guest needs
//! (console, timer, and a few single-hart basics). [`mock`] contains a
//! scripted guest which does not execute any code.
//!
//! [sbi]: https://github.com/riscv-non-isa/riscv-sbi-doc

use core::cell::Cell;

use kernel::process::{KernelHooks, Process, ProcessId, Schedulable, StepResult};
use kernel::utilities::cells::MapCell;

pub mod basic;
pub mod boundary;
pub mod mock;

/// SBI extension IDs (EIDs).
pub mod eid {
    /// Legacy Console Putchar extension.
    pub const LEGACY_CONSOLE_PUTCHAR: usize = 0x01;
    /// Legacy Console Getchar extension.
    pub const LEGACY_CONSOLE_GETCHAR: usize = 0x02;
    /// Base extension.
    pub const BASE: usize = 0x10;
    /// Timer extension (`"TIME"`).
    pub const TIME: usize = 0x5449_4D45;
    /// IPI extension (`"sPI"`).
    pub const IPI: usize = 0x0073_5049;
    /// Hart State Management extension (`"HSM"`).
    pub const HSM: usize = 0x0048_534D;
    /// System Reset extension (`"SRST"`).
    pub const SRST: usize = 0x5352_5354;
}

/// Function IDs (FIDs) of the SBI base extension.
pub mod base_fid {
    pub const GET_SPEC_VERSION: usize = 0;
    pub const GET_IMPL_ID: usize = 1;
    pub const GET_IMPL_VERSION: usize = 2;
    pub const PROBE_EXTENSION: usize = 3;
    pub const GET_MVENDORID: usize = 4;
    pub const GET_MARCHID: usize = 5;
    pub const GET_MIMPID: usize = 6;
}

/// SBI error codes.
pub mod error {
    pub const SUCCESS: isize = 0;
    pub const ERR_FAILED: isize = -1;
    pub const ERR_NOT_SUPPORTED: isize = -2;
    pub const ERR_INVALID_PARAM: isize = -3;
}

/// Supervisor-level interrupt bits in `mip`/`mie` (and `sip`/`sie`).
pub mod sip {
    /// Supervisor software interrupt.
    pub const SSIP: usize = 1 << 1;
    /// Supervisor timer interrupt.
    pub const STIP: usize = 1 << 5;
    /// Supervisor external interrupt.
    pub const SEIP: usize = 1 << 9;
    /// All supervisor-level interrupts.
    pub const ALL: usize = SSIP | STIP | SEIP;
}

/// Supervisor-level interrupts pending for an SBI guest.
///
/// The guest's interrupts are virtual: they are set by its [`SbiHandler`]
/// (e.g., when a timer requested through the SBI expires), and injected by the
/// [`SbiBoundary`] whenever the kernel switches to the guest. This allows them
/// to be raised while the guest is not executing, without affecting the rest
/// of the kernel.
pub struct SbiGuestInterrupts {
    pending: Cell<usize>,
}

impl SbiGuestInterrupts {
    pub const fn new() -> Self {
        SbiGuestInterrupts {
            pending: Cell::new(0),
        }
    }

    /// The currently pending interrupts (a combination of [`sip`] bits).
    pub fn pending(&self) -> usize {
        self.pending.get()
    }

    /// Mark the interrupts `bits` as pending.
    pub fn set(&self, bits: usize) {
        self.pending.set(self.pending.get() | bits);
    }

    /// Mark the interrupts `bits` as no longer pending.
    pub fn clear(&self, bits: usize) {
        self.pending.set(self.pending.get() & !bits);
    }
}

/// The return value of an SBI call, passed back to the guest in `a0` (error)
/// and `a1` (value).
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct SbiRet {
    pub error: isize,
    pub value: usize,
}

impl SbiRet {
    pub const fn success(value: usize) -> Self {
        SbiRet {
            error: error::SUCCESS,
            value,
        }
    }

    pub const fn error(error: isize) -> Self {
        SbiRet { error, value: 0 }
    }
}

/// Reason why a guest returned control to the kernel.
#[derive(Copy, Clone, Debug)]
pub enum SbiTrap {
    /// The guest issued an `ecall`, i.e., an SBI call. `eid` is passed in
    /// `a7`, `fid` in `a6`, and the arguments in `a0`-`a5`.
    Ecall {
        eid: usize,
        fid: usize,
        args: [usize; 6],
    },
    /// The guest was interrupted by a hardware event.
    Interrupted,
    /// The guest caused a trap that is not handled by the kernel.
    Fault,
}

/// The architecture-specific boundary between the kernel and an SBI guest.
///
/// This is the equivalent of
/// [`UserspaceKernelBoundary`](kernel::syscall::UserspaceKernelBoundary) for
/// SBI processes. It is used directly by [`SbiProcess`], the core kernel does
/// not know about it.
pub trait SbiBoundary {
    /// Guest state saved while the guest is not executing (e.g., its
    /// registers).
    type StoredState;

    /// The state of the guest before it first executes.
    fn initial_state(&self) -> Self::StoredState;

    /// Switch to the guest, inject its pending `interrupts`, and return once it
    /// traps back into the kernel.
    ///
    /// ### Safety
    ///
    /// The implementation must ensure that memory protection is configured
    /// such that the guest cannot access any memory it does not own. The
    /// caller must ensure that `state` belongs to this guest.
    unsafe fn switch_to_guest(
        &self,
        state: &mut Self::StoredState,
        interrupts: &SbiGuestInterrupts,
    ) -> SbiTrap;

    /// Set the return value of the last SBI call, and advance the guest past
    /// its `ecall` instruction.
    fn set_ecall_return(&self, state: &mut Self::StoredState, ret: SbiRet);
}

/// What to do with the guest after an SBI call has been handled.
#[derive(Copy, Clone, Debug)]
pub enum SbiCallResult {
    /// Return to the guest with the given return value.
    Return(SbiRet),
    /// The call cannot be completed right now (e.g., because a buffer is
    /// full). The guest will re-issue it the next time it executes.
    Retry,
    /// The guest requested to be shut down.
    Shutdown,
}

/// Services SBI calls issued by an [`SbiProcess`]. This takes the role of
/// system call drivers for Tock processes.
pub trait SbiHandler {
    fn sbi_call(
        &self,
        guest: ProcessId,
        eid: usize,
        fid: usize,
        args: &[usize; 6],
    ) -> SbiCallResult;
}

/// The state of an [`SbiProcess`].
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum SbiProcessState {
    /// The guest can execute.
    Running,
    /// The guest faulted.
    Faulted,
    /// The guest shut itself down.
    Terminated,
}

/// A process whose interface to the kernel is the RISC-V SBI.
pub struct SbiProcess<'a, B: SbiBoundary, H: SbiHandler> {
    processid: ProcessId,
    boundary: &'a B,
    handler: &'a H,
    interrupts: &'a SbiGuestInterrupts,
    stored_state: MapCell<B::StoredState>,
    state: Cell<SbiProcessState>,
    timeslice_expirations: Cell<usize>,
}

impl<'a, B: SbiBoundary, H: SbiHandler> SbiProcess<'a, B, H> {
    pub fn new(
        processid: ProcessId,
        boundary: &'a B,
        handler: &'a H,
        interrupts: &'a SbiGuestInterrupts,
    ) -> Self {
        SbiProcess {
            processid,
            boundary,
            handler,
            interrupts,
            stored_state: MapCell::new(boundary.initial_state()),
            state: Cell::new(SbiProcessState::Running),
            timeslice_expirations: Cell::new(0),
        }
    }

    pub fn state(&self) -> SbiProcessState {
        self.state.get()
    }

    pub fn timeslice_expirations(&self) -> usize {
        self.timeslice_expirations.get()
    }
}

impl<B: SbiBoundary, H: SbiHandler> Schedulable for SbiProcess<'_, B, H> {
    fn processid(&self) -> ProcessId {
        self.processid
    }

    fn ready(&self) -> bool {
        self.state.get() == SbiProcessState::Running
    }

    fn step(&self, hooks: &KernelHooks) -> StepResult {
        if self.state.get() != SbiProcessState::Running {
            return StepResult::NoWorkLeft;
        }

        hooks.arm_scheduler_timer();
        let trap = self.stored_state.map(|state| {
            // ### Safety
            //
            // The boundary is responsible for configuring memory protection for
            // the guest, and `state` is this guest's state.
            unsafe { self.boundary.switch_to_guest(state, self.interrupts) }
        });
        hooks.disarm_scheduler_timer();

        match trap {
            Some(SbiTrap::Ecall { eid, fid, args }) => {
                match self.handler.sbi_call(self.processid, eid, fid, &args) {
                    SbiCallResult::Return(ret) => {
                        self.stored_state
                            .map(|state| self.boundary.set_ecall_return(state, ret));
                        StepResult::Continue
                    }
                    // Do not advance the guest past its `ecall`, so that it
                    // re-issues it.
                    SbiCallResult::Retry => StepResult::Continue,
                    SbiCallResult::Shutdown => {
                        self.state.set(SbiProcessState::Terminated);
                        StepResult::NoWorkLeft
                    }
                }
            }
            Some(SbiTrap::Interrupted) => StepResult::Interrupted,
            Some(SbiTrap::Fault) | None => {
                self.state.set(SbiProcessState::Faulted);
                StepResult::NoWorkLeft
            }
        }
    }

    fn debug_timeslice_expired(&self) {
        self.timeslice_expirations
            .set(self.timeslice_expirations.get() + 1);
    }

    fn as_process(&self) -> Option<&dyn Process> {
        // SBI processes are not Tock processes: they do not have grants,
        // upcalls, or process buffers, and are thus invisible to capsules.
        None
    }
}
