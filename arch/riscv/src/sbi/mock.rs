// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

//! Mock implementations of [`SbiBoundary`] and [`SbiHandler`].
//!
//! [`MockSbiGuest`] does not execute any code. Instead, every switch to the
//! guest returns the next SBI call of a fixed script: it queries the SBI
//! version, probes for the extensions it uses, prints a greeting using the
//! legacy console extension, and finally shuts itself down.
//!
//! [`MockSbiHandler`] implements the subset of the SBI used by this script.

use kernel::debug;
use kernel::process::ProcessId;
use kernel::utilities::cells::MapCell;

use super::{
    SbiBoundary, SbiCallResult, SbiGuestInterrupts, SbiHandler, SbiRet, SbiTrap, base_fid, eid,
    error,
};

const GREETING: &[u8] = b"Hello from an SBI guest!\n";

/// Saved state of a [`MockSbiGuest`].
#[derive(Default)]
pub struct MockSbiGuestState {
    /// Index of the next SBI call in the script.
    pc: usize,
    /// Return value of the last SBI call.
    last_ret: Option<SbiRet>,
}

/// A mock SBI guest which issues a fixed sequence of SBI calls.
pub struct MockSbiGuest;

impl MockSbiGuest {
    fn ecall(eid: usize, fid: usize, a0: usize) -> SbiTrap {
        SbiTrap::Ecall {
            eid,
            fid,
            args: [a0, 0, 0, 0, 0, 0],
        }
    }
}

impl SbiBoundary for MockSbiGuest {
    type StoredState = MockSbiGuestState;

    fn initial_state(&self) -> MockSbiGuestState {
        MockSbiGuestState::default()
    }

    unsafe fn switch_to_guest(
        &self,
        state: &mut MockSbiGuestState,
        _interrupts: &SbiGuestInterrupts,
    ) -> SbiTrap {
        // The guest checks that the SBI calls it relies on succeeded.
        if let Some(ret) = state.last_ret {
            if ret.error != error::SUCCESS || (state.pc == 2 && ret.value == 0) {
                return SbiTrap::Fault;
            }
        }

        match state.pc {
            0 => Self::ecall(eid::BASE, base_fid::GET_SPEC_VERSION, 0),
            1 => Self::ecall(
                eid::BASE,
                base_fid::PROBE_EXTENSION,
                eid::LEGACY_CONSOLE_PUTCHAR,
            ),
            n if n - 2 < GREETING.len() => {
                Self::ecall(eid::LEGACY_CONSOLE_PUTCHAR, 0, GREETING[n - 2] as usize)
            }
            // System reset: shutdown, no reason.
            _ => Self::ecall(eid::SRST, 0, 0),
        }
    }

    fn set_ecall_return(&self, state: &mut MockSbiGuestState, ret: SbiRet) {
        state.last_ret = Some(ret);
        state.pc += 1;
    }
}

const LINE_LEN: usize = 80;

/// A mock SBI implementation, supporting the base, legacy console putchar,
/// and system reset extensions.
pub struct MockSbiHandler {
    line: MapCell<([u8; LINE_LEN], usize)>,
}

impl MockSbiHandler {
    pub const fn new() -> Self {
        MockSbiHandler {
            line: MapCell::new(([0; LINE_LEN], 0)),
        }
    }

    fn putchar(&self, guest: ProcessId, c: u8) {
        self.line.map(|(buf, len)| {
            if c != b'\n' {
                buf[*len] = c;
                *len += 1;
            }
            if c == b'\n' || *len == LINE_LEN {
                debug!(
                    "[{:?} SBI console] {}",
                    guest,
                    core::str::from_utf8(&buf[..*len]).unwrap_or("<invalid UTF-8>")
                );
                *len = 0;
            }
        });
    }
}

impl SbiHandler for MockSbiHandler {
    fn sbi_call(
        &self,
        guest: ProcessId,
        eid: usize,
        fid: usize,
        args: &[usize; 6],
    ) -> SbiCallResult {
        let ret = match (eid, fid) {
            (eid::BASE, base_fid::GET_SPEC_VERSION) => {
                debug!("[{:?} SBI] get_spec_version", guest);
                // SBI v2.0.
                SbiRet::success(2 << 24)
            }
            (eid::BASE, base_fid::GET_IMPL_ID) => SbiRet::success(0xff),
            (eid::BASE, base_fid::GET_IMPL_VERSION) => SbiRet::success(0),
            (eid::BASE, base_fid::PROBE_EXTENSION) => {
                let available =
                    matches!(args[0], eid::BASE | eid::LEGACY_CONSOLE_PUTCHAR | eid::SRST);
                debug!(
                    "[{:?} SBI] probe_extension({:#x}) = {}",
                    guest, args[0], available
                );
                SbiRet::success(available as usize)
            }
            (eid::LEGACY_CONSOLE_PUTCHAR, _) => {
                self.putchar(guest, args[0] as u8);
                SbiRet::success(0)
            }
            (eid::SRST, 0) => {
                debug!(
                    "[{:?} SBI] system_reset({:#x}, {:#x})",
                    guest, args[0], args[1]
                );
                return SbiCallResult::Shutdown;
            }
            (eid::SRST, _) => SbiRet::error(error::ERR_INVALID_PARAM),
            _ => SbiRet::error(error::ERR_NOT_SUPPORTED),
        };
        SbiCallResult::Return(ret)
    }
}
