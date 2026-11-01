// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

//! A basic SBI implementation for a single-hart guest, such as Linux.
//!
//! [`BasicSbi`] implements the SBI extensions a guest needs to boot and run:
//!
//! - **Base:** version and extension probing.
//! - **Timer (`TIME`):** backed by a Tock [`Alarm`]. When it expires, a
//!   supervisor timer interrupt is raised for the guest.
//! - **Legacy console putchar / getchar:** output goes through a Tock UART
//!   (e.g., a device on the console's UART mux). When the output buffer is
//!   full, the guest re-issues the call later. Input is not supported yet.
//! - **IPI, HSM:** trivial single-hart implementations.
//! - **System reset (`SRST`):** shuts down the guest (not the system).
//!
//! The guest's `time` CSR and the [`Alarm`] must count the same ticks (on
//! QEMU's `virt` machine, both are `mtime`).

use kernel::ErrorCode;
use kernel::debug;
use kernel::hil::time::{Alarm, AlarmClient, Ticks, Ticks64};
use kernel::hil::uart;
use kernel::process::ProcessId;
use kernel::utilities::cells::{MapCell, TakeCell};

use super::{SbiCallResult, SbiGuestInterrupts, SbiHandler, SbiRet, base_fid, eid, error, sip};

/// Size of the buffer for console output which has not yet been passed to the
/// UART.
const CONSOLE_BUFFER_LEN: usize = 256;

/// SBI implementation ID reported to the guest. Not an officially assigned
/// ID.
const IMPL_ID: usize = 0x70c4;

/// HSM hart state `STARTED`.
const HSM_STATE_STARTED: usize = 0;

pub struct BasicSbi<'a, A: Alarm<'a, Ticks = Ticks64>, U: uart::Transmit<'a>> {
    interrupts: &'a SbiGuestInterrupts,
    alarm: &'a A,
    uart: &'a U,
    /// Buffer handed to the UART. `None` while a transmission is in progress.
    tx_buffer: TakeCell<'static, [u8]>,
    /// Console output not yet handed to the UART.
    pending: MapCell<([u8; CONSOLE_BUFFER_LEN], usize)>,
}

impl<'a, A: Alarm<'a, Ticks = Ticks64>, U: uart::Transmit<'a>> BasicSbi<'a, A, U> {
    pub fn new(
        interrupts: &'a SbiGuestInterrupts,
        alarm: &'a A,
        uart: &'a U,
        tx_buffer: &'static mut [u8],
    ) -> Self {
        BasicSbi {
            interrupts,
            alarm,
            uart,
            tx_buffer: TakeCell::new(tx_buffer),
            pending: MapCell::new(([0; CONSOLE_BUFFER_LEN], 0)),
        }
    }

    fn set_timer(&self, stime_value: u64) -> SbiRet {
        // Per the SBI specification, this clears the pending timer interrupt.
        self.interrupts.clear(sip::STIP);
        let now = self.alarm.now().into_u64();
        if stime_value <= now {
            self.interrupts.set(sip::STIP);
            let _ = self.alarm.disarm();
        } else {
            self.alarm
                .set_alarm(Ticks64::from(now), Ticks64::from_or_max(stime_value - now));
        }
        SbiRet::success(0)
    }

    fn putchar(&self, c: u8) -> SbiCallResult {
        let queued = self
            .pending
            .map(|(buf, len)| {
                if *len < buf.len() {
                    buf[*len] = c;
                    *len += 1;
                    true
                } else {
                    false
                }
            })
            .unwrap_or(false);
        self.flush();
        if queued {
            // Legacy extensions return their result in `a0`.
            SbiCallResult::Return(SbiRet::success(0))
        } else {
            SbiCallResult::Retry
        }
    }

    /// Hand pending console output to the UART, if it is idle.
    fn flush(&self) {
        let Some(tx_buffer) = self.tx_buffer.take() else {
            return;
        };
        let len = self
            .pending
            .map(|(buf, len)| {
                let n = (*len).min(tx_buffer.len());
                tx_buffer[..n].copy_from_slice(&buf[..n]);
                buf.copy_within(n..*len, 0);
                *len -= n;
                n
            })
            .unwrap_or(0);
        if len == 0 {
            self.tx_buffer.replace(tx_buffer);
        } else if let Err((_, tx_buffer)) = self.uart.transmit_buffer(tx_buffer, len) {
            self.tx_buffer.replace(tx_buffer);
        }
    }

    fn probe_extension(extension: usize) -> usize {
        matches!(
            extension,
            eid::BASE
                | eid::TIME
                | eid::IPI
                | eid::HSM
                | eid::SRST
                | eid::LEGACY_CONSOLE_PUTCHAR
                | eid::LEGACY_CONSOLE_GETCHAR
        ) as usize
    }
}

impl<'a, A: Alarm<'a, Ticks = Ticks64>, U: uart::Transmit<'a>> SbiHandler for BasicSbi<'a, A, U> {
    fn sbi_call(
        &self,
        guest: ProcessId,
        eid: usize,
        fid: usize,
        args: &[usize; 6],
    ) -> SbiCallResult {
        let ret = match (eid, fid) {
            // SBI v2.0
            (eid::BASE, base_fid::GET_SPEC_VERSION) => SbiRet::success(2 << 24),
            (eid::BASE, base_fid::GET_IMPL_ID) => SbiRet::success(IMPL_ID),
            (eid::BASE, base_fid::GET_IMPL_VERSION) => SbiRet::success(0),
            (eid::BASE, base_fid::PROBE_EXTENSION) => {
                SbiRet::success(Self::probe_extension(args[0]))
            }
            (eid::BASE, base_fid::GET_MVENDORID | base_fid::GET_MARCHID | base_fid::GET_MIMPID) => {
                SbiRet::success(0)
            }

            (eid::TIME, 0) => self.set_timer(args[0] as u64),

            (eid::LEGACY_CONSOLE_PUTCHAR, _) => return self.putchar(args[0] as u8),
            // No input yet: return -1 in `a0`.
            (eid::LEGACY_CONSOLE_GETCHAR, _) => SbiRet::error(-1),

            // send_ipi(hart_mask, hart_mask_base): the only hart is 0.
            (eid::IPI, 0) => {
                let (mask, base) = (args[0], args[1]);
                if base == usize::MAX || (base == 0 && mask & 1 != 0) {
                    self.interrupts.set(sip::SSIP);
                }
                SbiRet::success(0)
            }

            // hart_get_status(hartid)
            (eid::HSM, 2) if args[0] == 0 => SbiRet::success(HSM_STATE_STARTED),
            (eid::HSM, 2) => SbiRet::error(error::ERR_INVALID_PARAM),

            (eid::SRST, 0) => {
                debug!(
                    "[{:?}] SBI guest requested system reset ({:#x}, {:#x}), stopping it",
                    guest, args[0], args[1]
                );
                return SbiCallResult::Shutdown;
            }

            _ => SbiRet::error(error::ERR_NOT_SUPPORTED),
        };
        SbiCallResult::Return(ret)
    }
}

impl<'a, A: Alarm<'a, Ticks = Ticks64>, U: uart::Transmit<'a>> AlarmClient for BasicSbi<'a, A, U> {
    fn alarm(&self) {
        self.interrupts.set(sip::STIP);
    }
}

impl<'a, A: Alarm<'a, Ticks = Ticks64>, U: uart::Transmit<'a>> uart::TransmitClient
    for BasicSbi<'a, A, U>
{
    fn transmitted_buffer(
        &self,
        tx_buffer: &'static mut [u8],
        _tx_len: usize,
        _rval: Result<(), ErrorCode>,
    ) {
        self.tx_buffer.replace(tx_buffer);
        self.flush();
    }
}
