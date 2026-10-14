// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use super::Step::{Check, Discard, Expect, Send};
use super::{Requirement, TESTS, TestCase};
use crate::boards::{Board, Feature};
use anyhow::{Context, Result, bail};

/// Regression test for <https://github.com/tock/tock/pull/5194>.
///
/// The ARMv7-M HardFault handler must clear the sticky CFSR and HFSR bits which
/// it reports. Otherwise, the bits set by a process fault (here, a stack
/// overflow) leak into the report of a later kernel HardFault. A stale
/// BFSR.STKERR bit then makes the kernel misreport the HardFault as a kernel
/// stack overflow.
#[linkme::distributed_slice(TESTS)]
static TEST: TestCase = TestCase {
    id: "app_stack_overflow_then_kernel_hardfault",
    requires: &[
        Requirement::Uart,
        Requirement::Feature(Feature::ArmV7mHardFault),
    ],
    apps: &["tests/mpu/unit/mpu_stack_growth"],
    unsupported: &[],
    steps: &[
        Expect("This test should recursively add stack frames"),
        Expect("mpu_stack_growth faulted and was stopped."),
        Discard,
        // The `hardfault` command executes an undefined instruction in the
        // kernel, which should only set CFSR.UNDEFINSTR:
        Send("hardfault\r\n"),
        Expect("\tKernel version "),
        Check(check_kernel_hardfault),
    ],
};

/// Check the report of a kernel HardFault in `report`.
///
/// The HardFault handler panics in handler mode, in which the board's
/// `PANIC_RESOURCES` (a `SingleThreadValue` bound to thread mode) are not
/// accessible. Thus, the panic does not print the chip state (the "Cortex-M
/// Fault Status" of the process) or the process list. This only checks the
/// panic message, which ends with the kernel version.
fn check_kernel_hardfault(report: &str, _: &Board) -> Result<()> {
    if report.contains("kernel stack overflow") {
        bail!("kernel HardFault was reported as a kernel stack overflow");
    }
    if !report.contains("Kernel HardFault.") {
        bail!("no kernel HardFault report in {report:?}");
    }
    let cfsr = (report.lines())
        .find_map(|line| line.trim().strip_prefix("CFSR  0x"))
        .and_then(|hex| u32::from_str_radix(hex.trim(), 16).ok())
        .with_context(|| format!("no CFSR in kernel HardFault report {report:?}"))?;
    const UNDEFINSTR: u32 = 1 << 16;
    if cfsr != UNDEFINSTR {
        bail!(
            "kernel HardFault CFSR is {cfsr:#010x}, expected only UNDEFINSTR \
             ({UNDEFINSTR:#010x}), as bits from the process fault leaked"
        );
    }
    Ok(())
}
