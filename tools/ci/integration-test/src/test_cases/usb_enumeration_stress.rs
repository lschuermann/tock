// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use super::Step::{Expect, Repeat, Reset, Send, Steps};
use super::{Requirement, Step, TESTS, TestCase};
use crate::boards::Feature;

/// Stress test of the enumeration of the board's USB device.
///
/// The test restarts the board many times. Each time, it checks that the USB
/// device enumerates, and that its CDC-ACM console works in both directions.
/// The console must print the output of the app, which only prints once at
/// boot, and must answer a process console command. Enumeration of the USB
/// device of the RP2040 used to fail intermittently (with a timeout, or a
/// truncated configuration descriptor). A failing enumeration may also bring
/// down the USB controller of the host.
#[linkme::distributed_slice(TESTS)]
static TEST: TestCase = TestCase {
    id: "usb_enumeration_stress",
    requires: &[Requirement::Uart, Requirement::Feature(Feature::UsbConsole)],
    apps: &["c_hello"],
    unsupported: &[],
    steps: &[CHECK_CONSOLE, Repeat(50, &Steps(&[Reset, CHECK_CONSOLE]))],
};

/// Check that the console prints the app's output and answers a command.
const CHECK_CONSOLE: Step = Steps(&[Expect("Hello World!"), Send("list\r\n"), Expect("c_hello")]);
