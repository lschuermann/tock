// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use super::Step::{Check, Expect, Send};
use super::util::check_process_state;
use super::{Requirement, TESTS, TestCase};

#[linkme::distributed_slice(TESTS)]
static TEST: TestCase = TestCase {
    id: "exit",
    requires: &[Requirement::Uart],
    apps: &["tests/exit"],
    unsupported: &[],
    steps: &[
        Expect("Testing exit."),
        Expect("Exiting."),
        Send("list\r\n"),
        // Wait for the end of the list first, such that the following prompt
        // is the one printed after the list:
        Expect("Grants  State"),
        Expect("tock$ "),
        Check(|out, _| check_process_state(out, "exit", "Terminated")),
    ],
};
