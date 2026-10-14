// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use super::Step::{Check, Expect, Send};
use super::util::check_process_state;
use super::{Requirement, TESTS, TestCase};

#[linkme::distributed_slice(TESTS)]
static TEST: TestCase = TestCase {
    id: "process_console_stop_start",
    requires: &[Requirement::Uart],
    apps: &["tests/whileone"],
    unsupported: &[],
    steps: &[
        Expect("tock$ "),
        Send("list\r\n"),
        Expect("tock$ "),
        Check(|out, _| check_process_state(out, "whileone", "Running")),
        Send("stop whileone\r\n"),
        Expect("tock$ "),
        Send("list\r\n"),
        Expect("tock$ "),
        Check(|out, _| check_process_state(out, "whileone", "Stopped(Running)")),
        Send("start whileone\r\n"),
        Expect("tock$ "),
        Send("list\r\n"),
        Expect("tock$ "),
        Check(|out, _| check_process_state(out, "whileone", "Running")),
    ],
};
