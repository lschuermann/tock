// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use super::Step::{Expect, Send};
use super::{Requirement, TESTS, TestCase};

#[linkme::distributed_slice(TESTS)]
static TEST: TestCase = TestCase {
    id: "process_console_help",
    requires: &[Requirement::Uart],
    apps: &[],
    unsupported: &[],
    steps: &[
        Expect("tock$ "),
        Send("help\r\n"),
        Expect(
            "Valid commands are: help status list stop start fault boot terminate process \
             kernel reset panic console-start console-stop",
        ),
    ],
};
