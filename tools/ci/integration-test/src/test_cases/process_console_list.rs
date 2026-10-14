// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use super::Step::{Check, Expect, Send};
use super::util::process_pid;
use super::{Requirement, TESTS, TestCase};

#[linkme::distributed_slice(TESTS)]
static TEST: TestCase = TestCase {
    id: "process_console_list",
    requires: &[Requirement::Uart],
    apps: &["c_hello", "blink"],
    unsupported: &[],
    steps: &[
        Expect("tock$ "),
        Send("list\r\n"),
        Expect("tock$ "),
        Check(|out, _| {
            process_pid(out, "c_hello")
                .and(process_pid(out, "blink"))
                .map(drop)
        }),
    ],
};
