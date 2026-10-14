// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use super::Step::{Check, Expect, Send};
use super::blink::CHECK_BLINK_APP_LED_PATTERNS;
use super::util::{LED0, LED1, check_process_state};
use super::{Requirement, TESTS, TestCase};

#[linkme::distributed_slice(TESTS)]
static TEST: TestCase = TestCase {
    id: "scheduler_whileone_blink",
    requires: &[Requirement::Uart, LED0, LED1],
    apps: &["tests/whileone", "blink"],
    unsupported: &[],
    // blink must keep its timing even though whileone never yields.
    steps: &[
        Expect("tock$ "),
        Send("list\r\n"),
        Expect("tock$ "),
        Check(|out, _| check_process_state(out, "whileone", "Running")),
        CHECK_BLINK_APP_LED_PATTERNS,
    ],
};
