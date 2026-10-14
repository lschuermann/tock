// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use super::Step::{Expect, Send, Sleep};
use super::{Requirement, TESTS, TestCase};
use std::time::Duration;

#[linkme::distributed_slice(TESTS)]
static TEST: TestCase = TestCase {
    id: "console",
    requires: &[Requirement::Uart],
    apps: &["tests/console/console"],
    unsupported: &[],
    steps: &[
        // The kernel prints this right before running the app, which then
        // waits for input without announcing it.
        Expect("main loop"),
        Sleep(Duration::from_millis(100)),
        Send("a"),
        Expect("Got character: 'a'"),
        Send("R"),
        Expect("Got character: 'R'"),
    ],
};
