// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use super::Step::{Expect, Send, Sleep};
use super::{Requirement, TESTS, TestCase};
use crate::boards::qemu;
use std::time::Duration;

#[linkme::distributed_slice(TESTS)]
static TEST: TestCase = TestCase {
    id: "console_timeout",
    requires: &[Requirement::Uart],
    apps: &["tests/console/console_timeout"],
    unsupported: &[(
        &qemu::RV64_VIRT,
        "with the app, the console stops taking input (cause not yet investigated)",
    )],
    steps: &[
        Expect("tock$ "),
        Send("Hello, Tock!"),
        // The app aborts its read 5 s after it starts, and only then prints
        // what it read.
        Sleep(Duration::from_secs(5)),
        Expect("Userspace call to read console returned: Hello, Tock!"),
    ],
};
