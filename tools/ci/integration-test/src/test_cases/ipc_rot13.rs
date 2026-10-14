// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use super::Step::Expect;
use super::{Requirement, TESTS, TestCase};
use crate::boards::esp32_c3;

#[linkme::distributed_slice(TESTS)]
static TEST: TestCase = TestCase {
    id: "ipc_rot13",
    requires: &[Requirement::Uart],
    apps: &["rot13_client", "rot13_service"],
    unsupported: &[(&esp32_c3::BOARD, "the board's kernel has no IPC")],
    steps: &[
        Expect("12: Hello World!"),
        Expect("12: Uryyb Jbeyq!"),
        Expect("12: Hello World!"),
        Expect("12: Uryyb Jbeyq!"),
    ],
};
