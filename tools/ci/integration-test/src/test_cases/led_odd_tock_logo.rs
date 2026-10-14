// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use super::Step::{Expect, Screen, Sleep};
use super::util::DRAW_TIME;
use super::{Requirement, TESTS, TestCase};
use crate::boards::{Feature, qemu};

#[linkme::distributed_slice(TESTS)]
static TEST: TestCase = TestCase {
    id: "led_odd_tock_logo",
    requires: &[Requirement::Uart, Requirement::Feature(Feature::Display)],
    apps: &["tests/led/led-odd", "tests/u8g2-demos/tock-logo"],
    unsupported: &[],
    steps: &[
        Expect("main loop"),
        Sleep(DRAW_TIME),
        Screen(&[(
            &qemu::RV64_VIRT,
            "bfec0b5a3c5b72edb6128c7c9b6690b56d7ac93e1047d9ef45a22be0c9b4d4fa",
        )]),
    ],
};
