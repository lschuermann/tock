// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use super::Step::{Expect, Screen, Sleep};
use super::util::DRAW_TIME;
use super::{Requirement, TESTS, TestCase};
use crate::boards::{Feature, qemu};

#[linkme::distributed_slice(TESTS)]
static TEST: TestCase = TestCase {
    id: "led_odd",
    requires: &[Requirement::Uart, Requirement::Feature(Feature::Display)],
    apps: &["tests/led/led-odd"],
    unsupported: &[],
    steps: &[
        Expect("main loop"),
        Sleep(DRAW_TIME),
        Screen(&[(
            &qemu::RV64_VIRT,
            "c4d49a185592f23074ae5b83caa7d506967fbe8f93a547fa60de93bc2ddd282f",
        )]),
    ],
};
