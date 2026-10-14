// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use super::Step::{Expect, Screen, Sleep};
use super::util::DRAW_TIME;
use super::{Requirement, TESTS, TestCase};
use crate::boards::{Feature, qemu};

#[linkme::distributed_slice(TESTS)]
static TEST: TestCase = TestCase {
    id: "tock_logo",
    requires: &[Requirement::Uart, Requirement::Feature(Feature::Display)],
    apps: &["tests/u8g2-demos/tock-logo"],
    unsupported: &[],
    steps: &[
        Expect("main loop"),
        Sleep(DRAW_TIME),
        Screen(&[(
            &qemu::RV64_VIRT,
            "4150b38c3cf468a388c0a09eb309ac8c480f692599cc0fc3e2684e0fafb5463d",
        )]),
    ],
};
