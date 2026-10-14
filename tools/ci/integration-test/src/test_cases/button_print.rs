// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use super::Step::{Expect, Within};
use super::step::SETTLE;
use super::util::BUTTON0;
use super::{Requirement, TESTS, TestCase};
use std::time::Duration;

#[linkme::distributed_slice(TESTS)]
static TEST: TestCase = TestCase {
    id: "button_print",
    requires: &[Requirement::Uart, BUTTON0],
    apps: &["tests/button_print"],
    unsupported: &[],
    steps: &[
        Expect("[TEST] Button Press"),
        Within(Duration::from_secs(10), |ctx| {
            for _ in 0..2 {
                ctx.press("button0")?;
                ctx.expect("Button Press! Button: 0 Status: 0")?;
                ctx.release("button0")?;
                ctx.sleep(SETTLE)?;
            }
            Ok(())
        }),
    ],
};
