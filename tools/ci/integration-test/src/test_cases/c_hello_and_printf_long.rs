// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use super::Step::Within;
use super::{Requirement, TESTS, TestCase};
use anyhow::ensure;
use std::time::Duration;

#[linkme::distributed_slice(TESTS)]
static TEST: TestCase = TestCase {
    id: "c_hello_and_printf_long",
    requires: &[Requirement::Uart],
    apps: &["c_hello", "tests/printf_long"],
    unsupported: &[],
    // The two apps print in either order.
    steps: &[Within(Duration::from_secs(10), |ctx| {
        let out = ctx.expect("And a short message.")?;
        ensure!(
            out.contains(
                "Hi welcome to Tock. This test makes sure that a greater than 64 byte \
                 message can be printed.",
            ),
            "long message missing or not printed before the short one"
        );
        if !out.contains("Hello World!") {
            ctx.expect("Hello World!")?;
        }
        Ok(())
    })],
};
