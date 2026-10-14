// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use super::Step::Within;
use super::util::{EDGE_TOLERANCE, LED0, LED1, check_edge_times, millis};
use super::{Step, TESTS, TestCase};
use anyhow::bail;
use std::time::Duration;

#[linkme::distributed_slice(TESTS)]
static TEST: TestCase = TestCase {
    id: "blink",
    requires: &[LED0, LED1],
    apps: &["blink"],
    unsupported: &[],
    steps: &[CHECK_BLINK_APP_LED_PATTERNS],
};

/// Check the LED pattern of the blink app.
///
/// The app shows a binary counter on the LEDs, which advances every 250 ms.
/// Thus, `led0` toggles on every step, and `led1` on every other step.
pub const CHECK_BLINK_APP_LED_PATTERNS: Step = Within(Duration::from_secs(5), |ctx| {
    // Start recording halfway between two steps, such that no edge coincides
    // with the start:
    ctx.wait_edge("led0", Duration::from_secs(1))?;
    ctx.sleep(Duration::from_millis(125))?;
    let [led0, led1] = ctx.edges(["led0", "led1"], Duration::from_millis(3750))?;

    // Each step waits 250 ms after the previous one, such that scheduling
    // latency accumulates across steps. Thus, check the length of each step:
    let step = Duration::from_millis(250);
    let uneven = (led0.windows(2)).any(|w| (w[1] - w[0]).abs_diff(step) > EDGE_TOLERANCE);
    if led0.len() < 14 || uneven {
        bail!(
            "led0: expected a toggle every 250 ms, observed edges at {:?} ms",
            millis(&led0)
        );
    }
    let skip = match led1.first() {
        Some(&t) if t.abs_diff(led0[0]) <= EDGE_TOLERANCE => 0,
        _ => 1,
    };
    check_edge_times("led1", &led1, led0.iter().copied().skip(skip).step_by(2))
});
