// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use super::Step::Within;
use super::util::{LED0, LED1, check_edge_times};
use super::{TESTS, TestCase};
use anyhow::ensure;
use std::time::Duration;

#[linkme::distributed_slice(TESTS)]
static TEST: TestCase = TestCase {
    id: "multi_alarm_test",
    requires: &[LED0, LED1],
    apps: &["tests/alarms/multi_alarm_test"],
    unsupported: &[],
    steps: &[Within(Duration::from_secs(30), |ctx| {
        // Each of the board's four LEDs lights up for 300 ms every 4 s, one
        // second after the previous LED. Synchronize to the end of a `led0`
        // pulse, which is the only case of two `led0` edges less than 1 s
        // apart. At most two edges after any first edge end a pulse. Allow one
        // more edge before giving up on an LED which never pulses:
        ctx.wait_edge("led0", Duration::from_secs(6))?;
        let mut synced = false;
        for _ in 0..3 {
            if ctx.wait_edge("led0", Duration::from_secs(5))? <= Duration::from_secs(1) {
                synced = true;
                break;
            }
        }
        ensure!(
            synced,
            "led0 did not pulse: none of 3 edges followed the previous one within 1 s"
        );
        let [led0, led1] = ctx.edges(["led0", "led1"], Duration::from_millis(8350))?;
        check_edge_times(
            "led0",
            &led0,
            [3700, 4000, 7700, 8000].map(Duration::from_millis),
        )?;
        check_edge_times(
            "led1",
            &led1,
            [700, 1000, 4700, 5000].map(Duration::from_millis),
        )
    })],
};
