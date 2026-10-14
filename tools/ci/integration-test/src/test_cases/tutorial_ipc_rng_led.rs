// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use super::Step::{Expect, Within};
use super::util::{LED0, LED1};
use super::{Requirement, TESTS, TestCase};
use anyhow::bail;
use std::time::Duration;

#[linkme::distributed_slice(TESTS)]
static TEST: TestCase = TestCase {
    id: "tutorial_ipc_rng_led",
    requires: &[Requirement::Uart, LED0, LED1],
    apps: &[
        "tutorials/05_ipc/led",
        "tutorials/05_ipc/rng",
        "tutorials/05_ipc/logic",
    ],
    unsupported: &[],
    steps: &[
        // The logic app queries the LED service for the number of LEDs,
        // then every 500 ms sets a random LED to a random state, using
        // bytes from the RNG service.
        Expect("Number of LEDs: 4"),
        Within(Duration::from_secs(60), |ctx| {
            let edges = ctx.edges_until(["led0", "led1"], Duration::from_secs(60), |edges| {
                edges.iter().all(|pin| !pin.is_empty())
            })?;
            if edges.iter().any(|pin| pin.is_empty()) {
                bail!("not all LEDs changed within 60s: {edges:?}");
            }
            let mut all = edges.concat();
            all.sort();
            if all
                .windows(2)
                .any(|w| w[1] - w[0] < Duration::from_millis(450))
            {
                bail!("LEDs changed less than 500 ms apart: {edges:?}");
            }
            Ok(())
        }),
    ],
};
