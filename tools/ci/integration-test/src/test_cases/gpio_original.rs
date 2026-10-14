// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use super::Step::{Expect, Within};
use super::util::{GPIO0, check_edge_times};
use super::{Requirement, TESTS, TestCase};
use std::time::Duration;

#[linkme::distributed_slice(TESTS)]
static TEST: TestCase = TestCase {
    id: "gpio_original",
    requires: &[Requirement::Uart, GPIO0],
    apps: &["tests/gpio/gpio_original"],
    unsupported: &[],
    steps: &[
        Expect("Periodically toggling pin"),
        // The app toggles userspace GPIO pin 0 every second.
        Within(Duration::from_secs(10), |ctx| {
            ctx.wait_edge("gpio0", Duration::from_secs(2))?;
            ctx.sleep(Duration::from_millis(500))?;
            let [edges] = ctx.edges(["gpio0"], Duration::from_secs(4))?;
            let expected = (500..4000).step_by(1000).map(Duration::from_millis);
            check_edge_times("gpio0", &edges, expected)
        }),
    ],
};
