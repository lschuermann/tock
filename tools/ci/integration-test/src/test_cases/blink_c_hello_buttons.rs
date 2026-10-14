// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use super::Step::{Expect, Within};
use super::blink::CHECK_BLINK_APP_LED_PATTERNS;
use super::step::SETTLE;
use super::util::{BUTTON0, BUTTON1, LED0, LED1};
use super::{Requirement, TESTS, TestCase};
use anyhow::bail;
use std::time::Duration;

#[linkme::distributed_slice(TESTS)]
static TEST: TestCase = TestCase {
    id: "blink_c_hello_buttons",
    requires: &[Requirement::Uart, LED0, LED1, BUTTON0, BUTTON1],
    apps: &["blink", "c_hello", "buttons"],
    unsupported: &[],
    steps: &[
        Expect("Hello World!"),
        CHECK_BLINK_APP_LED_PATTERNS,
        // Pressing a button makes the buttons app toggle the button's LED.
        // Press halfway between two blink steps, such that blink does not
        // change the LEDs at the same time:
        Within(Duration::from_secs(5), |ctx| {
            for (button, leds) in [("button0", ["led0", "led1"]), ("button1", ["led1", "led0"])] {
                ctx.wait_edge("led0", Duration::from_secs(1))?;
                ctx.sleep(Duration::from_millis(100))?;
                let before = ctx.read_all(leds)?;
                ctx.press(button)?;
                ctx.sleep(SETTLE)?;
                let after = ctx.read_all(leds)?;
                ctx.release(button)?;
                if after != [!before[0], before[1]] {
                    bail!(
                        "pressing {button} changed {leds:?} from {before:?} to {after:?}, \
                         expected only {} to toggle",
                        leds[0]
                    );
                }
            }
            Ok(())
        }),
    ],
};
