// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use super::Step::Within;
use super::step::SETTLE;
use super::util::{BUTTON0, BUTTON1, LED0, LED1};
use super::{TESTS, TestCase};
use anyhow::bail;
use std::time::Duration;

#[linkme::distributed_slice(TESTS)]
static TEST: TestCase = TestCase {
    id: "buttons",
    requires: &[LED0, LED1, BUTTON0, BUTTON1],
    apps: &["buttons"],
    unsupported: &[],
    steps: &[Within(Duration::from_secs(10), |ctx| {
        // The app does not announce when it is ready. Thus, keep pressing
        // `button0` until it responds:
        ctx.retry(Duration::from_secs(3), |ctx| {
            let before = ctx.read("led0")?;
            ctx.click("button0")?;
            Ok(ctx.read("led0")? != before)
        })?;

        // Each press toggles the button's own LED, and releasing the button
        // has no effect:
        for (button, leds) in [("button0", ["led0", "led1"]), ("button1", ["led1", "led0"])] {
            for _ in 0..2 {
                let before = ctx.read_all(leds)?;
                ctx.press(button)?;
                ctx.sleep(SETTLE)?;
                let pressed = ctx.read_all(leds)?;
                ctx.release(button)?;
                ctx.sleep(SETTLE)?;
                let released = ctx.read_all(leds)?;
                if pressed != [!before[0], before[1]] || released != pressed {
                    bail!(
                        "{button}: {leds:?} went from {before:?} to {pressed:?} on press \
                         and {released:?} on release, expected only {} to toggle on press",
                        leds[0]
                    );
                }
            }
        }
        Ok(())
    })],
};
