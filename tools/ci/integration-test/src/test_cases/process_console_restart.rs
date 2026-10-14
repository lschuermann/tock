// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use super::Step::{Expect, Within};
use super::util::process_pid;
use super::{Ctx, Requirement, TESTS, TestCase};
use anyhow::{Result, ensure};
use std::time::Duration;

#[linkme::distributed_slice(TESTS)]
static TEST: TestCase = TestCase {
    id: "process_console_restart",
    requires: &[Requirement::Uart],
    apps: &["tests/whileone"],
    unsupported: &[],
    steps: &[
        Expect("tock$ "),
        Within(Duration::from_secs(20), |ctx| {
            let before = whileone_pid(ctx)?;
            ctx.send("terminate whileone\r\n")?;
            ctx.expect("tock$ ")?;
            ctx.send("boot whileone\r\n")?;
            ctx.expect("tock$ ")?;
            let after = whileone_pid(ctx)?;
            ensure!(after > before, "PID did not increase: {before} -> {after}");
            Ok(())
        }),
    ],
};

fn whileone_pid(ctx: &mut Ctx) -> Result<u32> {
    ctx.send("list\r\n")?;
    process_pid(&ctx.expect("tock$ ")?, "whileone")
}
