// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use super::Step::Within;
use super::util::{BUTTON0, check_mpu_walk_region_overrun_fault};
use super::{Requirement, TESTS, TestCase};
use crate::boards::qemu;
use std::time::Duration;

#[linkme::distributed_slice(TESTS)]
static TEST: TestCase = TestCase {
    id: "mpu_walk_region_flash",
    requires: &[Requirement::Uart, BUTTON0],
    apps: &["tests/mpu/mpu_walk_region"],
    unsupported: &[(&qemu::RV64_VIRT, "checks for a Cortex-M fault report")],
    steps: &[Within(Duration::from_secs(20), |ctx| {
        check_mpu_walk_region_overrun_fault(ctx, "memory", "flash")
    })],
};
