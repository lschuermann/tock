// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use super::Step::Expect;
use super::{Requirement, TESTS, TestCase};
use crate::boards::qemu;

#[linkme::distributed_slice(TESTS)]
static TEST: TestCase = TestCase {
    id: "lua_hello",
    requires: &[Requirement::Uart],
    apps: &["lua-hello"],
    unsupported: &[(
        &qemu::RV64_VIRT,
        "the app runs, but never prints (cause not yet investigated)",
    )],
    steps: &[Expect("Hello from Lua!")],
};
