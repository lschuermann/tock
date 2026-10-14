// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use super::Step::Expect;
use super::{Requirement, TESTS, TestCase};

#[linkme::distributed_slice(TESTS)]
static TEST: TestCase = TestCase {
    id: "malloc_test01",
    requires: &[Requirement::Uart],
    apps: &["tests/malloc_test01"],
    unsupported: &[],
    steps: &[Expect("malloc01: success")],
};
