// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use super::Step::Expect;
use super::{Requirement, TESTS, TestCase};

#[linkme::distributed_slice(TESTS)]
static TEST: TestCase = TestCase {
    id: "malloc_test02",
    requires: &[Requirement::Uart],
    apps: &["tests/malloc_test02"],
    unsupported: &[],
    steps: &[Expect("malloc02: success")],
};
