// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use crate::board::GpioMode;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Requirement {
    Uart,
    Gpio { name: &'static str, mode: GpioMode },
}
