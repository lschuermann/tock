// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

pub mod nrf52840dk;
pub mod nucleo_f429zi;
pub mod qemu_virt;
pub mod serial_uart;
pub mod sysfs_gpio;

use crate::board::Gpio;
use crate::hostspec::GpioPinSpec;
use sysfs_gpio::SysfsGpio;

/// Opens the host-side driver for a host-spec GPIO entry, or returns `None`
/// if this harness does not support the entry's type.
pub fn open_gpio(spec: &GpioPinSpec) -> Option<Box<dyn Gpio>> {
    match spec {
        GpioPinSpec::Sysfs(spec) => Some(Box::new(SysfsGpio::new(spec.clone()))),
        GpioPinSpec::Unsupported => None,
    }
}
