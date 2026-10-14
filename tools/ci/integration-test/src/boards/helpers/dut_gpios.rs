// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use super::gpiochip::LinuxGpiochip;
use crate::boards::{Board, DutConfig, Gpio};
use crate::host_spec::GpioPinSpec;
use anyhow::Result;
use std::collections::BTreeMap;

/// Host GPIOs which are wired to a DUT, by logical name.
///
/// Each GPIO is opened when it is first used.
#[derive(Default)]
pub struct DutGpios(BTreeMap<String, Box<dyn Gpio>>);

impl DutGpios {
    /// Return the host GPIO wired to the GPIO of `dut`, a `board`, with the
    /// logical name `name`, or `None` if there is no such GPIO, or it fails to
    /// open.
    pub fn get(&mut self, board: &Board, dut: &DutConfig, name: &str) -> Option<&mut dyn Gpio> {
        let pin_name = board.pin(name)?;
        if !self.0.contains_key(name) {
            let pin = match open_gpio(dut.gpio.get(pin_name)?)? {
                Ok(pin) => pin,
                Err(e) => {
                    log::warn!("failed to open GPIO {pin_name}: {e}");
                    return None;
                }
            };
            self.0.insert(name.to_string(), pin);
        }
        Some(self.0.get_mut(name)?.as_mut())
    }

    /// Close all GPIOs, which returns their lines to inputs.
    pub fn close(&mut self) {
        self.0.clear();
    }
}

/// Open the host GPIO wired to `pin`, or return `None` if the harness does not
/// support the driver of the pin's controller.
pub fn open_gpio(pin: &GpioPinSpec) -> Option<Result<Box<dyn Gpio>>> {
    let controller = pin.controller_spec.as_ref()?;
    match controller.driver.as_str() {
        "linux-gpiochip" => Some(
            LinuxGpiochip::new(&controller.config, pin).map(|gpio| Box::new(gpio) as Box<dyn Gpio>),
        ),
        _ => None,
    }
}
