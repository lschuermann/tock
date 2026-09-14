// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use crate::board::{Gpio, GpioMode};
use serde::Deserialize;
use std::path::Path;
use std::time::{Duration, Instant};

/// Host-spec GPIO entry of type `sysfs-gpio`.
#[derive(Debug, Clone, Deserialize)]
pub struct SysfsGpioSpec {
    /// Line number under `/sys/class/gpio`.
    #[serde(rename = "gpioN")]
    pub gpio_n: u32,
    /// Modes the line's wiring supports.
    pub modes: Vec<GpioMode>,
}

pub struct SysfsGpio {
    spec: SysfsGpioSpec,
    exported: bool,
}

impl SysfsGpio {
    pub fn new(spec: SysfsGpioSpec) -> Self {
        SysfsGpio {
            spec,
            exported: false,
        }
    }

    fn path(&self) -> String {
        format!("/sys/class/gpio/gpio{}", self.spec.gpio_n)
    }

    fn export(&mut self) -> Result<(), String> {
        if self.exported {
            return Ok(());
        }
        if !Path::new(&self.path()).exists() {
            std::fs::write("/sys/class/gpio/export", self.spec.gpio_n.to_string())
                .map_err(|e| e.to_string())?;
        }
        self.exported = true;

        // A freshly exported line only becomes writable for non-root users
        // once udev has adjusted its permissions.
        let direction = format!("{}/direction", self.path());
        let start = Instant::now();
        while let Err(e) = std::fs::OpenOptions::new().write(true).open(&direction) {
            if start.elapsed() > Duration::from_secs(2) {
                return Err(format!("{direction} did not become writable: {e}"));
            }
            std::thread::sleep(Duration::from_millis(10));
        }
        Ok(())
    }
}

impl Drop for SysfsGpio {
    /// Releases the line as a high-impedance input, so it does not keep
    /// driving the DUT after the test that used it.
    fn drop(&mut self) {
        if self.exported {
            let _ = std::fs::write(format!("{}/direction", self.path()), "in");
            let _ = std::fs::write("/sys/class/gpio/unexport", self.spec.gpio_n.to_string());
        }
    }
}

impl Gpio for SysfsGpio {
    fn set_mode(&mut self, mode: GpioMode) -> Result<(), String> {
        if !self.spec.modes.contains(&mode) {
            return Err(format!(
                "gpio{} is not wired for {mode:?} (supports {:?})",
                self.spec.gpio_n, self.spec.modes
            ));
        }
        self.export()?;
        let value = match mode {
            GpioMode::DigitalOut => "out",
            GpioMode::DigitalIn => "in",
        };
        std::fs::write(format!("{}/direction", self.path()), value).map_err(|e| e.to_string())
    }

    fn write(&mut self, high: bool) -> Result<(), String> {
        std::fs::write(
            format!("{}/value", self.path()),
            if high { "1" } else { "0" },
        )
        .map_err(|e| e.to_string())
    }

    fn read(&mut self) -> Result<bool, String> {
        let v =
            std::fs::read_to_string(format!("{}/value", self.path())).map_err(|e| e.to_string())?;
        Ok(v.trim() == "1")
    }
}
