// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use crate::board::{Gpio, GpioMode};
use crate::hostspec::{GpioDrive, GpioPinSpec};
use gpiocdev::Request;
use gpiocdev::line::{Drive, Value};
use std::path::PathBuf;

/// A line on a Linux GPIO character device (`linux-gpiochip` driver),
/// located by the controller's `config.label` and the pin's `config.offset`.
///
/// Reads and writes use the DUT pin's level: for an `inverted` pin, the host
/// line sits at the opposite level.
pub struct LinuxGpiochip {
    chip_label: String,
    offset: u32,
    modes: Vec<String>,
    inverted: bool,
    drive: Drive,
    request: Option<Request>,
}

impl LinuxGpiochip {
    pub fn new(controller: &serde_json::Value, pin: &GpioPinSpec) -> Result<Self, String> {
        let chip_label = controller
            .get("label")
            .and_then(serde_json::Value::as_str)
            .ok_or("linux-gpiochip controller has no config.label")?
            .to_string();
        let offset = pin
            .config
            .get("offset")
            .and_then(serde_json::Value::as_u64)
            .and_then(|o| u32::try_from(o).ok())
            .ok_or("linux-gpiochip pin has no config.offset")?;
        // Without inversion, the host line itself must only pull the DUT pin
        // the way the wiring allows. With inversion, the host line drives the
        // inverting stage (e.g. a transistor's base), which provides the
        // pin's drive.
        let drive = match (pin.inverted, pin.drive) {
            (true, _) | (false, None | Some(GpioDrive::PushPull)) => Drive::PushPull,
            (false, Some(GpioDrive::OpenDrain)) => Drive::OpenDrain,
            (false, Some(GpioDrive::OpenSource)) => Drive::OpenSource,
            (false, Some(GpioDrive::Unsupported)) => {
                return Err("linux-gpiochip pin has an unsupported drive".into());
            }
        };
        Ok(LinuxGpiochip {
            chip_label,
            offset,
            modes: pin.modes.clone(),
            inverted: pin.inverted,
            drive,
            request: None,
        })
    }

    /// The host line value that puts the DUT pin at `high`.
    fn host_value(&self, high: bool) -> Value {
        if high != self.inverted {
            Value::Active
        } else {
            Value::Inactive
        }
    }

    fn chip_path(&self) -> Result<PathBuf, String> {
        let chips = gpiocdev::chip::chips().map_err(|e| e.to_string())?;
        chips
            .into_iter()
            .find(|path| {
                gpiocdev::chip::Chip::from_path(path)
                    .and_then(|chip| chip.info())
                    .is_ok_and(|info| info.label == self.chip_label)
            })
            .ok_or_else(|| format!("no GPIO chip labeled {:?}", self.chip_label))
    }

    fn request(&self) -> Result<&Request, String> {
        self.request
            .as_ref()
            .ok_or_else(|| format!("line {} is not configured", self.offset))
    }
}

impl Drop for LinuxGpiochip {
    /// Releases the line as a high-impedance input, so it does not keep
    /// driving the DUT after the test that used it.
    fn drop(&mut self) {
        if let Some(request) = &self.request {
            let mut config = request.config();
            config.with_line(self.offset).as_input();
            let _ = request.reconfigure(&config);
        }
    }
}

impl Gpio for LinuxGpiochip {
    fn configure(&mut self, mode: GpioMode) -> Result<(), String> {
        if !self.modes.iter().any(|m| m == mode.as_str()) {
            return Err(format!(
                "{} line {} is not wired for {mode:?} (supports {:?})",
                self.chip_label, self.offset, self.modes
            ));
        }
        // Like a sysfs `out`, switching to an output drives the DUT pin low.
        let low = self.host_value(false);
        let drive = self.drive;
        let apply = |config: &mut gpiocdev::request::Config| {
            match mode {
                GpioMode::DigitalIn => config.as_input(),
                GpioMode::DigitalOut => config.as_output(low).with_drive(drive),
            };
        };
        match &self.request {
            Some(request) => {
                let mut config = request.config();
                apply(config.with_line(self.offset));
                request.reconfigure(&config).map_err(|e| e.to_string())
            }
            None => {
                let mut config = gpiocdev::request::Config::default();
                config.on_chip(self.chip_path()?);
                apply(config.with_line(self.offset));
                let request = Request::from_config(config)
                    .with_consumer("tock-harness")
                    .request()
                    .map_err(|e| e.to_string())?;
                self.request = Some(request);
                Ok(())
            }
        }
    }

    fn drive(&mut self, high: bool) -> Result<(), String> {
        let value = self.host_value(high);
        self.request()?
            .set_value(self.offset, value)
            .map_err(|e| e.to_string())
    }

    fn read(&mut self) -> Result<bool, String> {
        let value = self
            .request()?
            .value(self.offset)
            .map_err(|e| e.to_string())?;
        Ok((value == Value::Active) != self.inverted)
    }
}
