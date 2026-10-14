// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use crate::boards::{Gpio, GpioMode};
use crate::host_spec::{GpioActive, GpioDrive, GpioPinSpec};
use anyhow::{Context, Result, bail};
use gpiocdev::Request;
use gpiocdev::line::{Drive, Value};
use std::path::PathBuf;

/// Line of a Linux GPIO character device (the `linux-gpiochip` driver).
///
/// The line is located by the `config.label` of its controller and the
/// `config.offset` of the pin. Reads and writes use the level of the DUT pin.
/// For an `inverted` pin, the host line is at the opposite level.
pub struct LinuxGpiochip {
    chip_label: String,
    offset: u32,
    modes: Vec<String>,
    inverted: bool,
    drive: Drive,
    active_high: bool,
    request: Option<Request>,
}

impl LinuxGpiochip {
    /// Construct the line of `pin` on the controller with the configuration
    /// `controller`. The line is requested once it is first configured.
    pub fn new(controller: &serde_json::Value, pin: &GpioPinSpec) -> Result<Self> {
        let chip_label = controller
            .get("label")
            .and_then(serde_json::Value::as_str)
            .context("linux-gpiochip controller has no config.label")?
            .to_string();
        let offset = pin
            .config
            .get("offset")
            .and_then(serde_json::Value::as_u64)
            .and_then(|o| u32::try_from(o).ok())
            .context("linux-gpiochip pin has no config.offset")?;
        // Without inversion, the host line must only drive the DUT pin as the
        // wiring allows. With inversion, the host line drives the inverting
        // stage (e.g., the base of a transistor), which provides the drive of
        // the pin:
        let drive = match (pin.inverted, pin.drive) {
            (true, _) | (false, None | Some(GpioDrive::PushPull)) => Drive::PushPull,
            (false, Some(GpioDrive::OpenDrain)) => Drive::OpenDrain,
            (false, Some(GpioDrive::OpenSource)) => Drive::OpenSource,
            (false, Some(GpioDrive::Unsupported)) => {
                bail!("linux-gpiochip pin has an unsupported drive");
            }
        };
        Ok(LinuxGpiochip {
            chip_label,
            offset,
            modes: pin.modes.clone(),
            inverted: pin.inverted,
            drive,
            active_high: pin.active == Some(GpioActive::High),
            request: None,
        })
    }

    /// Return the value of the host line which puts the DUT pin at `high`.
    fn host_value(&self, high: bool) -> Value {
        if high != self.inverted {
            Value::Active
        } else {
            Value::Inactive
        }
    }

    /// Return the path of the GPIO character device with the controller's
    /// label.
    fn chip_path(&self) -> Result<PathBuf> {
        let chips = gpiocdev::chip::chips()?;
        chips
            .into_iter()
            .find(|path| {
                gpiocdev::chip::Chip::from_path(path)
                    .and_then(|chip| chip.info())
                    .is_ok_and(|info| info.label == self.chip_label)
            })
            .with_context(|| format!("no GPIO chip labeled {:?}", self.chip_label))
    }

    /// Return the request of the line, which exists once the line is
    /// configured.
    fn request(&self) -> Result<&Request> {
        self.request
            .as_ref()
            .with_context(|| format!("line {} is not configured", self.offset))
    }
}

impl Drop for LinuxGpiochip {
    /// Release the line as a high-impedance input, such that it stops driving
    /// the DUT after the test which used it.
    fn drop(&mut self) {
        if let Some(request) = &self.request {
            let mut config = request.config();
            config.with_line(self.offset).as_input();
            let _ = request.reconfigure(&config);
        }
    }
}

impl Gpio for LinuxGpiochip {
    fn configure(&mut self, mode: GpioMode) -> Result<()> {
        if !self.modes.iter().any(|m| m == mode.as_str()) {
            bail!(
                "{} line {} is not wired for {mode:?} (supports {:?})",
                self.chip_label,
                self.offset,
                self.modes
            );
        }
        // Switching to an output drives the DUT pin low, like a sysfs `out`:
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
                Ok(request.reconfigure(&config)?)
            }
            None => {
                let mut config = gpiocdev::request::Config::default();
                config.on_chip(self.chip_path()?);
                apply(config.with_line(self.offset));
                let request = Request::from_config(config)
                    .with_consumer("tock-integration-test")
                    .request()?;
                self.request = Some(request);
                Ok(())
            }
        }
    }

    fn drive(&mut self, high: bool) -> Result<()> {
        let value = self.host_value(high);
        self.request()?.set_value(self.offset, value)?;
        Ok(())
    }

    fn read(&mut self) -> Result<bool> {
        let value = self.request()?.value(self.offset)?;
        Ok((value == Value::Active) != self.inverted)
    }

    fn active_high(&self) -> bool {
        self.active_high
    }
}
