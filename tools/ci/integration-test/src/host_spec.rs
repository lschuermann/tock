// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

//! Subset of a v2 host spec which the harness uses.
//!
//! Unknown fields are ignored, as are unknown console kinds and GPIO drivers,
//! such that additions to the schema do not break the harness.

use crate::boards::{AttachedDut, Board, DutConfig};
use anyhow::{Context, Result, bail};
use fs_err as fs;
use serde::Deserialize;
use std::collections::BTreeMap;
use std::path::Path;

/// Description of a host and the DUTs attached to it.
#[derive(Debug, Clone, Deserialize)]
pub struct HostSpec {
    pub spec_version: String,
    /// Name of the host (e.g., `pton-rpi2003`).
    #[serde(default)]
    pub name: Option<String>,
    #[serde(default)]
    pub platform: Option<PlatformSpec>,
    #[serde(default)]
    pub gpio_controllers: BTreeMap<String, GpioControllerSpec>,
    #[serde(default)]
    pub duts: Vec<DutSpec>,
}

impl HostSpec {
    /// Read the host spec at `path`.
    pub fn load(path: &Path) -> Result<HostSpec> {
        let text = fs::read_to_string(path)?;
        serde_json::from_str::<serde_json::Value>(&text)
            .map_err(anyhow::Error::from)
            .and_then(HostSpec::from_value)
            .with_context(|| format!("failed to parse {}", path.display()))
    }

    /// Parse a host spec from its JSON representation.
    pub fn from_value(value: serde_json::Value) -> Result<HostSpec> {
        let spec: HostSpec = serde_json::from_value(value).context("invalid host spec")?;
        spec.resolve()
    }

    /// Check the spec's version, and resolve the controller of each GPIO.
    fn resolve(mut self) -> Result<HostSpec> {
        if self.spec_version != "v2" {
            bail!(
                "unsupported host spec version {:?}, expected \"v2\"",
                self.spec_version
            );
        }
        for dut in &mut self.duts {
            for (name, pin) in &mut dut.gpio {
                let controller = self
                    .gpio_controllers
                    .get(&pin.controller)
                    .with_context(|| {
                        format!(
                            "GPIO {name:?} of DUT {:?} references unknown controller {:?}",
                            dut.board, pin.controller
                        )
                    })?;
                pin.controller_spec = Some(controller.clone());
            }
        }
        Ok(self)
    }

    /// Return the name of the host, along with its architecture if known.
    pub fn summary(&self) -> String {
        let name = self.name.as_deref().unwrap_or("unnamed host");
        match self.platform.as_ref().and_then(|p| p.arch.as_deref()) {
            Some(arch) => format!("{name} ({arch})"),
            None => name.to_string(),
        }
    }
}

#[derive(Debug, Clone, Deserialize)]
pub struct PlatformSpec {
    /// CPU architecture of the host (e.g., `aarch64`).
    #[serde(default)]
    pub arch: Option<String>,
}

/// DUT attached to the host.
#[derive(Debug, Clone, Deserialize)]
pub struct DutSpec {
    /// Name of the board (e.g., `nrf52840dk`).
    pub board: String,
    #[serde(default)]
    pub debug: Option<DebugSpec>,
    #[serde(default)]
    pub console: Option<ConsoleSpec>,
    /// Connections of the DUT to the host besides its debug probe and console
    /// (e.g., `usb` for its own USB port).
    #[serde(default)]
    pub connectivity: Vec<String>,
    /// GPIOs of the DUT which are wired to the host, by the board's own pin
    /// name (e.g., `P0.13`).
    #[serde(default)]
    pub gpio: BTreeMap<String, GpioPinSpec>,
}

impl DutSpec {
    /// Return the DUT as it is attached to the host, or `None` if the harness
    /// does not know its board.
    ///
    /// The baud rate of the console is defined by the board, so the baud rate
    /// in the spec is not used.
    pub fn attached(&self) -> Option<AttachedDut> {
        Some(AttachedDut {
            board: Board::from_name(&self.board).ok()?,
            config: DutConfig {
                probe: self.debug.as_ref().and_then(|d| d.probe.serial.clone()),
                console: match &self.console {
                    Some(ConsoleSpec::Uart { device }) => Some(device.clone()),
                    Some(ConsoleSpec::Unsupported) | None => None,
                },
                usb: self.connectivity.iter().any(|c| c == "usb"),
                gpio: self.gpio.clone(),
            },
        })
    }
}

#[derive(Debug, Clone, Deserialize)]
pub struct DebugSpec {
    pub probe: ProbeSpec,
}

#[derive(Debug, Clone, Deserialize)]
pub struct ProbeSpec {
    #[serde(default)]
    pub serial: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum ConsoleSpec {
    Uart {
        device: String,
    },
    /// Console of a kind which the harness does not support.
    #[serde(other)]
    Unsupported,
}

/// GPIO controller of the host (e.g., the pin controller of its SoC).
#[derive(Debug, Clone, Deserialize)]
pub struct GpioControllerSpec {
    /// Driver through which the host accesses the controller (e.g.,
    /// `linux-gpiochip`).
    pub driver: String,
    /// Driver-specific settings which locate the controller.
    #[serde(default)]
    pub config: serde_json::Value,
}

/// Pin of a DUT which is wired to a GPIO controller of the host.
#[derive(Debug, Clone, Deserialize)]
pub struct GpioPinSpec {
    /// Key of the controller in `gpio_controllers`.
    pub controller: String,
    /// Driver-specific settings which locate the pin on its controller.
    #[serde(default)]
    pub config: serde_json::Value,
    /// Modes in which the host may use the pin (e.g., `digital_in` or
    /// `digital_out`).
    #[serde(default)]
    pub modes: Vec<String>,
    /// Whether the wiring inverts the signal, such that the DUT pin is at the
    /// opposite level of the host line.
    #[serde(default)]
    pub inverted: bool,
    #[serde(default)]
    pub drive: Option<GpioDrive>,
    /// Level of the DUT pin while the component connected to it (e.g., a
    /// button or an LED) is active. With `inverted` wiring, the host line is
    /// at the opposite level.
    #[serde(default)]
    pub active: Option<GpioActive>,
    /// Controller of the pin, resolved from `controller` by
    /// [`HostSpec::from_value`].
    #[serde(skip)]
    pub controller_spec: Option<GpioControllerSpec>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum GpioActive {
    Low,
    High,
    /// Level which the harness does not support.
    #[serde(other)]
    Unsupported,
}

/// Output drive of a DUT pin.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum GpioDrive {
    PushPull,
    OpenDrain,
    OpenSource,
    /// Drive which the harness does not support.
    #[serde(other)]
    Unsupported,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::boards::helpers::dut_gpios::open_gpio;
    use crate::boards::nrf52;

    fn parse(json: &str) -> HostSpec {
        let file = tempfile::NamedTempFile::new().unwrap();
        std::fs::write(file.path(), json).unwrap();
        HostSpec::load(file.path()).unwrap()
    }

    #[test]
    fn parses_v2_and_ignores_unknown_fields() {
        let spec = parse(
            r#"{
              "spec_version": "v2",
              "id": "0679be07-6106-48aa-8057-b1d4f2e18a99",
              "name": "host",
              "site": "site",
              "platform": {"kind": "physical", "arch": "aarch64", "profiles": [],
                           "vendor": "Raspberry Pi", "model": "Raspberry Pi 5"},
              "resources": {"cpu_cores": 4, "memory_mb": 8192, "storage_gb": 32},
              "labels": {},
              "future_top_level_field": [1, 2, 3],
              "gpio_controllers": {
                "rp1": {"driver": "linux-gpiochip", "config": {"label": "pinctrl-rp1"},
                        "future_field": true},
                "exp": {"driver": "some-future-driver", "config": {}}
              },
              "duts": [
                {
                  "vendor": "Nordic", "board": "nrf52840dk", "arch": ["cortex-m4"],
                  "connectivity": [], "labels": {}, "future_dut_field": {},
                  "console": {"kind": "uart", "device": "/dev/ttyACM0", "baud": 115200,
                              "future_field": 1},
                  "debug": {"protocol": "swd", "future_field": 1,
                            "probe": {"vendor": "SEGGER", "model": "J-Link OB",
                                      "serial": "123", "future_field": 1}},
                  "gpio": {
                    "P0.13": {"controller": "rp1", "config": {"offset": 21}, "label": "LED1",
                              "modes": ["digital_in", "analog_in"], "future_field": 1,
                              "active": "low", "inverted": true, "drive": "open_drain",
                              "note": "NPN, 1k base"},
                    "P0.14": {"controller": "exp", "config": {}, "modes": []}
                  }
                },
                {
                  "vendor": "Raspberry Pi", "board": "raspberry_pi_pico_2", "arch": [],
                  "connectivity": [], "labels": {}, "gpio": {},
                  "console": {"kind": "some-future-console"},
                  "debug": null
                }
              ]
            }"#,
        );
        assert_eq!(spec.summary(), "host (aarch64)");
        let nrf = &spec.duts[0];
        let attached = nrf.attached().unwrap();
        assert_eq!(attached.board, &nrf52::NRF52840DK);
        assert_eq!(attached.config.probe.as_deref(), Some("123"));
        assert_eq!(attached.config.console.as_deref(), Some("/dev/ttyACM0"));
        let led = &nrf.gpio["P0.13"];
        assert!(led.inverted);
        assert_eq!(led.drive, Some(GpioDrive::OpenDrain));
        assert!(!nrf.gpio["P0.14"].inverted);
        assert_eq!(nrf.gpio["P0.14"].drive, None);
        assert_eq!(
            led.controller_spec.as_ref().unwrap().driver,
            "linux-gpiochip"
        );
        assert!(open_gpio(led).unwrap().is_ok());
        assert!(open_gpio(&nrf.gpio["P0.14"]).is_none());

        let pico = spec.duts[1].attached().unwrap().config;
        assert_eq!(pico.probe, None);
        assert_eq!(pico.console, None);
    }

    #[test]
    #[should_panic(expected = "unsupported host spec version")]
    fn rejects_other_versions() {
        parse(r#"{"spec_version": "v1", "duts": []}"#);
    }

    #[test]
    #[should_panic(expected = "unknown controller")]
    fn rejects_unknown_controllers() {
        parse(
            r#"{"spec_version": "v2", "gpio_controllers": {}, "duts": [
                  {"board": "b", "gpio": {"P0": {"controller": "nope", "config": {}, "modes": []}}}
                ]}"#,
        );
    }
}
