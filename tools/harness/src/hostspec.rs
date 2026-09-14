// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

//! The subset of a v2 host spec that the harness uses.
//!
//! Unknown fields are ignored, as are unknown console kinds and GPIO drivers,
//! so that additions to the schema do not break the harness.

use serde::Deserialize;
use std::collections::BTreeMap;
use std::path::Path;

#[derive(Debug, Clone, Deserialize)]
pub struct HostSpec {
    pub spec_version: String,
    /// The host's name, e.g. `pton-rpi2003`.
    #[serde(default)]
    pub name: Option<String>,
    #[serde(default)]
    pub platform: Option<PlatformSpec>,
    #[serde(default)]
    pub gpio_controllers: BTreeMap<String, GpioControllerSpec>,
    #[serde(default)]
    pub duts: Vec<DutSpec>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct PlatformSpec {
    /// The host's CPU architecture, e.g. `aarch64`.
    #[serde(default)]
    pub arch: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct DutSpec {
    /// The board, as a lowercase identifier, e.g. `nrf52840dk`.
    pub board: String,
    #[serde(default)]
    pub debug: Option<DebugSpec>,
    #[serde(default)]
    pub console: Option<ConsoleSpec>,
    /// Keyed by the board's own pin name, e.g. `P0.13`.
    #[serde(default)]
    pub gpio: BTreeMap<String, GpioPinSpec>,
}

impl DutSpec {
    /// The serial number of the DUT's debug probe, if known.
    pub fn probe_serial(&self) -> Option<&str> {
        self.debug.as_ref()?.probe.serial.as_deref()
    }

    /// The device node and baud rate of the DUT's UART console.
    pub fn uart_console(&self) -> Result<(String, u32), String> {
        match &self.console {
            Some(ConsoleSpec::Uart { device, baud }) => Ok((device.clone(), *baud)),
            Some(ConsoleSpec::Unsupported) => Err("DUT console is not a UART".into()),
            None => Err("DUT has no console configured".into()),
        }
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
        baud: u32,
    },
    /// A console kind this harness does not know how to use.
    #[serde(other)]
    Unsupported,
}

/// A GPIO controller on the host, e.g. a SoC's pin controller.
#[derive(Debug, Clone, Deserialize)]
pub struct GpioControllerSpec {
    /// How the host drives this controller, e.g. `linux-gpiochip`.
    pub driver: String,
    /// Driver-specific settings that locate the controller.
    #[serde(default)]
    pub config: serde_json::Value,
}

/// A DUT pin wired to a host GPIO controller.
#[derive(Debug, Clone, Deserialize)]
pub struct GpioPinSpec {
    /// The key of the controller in `gpio_controllers`.
    pub controller: String,
    /// Driver-specific settings that locate the pin on its controller.
    #[serde(default)]
    pub config: serde_json::Value,
    /// How the host may use the pin, e.g. `digital_in`, `digital_out`.
    #[serde(default)]
    pub modes: Vec<String>,
    /// Whether the wiring inverts the signal, so that the DUT pin sits at the
    /// opposite level of the host line.
    #[serde(default)]
    pub inverted: bool,
    /// How the host can drive the DUT pin.
    #[serde(default)]
    pub drive: Option<GpioDrive>,
    /// The pin's controller, resolved from `controller` by [`load`].
    #[serde(skip)]
    pub controller_spec: Option<GpioControllerSpec>,
}

/// The output drive at a DUT pin.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum GpioDrive {
    PushPull,
    OpenDrain,
    OpenSource,
    /// A drive this harness does not know.
    #[serde(other)]
    Unsupported,
}

pub fn load(path: &Path) -> HostSpec {
    let data = std::fs::read_to_string(path).expect("failed to read host spec");
    let spec = serde_json::from_str(&data).expect("failed to parse host spec");
    resolve(spec).unwrap_or_else(|e| panic!("{e}"))
}

/// Parse a host spec from JSON, e.g. one Treadmill serves for a host.
pub fn from_value(value: serde_json::Value) -> Result<HostSpec, String> {
    resolve(serde_json::from_value(value).map_err(|e| format!("invalid host spec: {e}"))?)
}

/// Check a parsed spec's version, and resolve each pin's controller.
fn resolve(mut spec: HostSpec) -> Result<HostSpec, String> {
    if spec.spec_version != "v2" {
        return Err(format!(
            "unsupported host spec version {:?}, expected \"v2\"",
            spec.spec_version
        ));
    }
    for dut in &mut spec.duts {
        for (name, pin) in &mut dut.gpio {
            let controller = spec.gpio_controllers.get(&pin.controller).ok_or_else(|| {
                format!(
                    "GPIO {name:?} of DUT {:?} references unknown controller {:?}",
                    dut.board, pin.controller
                )
            })?;
            pin.controller_spec = Some(controller.clone());
        }
    }
    Ok(spec)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(json: &str) -> HostSpec {
        let file = tempfile::NamedTempFile::new().unwrap();
        std::fs::write(file.path(), json).unwrap();
        load(file.path())
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
        let nrf = &spec.duts[0];
        assert_eq!(nrf.board, "nrf52840dk");
        assert_eq!(nrf.probe_serial(), Some("123"));
        assert_eq!(nrf.uart_console(), Ok(("/dev/ttyACM0".into(), 115200)));
        let led = &nrf.gpio["P0.13"];
        assert!(led.inverted);
        assert_eq!(led.drive, Some(GpioDrive::OpenDrain));
        assert!(!nrf.gpio["P0.14"].inverted);
        assert_eq!(nrf.gpio["P0.14"].drive, None);
        assert_eq!(
            led.controller_spec.as_ref().unwrap().driver,
            "linux-gpiochip"
        );
        assert!(crate::boards::open_gpio(led).unwrap().is_ok());
        assert!(crate::boards::open_gpio(&nrf.gpio["P0.14"]).is_none());

        let pico = &spec.duts[1];
        assert_eq!(pico.probe_serial(), None);
        assert!(pico.uart_console().is_err());
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
