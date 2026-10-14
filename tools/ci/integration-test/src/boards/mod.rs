// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

pub mod helpers;
pub mod nrf52;
pub mod nucleo_f429zi;
pub mod qemu;

use crate::host_spec::GpioPinSpec;
use crate::test_cases::{Requirement, TestCase};
use anyhow::{Context, Result, anyhow, bail};
use std::collections::BTreeMap;
use std::path::Path;
use std::time::Duration;

/// Collection of boards supported by this end-to-end harness.
///
/// Implemented through a [`distributed_slice`](linkme::distributed_slice),
/// such that this slice gets assembled at link-time and we don't need to have a
/// single registry for it.
#[linkme::distributed_slice]
pub static BOARDS: [Board];

/// Description of a board supported by this harness.
///
/// Each board is registered in [`BOARDS`]. Boards are compared by their
/// [`name`](Board::name).
#[derive(Debug)]
pub struct Board {
    /// Name of the board in plans, test ids, host specs and on the command
    /// line. This name must be unique among [`BOARDS`].
    pub name: &'static str,
    /// Tock board directory in which the kernel is built, relative to the Tock
    /// repository.
    pub dir: &'static str,
    /// Path of the kernel binary produced by the build, relative to the Tock
    /// repository.
    pub kernel: &'static str,
    /// libtock-c `TOCK_TARGETS` to build apps for.
    ///
    /// Apps are only built for specific boards, which accelerates builds and
    /// avoids us having to depend on compilers for architecture we don't intend
    /// to execute on.
    ///
    /// Entries can reference variables from libtock-c's `Configuration.mk` as
    /// those get expanded by the call to `make`.
    pub tock_targets: &'static [&'static str],
    /// Logical names of the board's GPIOs, mapped to the backend's name for
    /// each (e.g., the pin name in the board's schematic, as used in host
    /// specs, or the QEMU key which stands in for a button).
    pub pins: &'static [(&'static str, &'static str)],
    /// Features which the board provides to tests, beyond its console and
    /// GPIOs.
    pub features: &'static [Feature],
    /// Upper bounds on the duration of certain board-specific operations.
    pub timing: Timing,
    /// Open the board for running tests.
    pub open: Open,
}

/// Function which opens a board for running tests.
#[derive(Debug)]
pub enum Open {
    /// The board's a virtual board which we spawn on demand from the Tock
    /// repository at the given path, no config required.
    Virtual(fn(&Path) -> Box<dyn Dut>),
    /// This is a physical board which needs a configuration (e.g., to select a
    /// board with a particular serial number).
    Physical(fn(DutConfig) -> Box<dyn Dut>),
}

/// Physical board attached to this host.
///
/// Only a host spec can wire GPIOs of the board to the host.
#[derive(Debug, Clone)]
pub struct AttachedDut {
    pub board: &'static Board,
    /// How the board is connected to this host.
    pub config: DutConfig,
}

impl AttachedDut {
    /// Return the requirements among `requires` which the board, as it is
    /// attached to this host, does not provide.
    pub fn missing(&self, requires: &[Requirement]) -> Vec<Requirement> {
        (requires.iter().copied())
            .filter(|r| !self.satisfies(*r))
            .collect()
    }

    /// Check if the board, as it is attached to this host, provides all
    /// requirements of `test`.
    pub fn can_run(&self, test: &TestCase) -> bool {
        test.requires.iter().all(|r| self.satisfies(*r))
    }

    /// Check if the board, as it is attached to this host, provides
    /// `requirement`.
    pub fn satisfies(&self, requirement: Requirement) -> bool {
        match requirement {
            Requirement::Uart => self.config.console.is_some(),
            Requirement::Gpio { name, mode } => (self.board.pin(name))
                .and_then(|pin| self.config.gpio.get(pin))
                .is_some_and(|pin| {
                    pin.modes.iter().any(|m| m == mode.as_str())
                        && (pin.controller_spec.as_ref())
                            .is_some_and(|c| c.driver == "linux-gpiochip")
                }),
            // Features are properties of the board's kernel and backend:
            Requirement::Feature(_) => true,
        }
    }
}

/// Configuration describing how a physical board is connected to this host (see
/// [`Open::Physical`]).
#[derive(Debug, Clone, Default)]
pub struct DutConfig {
    /// Serial number of the board's debug probe, to select it among several.
    pub probe: Option<String>,
    /// Path of the serial port of the board's UART console (e.g.,
    /// `/dev/ttyACM0`).
    pub console: Option<String>,
    /// GPIOs of the board which are wired to the host, by the board's own pin
    /// name (e.g., `P0.13`).
    pub gpio: BTreeMap<String, GpioPinSpec>,
}

impl DutConfig {
    /// Return the path of the serial port of the board's UART console, or an
    /// error if none is configured.
    pub fn console(&self) -> Result<&str> {
        (self.console.as_deref()).context("DUT has no console configured")
    }
}

impl std::str::FromStr for AttachedDut {
    type Err = anyhow::Error;

    /// Parse an [`AttachedDut`] from a string description of the form
    /// `<board>[,probe=<serial>][,console=<device>]`.
    fn from_str(s: &str) -> Result<Self> {
        let mut fields = s.split(',');

        let board = Board::from_name(fields.next().unwrap_or_default())?;
        if board.is_virtual() {
            bail!("{board} is virtual, `--dut` is not supported for it");
        }

        let mut config = DutConfig::default();
        for field in fields {
            match field.split_once('=') {
                Some(("probe", serial)) => config.probe = Some(serial.to_string()),
                Some(("console", device)) => config.console = Some(device.to_string()),
                _ => bail!(
                    "unknown DUT setting {field:?}, expected probe=<serial> or console=<device>"
                ),
            }
        }

        Ok(AttachedDut { board, config })
    }
}

/// Upper bounds on the duration of a board's operations.
///
/// A test's worst-case duration is computed from these bounds.
#[derive(Debug)]
pub struct Timing {
    /// Maximum time to write the kernel and apps to the board and start them.
    ///
    /// Exceeding this time will abort tests and report an error.
    pub flash: Duration,
    /// Maximum time for the board to boot after flashing.
    ///
    /// This time is added to the timeout of the test's first wait, or following
    /// every board reset.
    pub boot: Duration,
    /// Maximum time a test waits for expected console output.
    pub expect: Duration,
}

/// Feature of a board, beyond its console and GPIOs, which tests may require.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Feature {
    /// The board has a screen, which its backend captures through
    /// [`Dut::screen`].
    Display,
    /// The kernel reports the ARMv7-M fault status registers (CFSR and HFSR)
    /// of a kernel HardFault, and its process console has the `hardfault`
    /// command to cause one.
    ArmV7mHardFault,
}

impl Board {
    /// Look up a board in [`BOARDS`] by name.
    ///
    /// Returns an error which lists all boards if no board has this name.
    pub fn from_name(name: &str) -> Result<&'static Board> {
        BOARDS.iter().find(|b| b.name == name).ok_or_else(|| {
            let mut names: Vec<_> = BOARDS.iter().map(|b| b.name).collect();
            names.sort();
            anyhow!(
                "unknown board {name:?}, expected one of: {}",
                names.join(", ")
            )
        })
    }

    /// Check if the board is virtual, such that it needs no DUT attached to the
    /// host.
    pub fn is_virtual(&self) -> bool {
        matches!(self.open, Open::Virtual(_))
    }

    /// Check if the board provides `requirement`.
    ///
    /// This only considers the static board definition in this harness itself,
    /// and does not dynamically probe whether the requirement is fulfilled on
    /// this host. Whether a physical board provides `requirement` also depends
    /// on how it is attached to the host (e.g., if its console is connected),
    /// which [`AttachedDut::satisfies`] checks.
    pub fn provides(&self, requirement: Requirement) -> bool {
        match requirement {
            Requirement::Uart => true,
            Requirement::Gpio { name, .. } => self.pin(name).is_some(),
            Requirement::Feature(feature) => self.features.contains(&feature),
        }
    }

    /// Return the backend's name for the GPIO with the logical name `name`, or
    /// `None` if the board has no such GPIO.
    pub fn pin(&self, name: &str) -> Option<&'static str> {
        (self.pins.iter())
            .find(|(n, _)| *n == name)
            .map(|(_, pin)| *pin)
    }
}

impl PartialEq for Board {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl std::fmt::Display for Board {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name)
    }
}

/// Serialize and deserialize a [`Board`] by its name.
///
/// Module for use with `#[serde(with = ...)]` to serialize a Board to its name,
/// and to deserialize a name of a board by trying to find a match in
/// [`BOARDS`](super::BOARDS).
pub mod by_name {
    use super::Board;
    use serde::{Deserialize, Deserializer, Serializer};
    use std::borrow::Cow;

    pub fn serialize<S: Serializer>(board: &&'static Board, s: S) -> Result<S::Ok, S::Error> {
        s.serialize_str(board.name)
    }

    /// Deserialize a board name and look up the board in
    /// [`BOARDS`](super::BOARDS).
    pub fn deserialize<'de, D: Deserializer<'de>>(d: D) -> Result<&'static Board, D::Error> {
        let name = Cow::<'de, str>::deserialize(d)?;
        Board::from_name(&name).map_err(serde::de::Error::custom)
    }
}

/// Console of a board.
///
/// Implementations provide [`Uart::send`] and [`Uart::receive_until`]. Callers
/// use [`Uart::write_slowly`] and [`Uart::wait_for`].
pub trait Uart {
    /// Send `bytes` on the console.
    fn send(&mut self, bytes: &[u8]) -> Result<()>;
    /// Wait up to `timeout` for `needle` on the console. This returns the
    /// output up to and including `needle`.
    fn receive_until(&mut self, needle: &str, timeout: Duration) -> Result<String>;
    /// Return all output received since the console was opened.
    fn transcript(&mut self) -> &[u8];

    /// Send `bytes` one at a time, with `delay` after each byte.
    ///
    /// Consoles may drop input which arrives faster than they process it. This
    /// logs the sent text once.
    fn write_slowly(&mut self, bytes: &[u8], delay: Duration) -> Result<()> {
        log::info!("  typing {:?}", String::from_utf8_lossy(bytes));
        for byte in bytes {
            self.send(std::slice::from_ref(byte))?;
            std::thread::sleep(delay);
        }
        Ok(())
    }

    /// Wait up to `timeout` for `needle` on the console. This behaves like
    /// [`Uart::receive_until`], and additionally logs the wait and the received
    /// output.
    fn wait_for(&mut self, needle: &str, timeout: Duration) -> Result<String> {
        log::info!("  waiting for {needle:?} (up to {timeout:?})");
        let out = self.receive_until(needle, timeout)?;
        log::debug!("  received {out:?}");
        Ok(out)
    }
}

/// Mode of a GPIO, as seen from the host.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GpioMode {
    DigitalIn,
    DigitalOut,
}

impl GpioMode {
    /// Return the name of the mode in the `modes` of a GPIO in a host spec.
    pub fn as_str(self) -> &'static str {
        match self {
            GpioMode::DigitalIn => "digital_in",
            GpioMode::DigitalOut => "digital_out",
        }
    }
}

/// GPIO of a device, driven or read by the host.
///
/// Implementations provide [`Gpio::configure`], [`Gpio::drive`] and
/// [`Gpio::read`]. Callers use [`Gpio::set_mode`] and [`Gpio::write`], which
/// also log each change.
pub trait Gpio {
    /// Configure the GPIO in `mode`.
    fn configure(&mut self, mode: GpioMode) -> Result<()>;
    /// Drive the GPIO high or low.
    fn drive(&mut self, high: bool) -> Result<()>;
    /// Read the level of the GPIO. This is not logged, as tests poll it.
    fn read(&mut self) -> Result<bool>;

    /// Return the level of the device's pin while the component connected to
    /// it (e.g., a button) is active.
    ///
    /// The default implementation returns `false` (active low).
    fn active_high(&self) -> bool {
        false
    }

    /// Configure the GPIO in `mode`, and log the change.
    fn set_mode(&mut self, mode: GpioMode) -> Result<()> {
        log::debug!("  setting GPIO to {mode:?}");
        self.configure(mode)
    }

    /// Drive the GPIO high or low, and log the change.
    fn write(&mut self, high: bool) -> Result<()> {
        log::debug!("  driving GPIO {}", if high { "high" } else { "low" });
        self.drive(high)
    }
}

/// Device under test.
///
/// Whereas a [`Board`] statically describes a board "kind", a `Dut` is a single
/// instance of that kind, opened through [`Board::open`], and able to run
/// tests.
///
/// It holds the state of this device, such as connections to an emulator or to
/// the device's console.
pub trait Dut {
    /// Return the board this device is an instance of.
    fn board(&self) -> &'static Board;
    /// Write `kernel` and `apps` to the device and start them.
    fn flash(&mut self, kernel: &Path, apps: &[&Path]) -> Result<()>;
    /// Stop the software running on the device. A subsequent [`Dut::flash`]
    /// starts it again.
    fn reset(&mut self) -> Result<()>;
    /// Return the device's console, or `None` if the device has no console.
    fn uart(&mut self) -> Option<&mut dyn Uart>;
    /// Return the device's GPIO with the logical name `name`, or `None` if the
    /// device has no such GPIO.
    fn gpio(&mut self, name: &str) -> Option<&mut dyn Gpio>;
    /// Capture the device's screen as a PPM image.
    ///
    /// The default implementation returns an error, for devices without a
    /// screen.
    fn screen(&mut self) -> Result<Vec<u8>> {
        bail!("{} has no screen", self.board())
    }
}

#[cfg(test)]
mod tests {
    use super::{AttachedDut, BOARDS, by_name, nrf52};

    #[test]
    fn duts_parse_from_their_board_and_settings() {
        let dut: AttachedDut = "nrf52840dk,console=/dev/ttyACM0,probe=123".parse().unwrap();
        assert_eq!(dut.board, &nrf52::NRF52840DK);
        assert_eq!(dut.config.console.as_deref(), Some("/dev/ttyACM0"));
        assert_eq!(dut.config.probe.as_deref(), Some("123"));
        assert!("nrf52840dk,baud=9600".parse::<AttachedDut>().is_err());
        assert!("qemu_rv64_virt".parse::<AttachedDut>().is_err());
    }

    #[test]
    fn duts_satisfy_what_they_are_wired_for() {
        use super::GpioMode::{DigitalIn, DigitalOut};
        use crate::test_cases::Requirement;

        // An nRF52840DK with a console. Its `button0` (`P0.11`) supports
        // `digital_out` on a `linux-gpiochip` controller, `button1` (`P0.12`)
        // only on another driver, and `led0` (`P0.13`) only `digital_in`:
        let spec = crate::host_spec::HostSpec::from_value(serde_json::json!({
            "spec_version": "v2",
            "gpio_controllers": {
                "rp1": {"driver": "linux-gpiochip", "config": {}},
                "exp": {"driver": "some-future-driver", "config": {}}
            },
            "duts": [{
                "board": "nrf52840dk",
                "console": {"kind": "uart", "device": "/dev/ttyACM0", "baud": 115200},
                "gpio": {
                    "P0.11": {"controller": "rp1", "modes": ["digital_out"]},
                    "P0.12": {"controller": "exp", "modes": ["digital_out"]},
                    "P0.13": {"controller": "rp1", "modes": ["digital_in"]}
                }
            }]
        }))
        .unwrap();
        let dut = spec.duts[0].attached().unwrap();
        let gpio = |name, mode| Requirement::Gpio { name, mode };

        assert!(dut.satisfies(Requirement::Uart));
        assert!(dut.satisfies(gpio("button0", DigitalOut)));
        assert!(dut.satisfies(gpio("led0", DigitalIn)));
        // A controller which the harness cannot drive:
        assert!(!dut.satisfies(gpio("button1", DigitalOut)));
        // A mode which the host does not support:
        assert!(!dut.satisfies(gpio("led0", DigitalOut)));
        // GPIOs which are not wired, or not mapped by the backend:
        assert!(!dut.satisfies(gpio("led1", DigitalIn)));
        assert!(!dut.satisfies(gpio("no-such-gpio", DigitalIn)));

        let without: AttachedDut = "nrf52840dk".parse().unwrap();
        assert!(!without.satisfies(Requirement::Uart));
    }

    #[test]
    fn board_names_are_unique() {
        let mut names: Vec<_> = BOARDS.iter().map(|b| b.name).collect();
        names.sort();
        let len = names.len();
        names.dedup();
        assert_eq!(names.len(), len, "a board name is used twice");
    }

    #[test]
    fn serialized_as_name() {
        for board in BOARDS {
            let json = by_name::serialize(&board, serde_json::value::Serializer).unwrap();
            assert_eq!(json, board.name);
            let back = by_name::deserialize(json).unwrap();
            assert!(std::ptr::eq(back, board));
        }
    }
}
