// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use std::path::Path;
use std::time::Duration;

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize,
)]
/// A board, named (in plans, test ids, on the command line and in host specs)
/// like its Tock board directory; see [`BoardKind::name`].
pub enum BoardKind {
    #[serde(rename = "qemu_rv64_virt")]
    QemuVirt,
    #[serde(rename = "nrf52840dk")]
    Nrf52840Dk,
    #[serde(rename = "nucleo_f429zi")]
    NucleoF429zi,
    #[serde(rename = "raspberry_pi_pico")]
    RaspberryPiPico,
    #[serde(rename = "raspberry_pi_pico_2")]
    RaspberryPiPico2,
    /// Lowercase, unlike its Tock board directory (`esp32-c3-devkitM-1`), as
    /// host-spec board names are.
    #[serde(rename = "esp32-c3-devkitm-1")]
    Esp32C3DevkitM1,
}

impl BoardKind {
    pub const ALL: &[BoardKind] = &[
        BoardKind::QemuVirt,
        BoardKind::Nrf52840Dk,
        BoardKind::NucleoF429zi,
        BoardKind::RaspberryPiPico,
        BoardKind::RaspberryPiPico2,
        BoardKind::Esp32C3DevkitM1,
    ];

    /// The board's name, equal to its serde name and its host-spec DUT
    /// `board`.
    pub fn name(self) -> &'static str {
        match self {
            BoardKind::QemuVirt => "qemu_rv64_virt",
            BoardKind::Nrf52840Dk => "nrf52840dk",
            BoardKind::NucleoF429zi => "nucleo_f429zi",
            BoardKind::RaspberryPiPico => "raspberry_pi_pico",
            BoardKind::RaspberryPiPico2 => "raspberry_pi_pico_2",
            BoardKind::Esp32C3DevkitM1 => "esp32-c3-devkitm-1",
        }
    }

    pub fn from_name(name: &str) -> Option<BoardKind> {
        BoardKind::ALL.iter().copied().find(|b| b.name() == name)
    }

    /// The DUT `board` name in a host spec, or `None` for a virtual board.
    pub fn host_spec_board(self) -> Option<&'static str> {
        (self != BoardKind::QemuVirt).then(|| self.name())
    }

    /// The schematic pin name (host-spec `gpio` key) of a logical GPIO name,
    /// or `None` if the board's backend provides no such GPIO.
    pub fn pin(self, name: &str) -> Option<&'static str> {
        let map = match self {
            BoardKind::Nrf52840Dk => crate::boards::nrf52840dk::PIN_MAP,
            BoardKind::NucleoF429zi => crate::boards::nucleo_f429zi::PIN_MAP,
            BoardKind::QemuVirt
            | BoardKind::RaspberryPiPico
            | BoardKind::RaspberryPiPico2
            | BoardKind::Esp32C3DevkitM1 => &[],
        };
        map.iter().find(|(n, _)| *n == name).map(|(_, pin)| *pin)
    }

    /// The libtock-c `TOCK_TARGETS` to build apps for, so that building an app
    /// only needs the compilers for the boards that run it. Entries may
    /// reference variables from libtock-c's `Configuration.mk`, which make
    /// expands.
    pub fn tock_targets(self) -> &'static [&'static str] {
        match self {
            BoardKind::QemuVirt => &["$(QEMU_RV64_VIRT_TOCK_TARGETS)"],
            BoardKind::Nrf52840Dk | BoardKind::NucleoF429zi => &["cortex-m4"],
            BoardKind::RaspberryPiPico => &["cortex-m0"],
            // The RP2350's Cortex-M33 runs cortex-m4 builds.
            BoardKind::RaspberryPiPico2 => &["cortex-m4"],
            BoardKind::Esp32C3DevkitM1 => &["$(ESP32_C3_TOCK_TARGETS)"],
        }
    }
}

impl std::str::FromStr for BoardKind {
    type Err = String;

    fn from_str(name: &str) -> Result<Self, String> {
        BoardKind::from_name(name).ok_or_else(|| {
            let names: Vec<_> = BoardKind::ALL.iter().map(|b| b.name()).collect();
            format!("unknown board, expected one of: {}", names.join(", "))
        })
    }
}

impl std::fmt::Display for BoardKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GpioMode {
    DigitalIn,
    DigitalOut,
}

impl GpioMode {
    /// The mode's name in a host spec's GPIO `modes`.
    pub fn as_str(self) -> &'static str {
        match self {
            GpioMode::DigitalIn => "digital_in",
            GpioMode::DigitalOut => "digital_out",
        }
    }
}

/// A board's console. Backends implement `send` and `receive_until`; tests
/// use `write` and `wait_for`, which log each step.
pub trait Uart {
    fn send(&mut self, bytes: &[u8]) -> Result<(), String>;
    /// Wait for `needle`, returning the output up to and including it.
    fn receive_until(&mut self, needle: &str, timeout: Duration) -> Result<String, String>;
    /// Everything received since the console was opened, for the test's log.
    fn transcript(&self) -> &[u8];

    fn write(&mut self, bytes: &[u8]) -> Result<(), String> {
        log::info!("  sending {:?}", String::from_utf8_lossy(bytes));
        self.send(bytes)
    }

    /// Like `write`, but one byte at a time with `delay` after each, for
    /// consoles that drop input sent faster than they process it. Logged once.
    fn write_slowly(&mut self, bytes: &[u8], delay: Duration) -> Result<(), String> {
        log::info!("  typing {:?}", String::from_utf8_lossy(bytes));
        for byte in bytes {
            self.send(std::slice::from_ref(byte))?;
            std::thread::sleep(delay);
        }
        Ok(())
    }

    fn wait_for(&mut self, needle: &str, timeout: Duration) -> Result<String, String> {
        log::info!("  waiting for {needle:?} (up to {timeout:?})");
        let out = self.receive_until(needle, timeout)?;
        log::debug!("  received {out:?}");
        Ok(out)
    }
}

pub fn take_through(buf: &mut Vec<u8>, needle: &str) -> Option<String> {
    let end = buf
        .windows(needle.len())
        .position(|w| w == needle.as_bytes())?
        + needle.len();
    let taken: Vec<u8> = buf.drain(..end).collect();
    Some(String::from_utf8_lossy(&taken).into_owned())
}

pub fn timeout_error(buf: &[u8], needle: &str) -> String {
    let tail = &buf[buf.len().saturating_sub(200)..];
    format!(
        "timed out waiting for {needle:?}, last output: {:?}",
        String::from_utf8_lossy(tail)
    )
}

/// A host GPIO wired to the DUT. Backends implement `configure`, `drive` and
/// `read`; `set_mode` and `write` log each change.
pub trait Gpio {
    fn configure(&mut self, mode: GpioMode) -> Result<(), String>;
    fn drive(&mut self, high: bool) -> Result<(), String>;
    /// Not logged: tests poll it.
    fn read(&mut self) -> Result<bool, String>;

    fn set_mode(&mut self, mode: GpioMode) -> Result<(), String> {
        log::debug!("  setting GPIO to {}", mode.as_str());
        self.configure(mode)
    }

    fn write(&mut self, high: bool) -> Result<(), String> {
        log::debug!("  driving GPIO {}", if high { "high" } else { "low" });
        self.drive(high)
    }
}

pub trait Board {
    fn kind(&self) -> BoardKind;
    fn flash(&mut self, kernel: &Path, apps: &[&Path]) -> Result<(), String>;
    fn reset(&mut self) -> Result<(), String>;
    /// Reset the DUT, keeping what `flash` wrote, and reopen its console,
    /// continuing its transcript. Only some backends support it.
    fn restart(&mut self) -> Result<(), String> {
        Err(format!("restarting is not supported on {}", self.kind()))
    }
    fn uart(&mut self) -> Option<&mut dyn Uart>;
    fn gpio(&mut self, name: &str) -> Option<&mut dyn Gpio>;
}

#[cfg(test)]
mod tests {
    use super::BoardKind;

    #[test]
    fn serde_name_is_name() {
        for &board in BoardKind::ALL {
            let json = serde_json::to_value(board).unwrap();
            assert_eq!(json, board.name());
        }
    }
}
