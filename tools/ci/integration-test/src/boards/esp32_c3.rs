// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

//! ESP32-C3-DevKitM-1, flashed through the ROM's UART download mode over its
//! USB-UART bridge (a CP2102N), which is also its console.
//!
//! The kernel and apps run from SRAM. The ROM loads them from an ESP image at
//! the start of flash, which has a single segment at the kernel's `rom`
//! address. The board has no second-stage bootloader or partition table.

use crate::boards::helpers::serial::SerialUart;
use crate::boards::helpers::{command, tockloader};
use crate::boards::{BOARDS, Board, Dut, DutConfig, Gpio, Open, Timing, Uart};
use anyhow::Result;
use std::path::Path;
use std::process::Command;
use std::time::Duration;

/// Start of the kernel `rom` region (see
/// `boards/esp32-c3-devkitM-1/layout.ld`).
const ROM_START: u32 = 0x4038_0000;
/// Start of the app `prog` region.
const APP_START: u32 = 0x403B_8000;
/// End of the app `prog` region.
const PROG_END: u32 = 0x403E_0000;
/// Length of erased flash appended after the apps. SRAM keeps its contents
/// across resets, so without it the kernel could find a stale app behind the
/// new ones.
const APPS_TERMINATOR: usize = 256;

/// Baud rate of the board's console.
const BAUD: u32 = 115_200;
/// Baud rate for flashing. The CP2102N supports up to 3 Mbaud.
const FLASH_BAUD: &str = "460800";
/// Duration for which EN is held low to reset the chip.
const RESET_PULSE: Duration = Duration::from_millis(100);

#[linkme::distributed_slice(BOARDS)]
pub static BOARD: Board = Board {
    // Host specs use lowercase board names. The board's directory is
    // `esp32-c3-devkitM-1`:
    name: "esp32-c3-devkitm-1",
    dir: "boards/esp32-c3-devkitM-1",
    kernel: "target/riscv32imc-unknown-none-elf/release/esp32-c3-board.bin",
    tock_targets: &["$(ESP32_C3_TOCK_TARGETS)"],
    pins: &[],
    features: &[],
    timing: Timing {
        flash: Duration::from_secs(60),
        restart: Duration::from_secs(5),
        boot: Duration::from_secs(5),
        expect: Duration::from_secs(3),
    },
    open: Open::Physical(|dut| Box::new(Esp32C3::new(dut))),
};

pub struct Esp32C3 {
    dut: DutConfig,
    console: Option<SerialUart>,
}

impl Esp32C3 {
    pub fn new(dut: DutConfig) -> Self {
        Esp32C3 { dut, console: None }
    }

    /// Open the console, which continues `transcript`, and then reset the chip
    /// into the flashed image, such that no output is lost.
    ///
    /// The board's auto-reset circuit pulls EN low for RTS without DTR, and
    /// GPIO9 (boot mode) low for DTR without RTS. Holding DTR off selects SPI
    /// (flash) boot.
    fn boot(&mut self, transcript: Vec<u8>) -> Result<()> {
        let mut console = SerialUart::open_continuing(self.dut.console()?, BAUD, transcript)?;
        console.set_control_lines(false, true)?;
        std::thread::sleep(RESET_PULSE);
        console.set_control_lines(false, false)?;
        self.console = Some(console);
        Ok(())
    }
}

impl Dut for Esp32C3 {
    fn board(&self) -> &'static Board {
        &BOARD
    }

    fn flash(&mut self, kernel: &Path, apps: &[&Path]) -> Result<()> {
        let dir = tempfile::tempdir()?;
        let flash_file = dir.path().join("flash.img");
        let image = dir.path().join("image.bin");
        let board = BOARD.name;

        // The flash file starts at `ROM_START`, which is tockloader's
        // `flash_address` for this board:
        let (rom, app) = (ROM_START.into(), Some(APP_START.into()));
        tockloader::write_flash_file(&flash_file, board, rom, app, kernel, apps)?;

        let mut ram = fs_err::read(&flash_file)?;
        let max_len = (PROG_END - ROM_START) as usize;
        ram.resize((ram.len() + APPS_TERMINATOR).min(max_len), 0xff);
        fs_err::write(&image, esp_image(ROM_START, &ram))?;

        // esptool needs the port, and resets the chip into download mode
        // itself. The chip stays in download mode until `boot` resets it, once
        // the console is open:
        self.console = None;
        let device = self.dut.console()?;
        command::run(
            Command::new("esptool")
                .args(["--chip", "esp32c3", "--port", device, "--baud", FLASH_BAUD])
                .args(["--before", "default-reset", "--after", "no-reset"])
                .args([
                    "write-flash",
                    "--flash-mode",
                    "keep",
                    "--flash-freq",
                    "keep",
                ])
                .args(["--flash-size", "keep", "0x0"])
                .arg(&image),
        )?;

        self.boot(Vec::new())
    }

    fn restart(&mut self) -> Result<()> {
        let transcript = self
            .console
            .take()
            .map_or_else(Vec::new, SerialUart::into_transcript);
        self.boot(transcript)
    }

    fn reset(&mut self) -> Result<()> {
        self.console = None;
        Ok(())
    }

    fn uart(&mut self) -> Option<&mut dyn Uart> {
        self.console.as_mut().map(|c| c as &mut dyn Uart)
    }

    fn gpio(&mut self, _name: &str) -> Option<&mut dyn Gpio> {
        None
    }
}

/// Construct an ESP32-C3 image of `data`, which the ROM loads to and enters at
/// `addr`.
///
/// The image equals the one which `esptool elf2image --flash-mode dio
/// --flash-freq 80m --flash-size 4MB --dont-append-digest` creates of an ELF
/// file with one segment.
fn esp_image(addr: u32, data: &[u8]) -> Vec<u8> {
    let mut segment = data.to_vec();
    segment.resize(data.len().next_multiple_of(4), 0);

    // Magic, segment count, SPI mode (DIO), flash size (4MB) and frequency
    // (80 MHz), and entry point:
    let mut image = vec![0xe9, 1, 0x02, 0x2f];
    image.extend_from_slice(&addr.to_le_bytes());
    // Extended header with the WP pin (unused), SPI pin drive strengths, chip
    // ID (5, the ESP32-C3), minimum chip revision (0, and 0.0), maximum chip
    // revision (any), reserved bytes, and no appended SHA-256 digest:
    image.extend_from_slice(&[0xee, 0, 0, 0]);
    image.extend_from_slice(&5u16.to_le_bytes());
    image.extend_from_slice(&[0, 0, 0, 0xff, 0xff, 0, 0, 0, 0, 0]);

    image.extend_from_slice(&addr.to_le_bytes());
    image.extend_from_slice(&(segment.len() as u32).to_le_bytes());
    image.extend_from_slice(&segment);

    // Pad the image, such that the checksum byte ends a 16-byte block:
    image.resize((image.len() + 1).next_multiple_of(16) - 1, 0);
    image.push(segment.iter().fold(0xef, |sum, byte| sum ^ byte));
    image
}

#[cfg(test)]
mod tests {
    use super::esp_image;

    #[test]
    fn esp_image_matches_esptool() {
        // Image created by esptool v5.4.0 from an ELF of `data` at 0x40380000:
        let data = [0x97, 0x11, 0x92, 0xff, 0x93, 0x81];
        let image = esp_image(0x4038_0000, &data);
        let header = [
            0xe9, 0x01, 0x02, 0x2f, 0x00, 0x00, 0x38, 0x40, // header
            0xee, 0x00, 0x00, 0x00, 0x05, 0x00, 0x00, 0x00, // extended header
            0x00, 0xff, 0xff, 0x00, 0x00, 0x00, 0x00, 0x00, //
            0x00, 0x00, 0x38, 0x40, 0x08, 0x00, 0x00, 0x00, // segment header
        ];
        assert_eq!(image[..32], header);
        assert_eq!(image[32..40], [0x97, 0x11, 0x92, 0xff, 0x93, 0x81, 0, 0]);
        assert_eq!(image.len(), 48);
        let checksum = data.iter().fold(0xef, |sum, byte| sum ^ byte);
        assert_eq!(image[47], checksum);
    }
}
