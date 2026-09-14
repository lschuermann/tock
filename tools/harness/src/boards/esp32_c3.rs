// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

//! ESP32-C3-DevKitM-1, flashed through the ROM's UART download mode over its
//! USB-UART bridge (a CP2102N), which is also its console.
//!
//! The kernel and apps run from SRAM: the ROM loads them from an ESP image at
//! the start of flash, a single segment at the kernel's `rom` address. The
//! board has no second-stage bootloader or partition table.

use crate::board::{Board, BoardKind, Gpio, Uart};
use crate::boards::run;
use crate::boards::serial_uart::SerialUart;
use crate::hostspec::DutSpec;
use std::path::Path;
use std::process::Command;
use std::time::Duration;

// Kernel `rom` and app `prog` regions from boards/esp32-c3-devkitM-1/layout.ld.
const ROM_START: u32 = 0x4038_0000;
const APP_START: u32 = 0x403B_8000;
const PROG_END: u32 = 0x403E_0000;
/// Erased flash appended after the apps. SRAM keeps its contents across
/// resets, so without it the kernel could find a stale app behind the new
/// ones.
const APPS_TERMINATOR: usize = 256;

/// Baud rate for flashing; the CP2102N supports up to 3 Mbaud.
const FLASH_BAUD: &str = "460800";
/// How long EN is held low to reset the chip.
const RESET_PULSE: Duration = Duration::from_millis(100);

pub struct Esp32C3 {
    dut: DutSpec,
    console: Option<SerialUart>,
}

impl Esp32C3 {
    pub fn new(dut: DutSpec) -> Self {
        Esp32C3 { dut, console: None }
    }

    /// Open the console, continuing `transcript`, and reset the chip into
    /// the flashed image, so no output is lost.
    ///
    /// The board's auto-reset circuit pulls EN low for RTS without DTR, and
    /// GPIO9 (boot mode) low for DTR without RTS. Holding DTR off selects SPI
    /// (flash) boot.
    fn boot(&mut self, transcript: Vec<u8>) -> Result<(), String> {
        let (device, baud) = self.dut.uart_console()?;
        let mut console = SerialUart::open_continuing(&device, baud, transcript)?;
        console.set_control_lines(false, true)?;
        std::thread::sleep(RESET_PULSE);
        console.set_control_lines(false, false)?;
        self.console = Some(console);
        Ok(())
    }
}

impl Board for Esp32C3 {
    fn kind(&self) -> BoardKind {
        BoardKind::Esp32C3DevkitM1
    }

    fn flash(&mut self, kernel: &Path, apps: &[&Path]) -> Result<(), String> {
        let dir = tempfile::tempdir().map_err(|e| e.to_string())?;
        let flash_file = dir.path().join("flash.img");
        let image = dir.path().join("image.bin");
        let board = BoardKind::Esp32C3DevkitM1.name();

        // The flash file starts at ROM_START (tockloader's `flash_address`
        // for this board).
        run(Command::new("tockloader")
            .arg("flash")
            .arg("--flash-file")
            .arg(&flash_file)
            .args(["--board", board, "--address", &format!("{ROM_START:#x}")])
            .arg(kernel))?;
        run(Command::new("tockloader")
            .arg("install")
            .arg("--flash-file")
            .arg(&flash_file)
            .args([
                "--board",
                board,
                "--app-address",
                &format!("{APP_START:#x}"),
            ])
            .args(apps))?;

        let mut ram = std::fs::read(&flash_file)
            .map_err(|e| format!("failed to read assembled flash file: {e}"))?;
        let max_len = (PROG_END - ROM_START) as usize;
        ram.resize((ram.len() + APPS_TERMINATOR).min(max_len), 0xff);
        std::fs::write(&image, esp_image(ROM_START, &ram)).map_err(|e| e.to_string())?;

        // esptool needs the port, and resets the chip into download mode
        // itself. It leaves it there, for `boot` to reset once the console
        // is open.
        self.console = None;
        let (device, _) = self.dut.uart_console()?;
        run(Command::new("esptool")
            .args(["--chip", "esp32c3", "--port", &device, "--baud", FLASH_BAUD])
            .args(["--before", "default-reset", "--after", "no-reset"])
            .args([
                "write-flash",
                "--flash-mode",
                "keep",
                "--flash-freq",
                "keep",
            ])
            .args(["--flash-size", "keep", "0x0"])
            .arg(&image))?;

        self.boot(Vec::new())
    }

    fn restart(&mut self) -> Result<(), String> {
        let transcript = self
            .console
            .take()
            .map_or_else(Vec::new, SerialUart::into_transcript);
        self.boot(transcript)
    }

    fn reset(&mut self) -> Result<(), String> {
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

/// An ESP32-C3 ROM-loadable image of `data`, loaded to and entered at `addr`,
/// as `esptool elf2image --flash-mode dio --flash-freq 80m --flash-size 4MB
/// --dont-append-digest` makes of an ELF file with one segment.
fn esp_image(addr: u32, data: &[u8]) -> Vec<u8> {
    let mut segment = data.to_vec();
    segment.resize(data.len().next_multiple_of(4), 0);

    // Magic, segment count, SPI mode (DIO), flash size (4MB) and frequency
    // (80 MHz), entry point.
    let mut image = vec![0xe9, 1, 0x02, 0x2f];
    image.extend_from_slice(&addr.to_le_bytes());
    // Extended header: WP pin (unused), SPI pin drive strengths, chip ID (5,
    // the ESP32-C3), minimum chip revision (0, and 0.0), maximum chip
    // revision (any), reserved, no SHA-256 digest appended.
    image.extend_from_slice(&[0xee, 0, 0, 0]);
    image.extend_from_slice(&5u16.to_le_bytes());
    image.extend_from_slice(&[0, 0, 0, 0xff, 0xff, 0, 0, 0, 0, 0]);

    image.extend_from_slice(&addr.to_le_bytes());
    image.extend_from_slice(&(segment.len() as u32).to_le_bytes());
    image.extend_from_slice(&segment);

    // Pad so that the checksum byte ends a 16-byte block.
    image.resize((image.len() + 1).next_multiple_of(16) - 1, 0);
    image.push(segment.iter().fold(0xef, |sum, byte| sum ^ byte));
    image
}

#[cfg(test)]
mod tests {
    use super::esp_image;

    #[test]
    fn esp_image_matches_esptool() {
        // As made by esptool v5.4.0 from an ELF of `data` at 0x40380000.
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
