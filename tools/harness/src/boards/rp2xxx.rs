// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

//! Raspberry Pi Pico (RP2040) and Pico 2 (RP2350) boards, flashed and reset
//! over SWD through a CMSIS-DAP probe.

use crate::board::{Board, BoardKind, Gpio, Uart};
use crate::boards::run;
use crate::boards::serial_uart::SerialUart;
use crate::hostspec::DutSpec;
use std::io::{Read, Seek, SeekFrom};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{Duration, Instant};

// Kernel and app `prog` regions from boards/raspberry_pi_pico{,_2}/layout.ld.
const FLASH_START: u64 = 0x1000_0000;
const APP_START: u64 = 0x1004_0000;
const IMAGE_LEN: usize = 0x8_0000;

/// How long a USB CDC console may take to enumerate after a reset.
pub const CDC_ENUMERATION_TIMEOUT: Duration = Duration::from_secs(10);
/// How long the USB CDC console of the kernel before a reset may take to
/// disappear. It may already be gone, or never have been there.
const CDC_DISCONNECT_TIMEOUT: Duration = Duration::from_secs(2);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Chip {
    Rp2040,
    Rp2350,
}

impl Chip {
    fn openocd_target(self) -> &'static str {
        match self {
            Chip::Rp2040 => "target/rp2040.cfg",
            Chip::Rp2350 => "target/rp2350.cfg",
        }
    }

    /// The MCU address at the start of a tockloader flash file: its
    /// `flash_address` for the board, or 0 (absolute addresses as offsets)
    /// for boards that declare none, like the Pico.
    fn flash_file_base(self) -> u64 {
        match self {
            Chip::Rp2040 => 0,
            Chip::Rp2350 => FLASH_START,
        }
    }

    fn tockloader_board(self) -> &'static str {
        match self {
            Chip::Rp2040 => "raspberry_pi_pico",
            Chip::Rp2350 => "raspberry_pi_pico_2",
        }
    }

    /// Resets the whole chip through the watchdog, like pulsing RUN.
    /// OpenOCD's `reset` only resets the cores, which e.g. leaves USB
    /// enumerated. The DAP link drops with the reset, so the final write may
    /// report an error.
    fn watchdog_reset(self) -> [String; 3] {
        // (WATCHDOG_BASE, PSM_BASE, PSM_WDSEL with everything except the
        // oscillators)
        let (watchdog, psm, wdsel) = match self {
            Chip::Rp2040 => (0x4005_8000u32, 0x4001_0000u32, 0x0001_fffcu32),
            Chip::Rp2350 => (0x400d_8000, 0x4001_8000, 0x01ff_fff3),
        };
        [
            // Clear SCRATCH4, the bootrom's watchdog boot magic, so we boot
            // from flash.
            format!("write_memory {:#x} 32 0", watchdog + 0x1c),
            format!("write_memory {:#x} 32 {wdsel:#x}", psm + 0x8),
            // WATCHDOG_CTRL.TRIGGER
            format!("catch {{write_memory {watchdog:#x} 32 0x80000000}}"),
        ]
    }
}

/// Where the kernel's console is: a fixed property of the kernel's
/// configuration, which each board kind using this backend chooses.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Console {
    /// The host spec's console, e.g. UART0 through the debug probe. Prefer
    /// this: it keeps working through panics.
    HostSpec,
    /// The board's own USB CDC-ACM device, which only exists once the kernel
    /// has booted and enumerated. Output before that is lost.
    UsbCdc,
}

pub struct Rp2xxx {
    chip: Chip,
    dut: DutSpec,
    console_kind: Console,
    console: Option<SerialUart>,
}

impl Rp2xxx {
    pub fn new(chip: Chip, console_kind: Console, dut: DutSpec) -> Self {
        Rp2xxx {
            chip,
            dut,
            console_kind,
            console: None,
        }
    }

    fn openocd(&self, commands: &[&str]) -> Result<(), String> {
        let mut cmd = Command::new("openocd");
        cmd.args(["-f", "interface/cmsis-dap.cfg"]);
        if let Some(serial) = self.dut.probe_serial() {
            cmd.args(["-c", &format!("adapter serial {serial}")]);
        }
        cmd.args(["-f", self.chip.openocd_target(), "-c", "adapter speed 5000"]);
        for command in commands {
            cmd.args(["-c", command]);
        }
        cmd.args(["-c", "exit"]);
        run(&mut cmd)
    }

    /// Reset the chip, and open its console, continuing `transcript`.
    fn boot(&mut self, transcript: Vec<u8>) -> Result<(), String> {
        match self.console_kind {
            Console::HostSpec => {
                // Open the console before the reset, so no output is lost.
                let (device, baud) = self.dut.uart_console()?;
                self.console = Some(SerialUart::open_continuing(&device, baud, transcript)?);
                self.watchdog_reset()
            }
            Console::UsbCdc => {
                self.console = None;
                self.watchdog_reset()?;
                self.console = Some(open_cdc_device(transcript)?);
                Ok(())
            }
        }
    }

    fn watchdog_reset(&self) -> Result<(), String> {
        let reset = self.chip.watchdog_reset();
        let mut commands = vec!["init"];
        commands.extend(reset.iter().map(String::as_str));
        self.openocd(&commands)
    }
}

impl Board for Rp2xxx {
    fn kind(&self) -> BoardKind {
        match self.chip {
            Chip::Rp2040 => BoardKind::RaspberryPiPico,
            Chip::Rp2350 => BoardKind::RaspberryPiPico2,
        }
    }

    fn flash(&mut self, kernel: &Path, apps: &[&Path]) -> Result<(), String> {
        let dir = tempfile::tempdir().map_err(|e| e.to_string())?;
        let flash_file = dir.path().join("flash.img");
        let image = dir.path().join("image.bin");
        let board = self.chip.tockloader_board();

        run(Command::new("tockloader")
            .arg("flash")
            .arg("--flash-file")
            .arg(&flash_file)
            .args(["--board", board, "--address", &format!("{FLASH_START:#x}")])
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

        // Take the image at FLASH_START out of the flash file. Pad it with
        // erased flash, so the kernel does not find stale apps behind the new
        // ones.
        let mut region = Vec::with_capacity(IMAGE_LEN);
        std::fs::File::open(&flash_file)
            .and_then(|mut f| {
                f.seek(SeekFrom::Start(FLASH_START - self.chip.flash_file_base()))?;
                f.take(IMAGE_LEN as u64).read_to_end(&mut region)
            })
            .map_err(|e| format!("failed to read assembled flash file: {e}"))?;
        region.resize(IMAGE_LEN, 0xff);
        std::fs::write(&image, &region).map_err(|e| e.to_string())?;

        self.openocd(&[&format!(
            "program {} verify {FLASH_START:#x}",
            image.display()
        )])?;

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

/// The kernel's USB CDC-ACM console, whose product string is "... - TockOS"
/// on the Raspberry Pi Pico boards, if it is present.
fn find_cdc_device() -> Option<PathBuf> {
    std::fs::read_dir("/dev/serial/by-id").ok().and_then(|dir| {
        dir.filter_map(|e| e.ok())
            .map(|e| e.path())
            .find(|p| p.to_string_lossy().contains("TockOS"))
    })
}

/// Opens the kernel's USB CDC-ACM console after a reset, continuing
/// `transcript`.
///
/// The device of the kernel before the reset has the same name, and may not
/// have gone yet: first wait (briefly) for it to disappear, then for the new
/// one to enumerate. Opening may fail while udev is still setting it up, so
/// that is retried too.
fn open_cdc_device(transcript: Vec<u8>) -> Result<SerialUart, String> {
    let start = Instant::now();
    while find_cdc_device().is_some() && start.elapsed() < CDC_DISCONNECT_TIMEOUT {
        std::thread::sleep(Duration::from_millis(20));
    }
    let start = Instant::now();
    let mut last_error = None;
    loop {
        if let Some(path) = find_cdc_device() {
            match SerialUart::open_continuing(&path.to_string_lossy(), 115200, transcript.clone()) {
                Ok(console) => return Ok(console),
                Err(e) => last_error = Some(e),
            }
        }
        if start.elapsed() > CDC_ENUMERATION_TIMEOUT {
            return Err(match last_error {
                Some(e) => format!(
                    "the TockOS USB CDC device appeared, but failed to open within \
                     {CDC_ENUMERATION_TIMEOUT:?}: {e}"
                ),
                None => {
                    format!("no TockOS USB CDC device appeared within {CDC_ENUMERATION_TIMEOUT:?}")
                }
            });
        }
        std::thread::sleep(Duration::from_millis(50));
    }
}
