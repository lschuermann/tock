// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

//! Raspberry Pi Pico (RP2040) and Pico 2 (RP2350) boards, flashed over SWD
//! through a CMSIS-DAP probe, and reset through their RUN pin if a host spec
//! wires it, or over SWD otherwise.

use crate::boards::helpers::dut_gpios::open_gpio;
use crate::boards::helpers::serial::SerialUart;
use crate::boards::helpers::{command, openocd, tockloader};
use crate::boards::{BOARDS, Board, Dut, DutConfig, Feature, Gpio, GpioMode, Open, Timing, Uart};
use anyhow::{Context, Result, anyhow};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{Duration, Instant};

/// Start of the flash, which holds the kernel (see
/// `boards/raspberry_pi_pico{,_2}/layout.ld`).
const FLASH_START: u64 = 0x1000_0000;
/// Start of the app `prog` region.
const APP_START: u64 = 0x1004_0000;
/// Combined length of the kernel and app `prog` regions.
const IMAGE_LEN: usize = 0x8_0000;

/// Maximum time the USB CDC console takes to enumerate after a reset.
const CDC_ENUMERATION_TIMEOUT: Duration = Duration::from_secs(10);
/// Maximum time the USB CDC console of the kernel before a reset takes to
/// disappear. The console may also be gone already, or never have existed.
const CDC_DISCONNECT_TIMEOUT: Duration = Duration::from_secs(2);
/// Duration for which a reset holds RUN low.
const RUN_PULSE: Duration = Duration::from_millis(10);
/// Baud rate of the board's UART console.
const BAUD: u32 = 115_200;

/// Host spec `gpio` key of the RUN pin, which resets the chip while it is low.
const RUN: &str = "RUN";
/// Host spec `gpio` key of the BOOTSEL pin (QSPI_SS), which is active low.
///
/// While BOOTSEL is held low through a reset, the chip boots into the USB
/// bootloader of its bootrom.
const BOOTSEL: &str = "BOOTSEL";
/// USB vendor ID of the USB bootloader of the bootrom ("RP2 Boot" on the
/// RP2040).
const BOOTROM_USB_VID: &str = "2e8a";
/// USB product IDs of the USB bootloader of the bootrom.
const BOOTROM_USB_PIDS: &[&str] = &["0003", "000f"];

/// Raspberry Pi Pico, whose kernel uses its USB CDC-ACM device as the console.
///
/// A kernel with another console configuration needs a board of its own.
#[linkme::distributed_slice(BOARDS)]
pub static PICO: Board = Board {
    name: "raspberry_pi_pico",
    dir: "boards/raspberry_pi_pico",
    kernel: "target/thumbv6m-none-eabi/release/raspberry_pi_pico.bin",
    tock_targets: &["cortex-m0"],
    pins: &[],
    features: &[Feature::UsbConsole],
    timing: Timing {
        flash: Duration::from_secs(45),
        restart: Duration::from_secs(15),
        boot: Duration::from_secs(5),
        expect: Duration::from_secs(3),
    },
    open: Open::Physical(|dut| Box::new(Rp2xxx::new(&PICO, Chip::Rp2040, Console::UsbCdc, dut))),
};

/// Raspberry Pi Pico 2, whose kernel uses UART0 as the console.
///
/// A kernel with another console configuration needs a board of its own.
#[linkme::distributed_slice(BOARDS)]
pub static PICO_2: Board = Board {
    name: "raspberry_pi_pico_2",
    dir: "boards/raspberry_pi_pico_2",
    kernel: "target/thumbv8m.main-none-eabi/release/raspberry_pi_pico_2.bin",
    // The Cortex-M33 of the RP2350 runs cortex-m4 builds:
    tock_targets: &["cortex-m4"],
    pins: &[],
    features: &[],
    timing: Timing {
        flash: Duration::from_secs(45),
        restart: Duration::from_secs(15),
        boot: Duration::from_secs(5),
        expect: Duration::from_secs(3),
    },
    open: Open::Physical(|dut| Box::new(Rp2xxx::new(&PICO_2, Chip::Rp2350, Console::Uart, dut))),
};

/// Chip of a Raspberry Pi Pico board.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Chip {
    Rp2040,
    Rp2350,
}

impl Chip {
    /// Return the OpenOCD configuration of the chip.
    fn openocd_target(self) -> &'static str {
        match self {
            Chip::Rp2040 => "target/rp2040.cfg",
            Chip::Rp2350 => "target/rp2350.cfg",
        }
    }

    /// Return the name of the chip's board in tockloader.
    fn tockloader_board(self) -> &'static str {
        match self {
            Chip::Rp2040 => "raspberry_pi_pico",
            Chip::Rp2350 => "raspberry_pi_pico_2",
        }
    }

    /// Return the OpenOCD commands which reset the whole chip through the
    /// watchdog, like pulsing RUN.
    ///
    /// OpenOCD's `reset` only resets the cores, which leaves USB enumerated
    /// (among others). The DAP link drops with the reset, such that the final
    /// write may report an error.
    fn watchdog_reset(self) -> [String; 3] {
        // Addresses of WATCHDOG_BASE and PSM_BASE, and the value of PSM_WDSEL
        // which selects everything except the oscillators:
        let (watchdog, psm, wdsel) = match self {
            Chip::Rp2040 => (0x4005_8000u32, 0x4001_0000u32, 0x0001_fffcu32),
            Chip::Rp2350 => (0x400d_8000, 0x4001_8000, 0x01ff_fff3),
        };
        [
            // Clear SCRATCH4, the watchdog boot magic of the bootrom, such that
            // the chip boots from flash:
            format!("write_memory {:#x} 32 0", watchdog + 0x1c),
            format!("write_memory {:#x} 32 {wdsel:#x}", psm + 0x8),
            // Set WATCHDOG_CTRL.TRIGGER:
            format!("catch {{write_memory {watchdog:#x} 32 0x80000000}}"),
        ]
    }
}

/// Location of the kernel's console.
///
/// This is a fixed property of the kernel's configuration, which each board of
/// this backend defines.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Console {
    /// UART console of the board (e.g., UART0 through the debug probe). This
    /// console is preferable, as it keeps working through panics.
    Uart,
    /// The board's own USB CDC-ACM device, which only exists once the kernel
    /// has booted and enumerated it. Output before that is lost.
    UsbCdc,
}

/// Host lines to the RUN and BOOTSEL pins, if the host spec wires them.
///
/// Once opened, the lines are held for the lifetime of the board, with BOOTSEL
/// released. Dropping the lines returns them to inputs.
#[derive(Default)]
struct ControlPins {
    opened: bool,
    run: Option<Box<dyn Gpio>>,
    bootsel: Option<Box<dyn Gpio>>,
}

impl ControlPins {
    /// Open the pins, unless they are open already.
    ///
    /// This releases BOOTSEL first, and leaves RUN alone until a reset. Thus, a
    /// floating host line, or a previous user which left BOOTSEL asserted,
    /// cannot send the next boot into the bootloader.
    fn open(&mut self, dut: &DutConfig) -> Result<()> {
        if self.opened {
            return Ok(());
        }
        // Configuring an output drives the DUT pin low, which asserts it.
        // Thus, BOOTSEL is released right after. RUN is only configured to
        // reset the chip:
        self.bootsel = open_control_pin(dut, BOOTSEL)?;
        if let Some(bootsel) = &mut self.bootsel {
            bootsel.set_mode(GpioMode::DigitalOut)?;
            bootsel.write(true)?;
        }
        self.run = open_control_pin(dut, RUN)?;
        self.opened = true;
        Ok(())
    }

    /// Reset the chip by pulsing RUN, which boots it from flash. This returns
    /// `Ok(false)` if RUN is not wired.
    fn reset(&mut self) -> Result<bool> {
        let Some(run) = &mut self.run else {
            return Ok(false);
        };
        log::debug!("  pulsing RUN");
        run.set_mode(GpioMode::DigitalOut)?;
        run.write(false)?;
        std::thread::sleep(RUN_PULSE);
        run.write(true)?;
        Ok(true)
    }
}

/// Open the host line to the DUT's control pin `name`, if the host spec wires
/// it for output on a controller which the harness supports.
fn open_control_pin(dut: &DutConfig, name: &str) -> Result<Option<Box<dyn Gpio>>> {
    let Some(pin) = dut.gpio.get(name) else {
        return Ok(None);
    };
    if !pin.modes.iter().any(|m| m == GpioMode::DigitalOut.as_str()) {
        log::warn!("{name} is not wired for digital_out, so the harness does not drive it");
        return Ok(None);
    }
    match open_gpio(pin) {
        Some(gpio) => gpio
            .map(Some)
            .with_context(|| format!("failed to open {name}")),
        None => {
            log::warn!("{name} is on a GPIO controller which the harness cannot drive");
            Ok(None)
        }
    }
}

pub struct Rp2xxx {
    board: &'static Board,
    chip: Chip,
    dut: DutConfig,
    console_kind: Console,
    console: Option<SerialUart>,
    control: ControlPins,
}

impl Rp2xxx {
    pub fn new(board: &'static Board, chip: Chip, console_kind: Console, dut: DutConfig) -> Self {
        Rp2xxx {
            board,
            chip,
            dut,
            console_kind,
            console: None,
            control: ControlPins::default(),
        }
    }

    /// Construct an OpenOCD invocation which runs `commands` on the board.
    fn openocd(&self, commands: &[&str]) -> Command {
        openocd::command(
            &["-f", "interface/cmsis-dap.cfg"],
            self.dut.probe.as_deref(),
            &["-f", self.chip.openocd_target(), "-c", "adapter speed 5000"],
            commands,
        )
    }

    /// Reset the chip, and open its console, which continues `transcript`.
    fn boot(&mut self, transcript: Vec<u8>) -> Result<()> {
        match self.console_kind {
            Console::Uart => {
                // Open the console before the reset, such that no output is
                // lost:
                let device = self.dut.console()?;
                self.console = Some(SerialUart::open_continuing(device, BAUD, transcript)?);
                self.chip_reset()
            }
            Console::UsbCdc => {
                self.console = None;
                self.chip_reset()?;
                self.console =
                    Some(
                        open_cdc_device(transcript).map_err(|e| match bootrom_usb_present() {
                            true => e.context("the chip is in its USB bootloader (BOOTSEL)"),
                            false => e,
                        })?,
                    );
                Ok(())
            }
        }
    }

    /// Reset the whole chip through RUN if it is wired, and through its
    /// watchdog otherwise.
    fn chip_reset(&mut self) -> Result<()> {
        self.control.open(&self.dut)?;
        if self.control.reset()? {
            return Ok(());
        }
        let reset = self.chip.watchdog_reset();
        let mut commands = vec!["init"];
        commands.extend(reset.iter().map(String::as_str));
        command::run(&mut self.openocd(&commands))
    }
}

impl Dut for Rp2xxx {
    fn board(&self) -> &'static Board {
        self.board
    }

    fn flash(&mut self, kernel: &Path, apps: &[&Path]) -> Result<()> {
        // Release BOOTSEL before OpenOCD resets the chip to program it:
        self.control.open(&self.dut)?;
        let dir = tempfile::tempdir()?;
        let flash_file = dir.path().join("flash.img");
        let image = dir.path().join("image.bin");
        let board = self.chip.tockloader_board();

        tockloader::write_flash_file(
            &flash_file,
            board,
            FLASH_START,
            Some(APP_START),
            kernel,
            apps,
        )?;
        // The flash file starts at `FLASH_START`, which is tockloader's
        // `flash_address` for both boards:
        let region = tockloader::read_flash_region(&flash_file, 0, IMAGE_LEN)?;
        fs_err::write(&image, &region)?;

        command::run(&mut self.openocd(&[&format!(
            "program {} verify {FLASH_START:#x}",
            image.display()
        )]))?;

        self.boot(Vec::new())
    }

    fn restart(&mut self) -> Result<()> {
        let transcript = (self.console.take()).map_or_else(Vec::new, SerialUart::into_transcript);
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

/// Find the device of the kernel's USB CDC-ACM console, whose product string
/// is "... - TockOS" on the Raspberry Pi Pico boards.
fn find_cdc_device() -> Option<PathBuf> {
    std::fs::read_dir("/dev/serial/by-id").ok().and_then(|dir| {
        dir.filter_map(|e| e.ok())
            .map(|e| e.path())
            .find(|p| p.to_string_lossy().contains("TockOS"))
    })
}

/// Check if the USB bootloader of a bootrom is enumerated on the host.
///
/// The bootloader cannot be associated with this DUT, as it does not report
/// the serial number of the chip's flash. Thus, this is only a hint.
fn bootrom_usb_present() -> bool {
    let read = |dir: &Path, attr: &str| std::fs::read_to_string(dir.join(attr)).ok();
    std::fs::read_dir("/sys/bus/usb/devices").is_ok_and(|dir| {
        dir.filter_map(|e| e.ok()).any(|e| {
            let dev = e.path();
            read(&dev, "idVendor").is_some_and(|v| v.trim() == BOOTROM_USB_VID)
                && read(&dev, "idProduct").is_some_and(|p| BOOTROM_USB_PIDS.contains(&p.trim()))
        })
    })
}

/// Open the kernel's USB CDC-ACM console after a reset, which continues
/// `transcript`.
///
/// The device of the kernel before the reset has the same name, and may still
/// exist. Thus, this first waits briefly for the old device to disappear, and
/// then for the new one to enumerate. Opening the device may fail while udev
/// is still setting it up, so opening is retried as well.
fn open_cdc_device(transcript: Vec<u8>) -> Result<SerialUart> {
    let start = Instant::now();
    while find_cdc_device().is_some() && start.elapsed() < CDC_DISCONNECT_TIMEOUT {
        std::thread::sleep(Duration::from_millis(20));
    }
    let start = Instant::now();
    let mut last_error = None;
    loop {
        if let Some(path) = find_cdc_device() {
            match SerialUart::open_continuing(&path.to_string_lossy(), BAUD, transcript.clone()) {
                Ok(console) => return Ok(console),
                Err(e) => last_error = Some(e),
            }
        }
        if start.elapsed() > CDC_ENUMERATION_TIMEOUT {
            return Err(match last_error {
                Some(e) => e.context(format!(
                    "the TockOS USB CDC device appeared, but failed to open within \
                     {CDC_ENUMERATION_TIMEOUT:?}"
                )),
                None => {
                    anyhow!("no TockOS USB CDC device appeared within {CDC_ENUMERATION_TIMEOUT:?}")
                }
            });
        }
        std::thread::sleep(Duration::from_millis(50));
    }
}
