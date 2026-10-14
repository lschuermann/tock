// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

//! NUCLEO-F429ZI, flashed through OpenOCD and its ST-LINK, with its console on
//! a serial port.

use crate::boards::helpers::dut_gpios::DutGpios;
use crate::boards::helpers::serial::SerialUart;
use crate::boards::helpers::{command, openocd, tockloader};
use crate::boards::{BOARDS, Board, Dut, DutConfig, Feature, Gpio, Open, Timing, Uart};
use anyhow::Result;
use std::path::Path;
use std::process::Command;
use std::time::Duration;

#[linkme::distributed_slice(BOARDS)]
pub static BOARD: Board = Board {
    name: "nucleo_f429zi",
    dir: "boards/nucleo_f429zi",
    kernel: "target/thumbv7em-none-eabi/release/nucleo_f429zi.bin",
    tock_targets: &["cortex-m4"],
    pins: &[
        ("led0", "PB0"),
        ("led1", "PB7"),
        // User button B1, which is active high:
        ("button0", "PC13"),
        // Userspace GPIO pin 0 (Arduino D0):
        ("gpio0", "PG9"),
    ],
    features: &[Feature::ArmV7mHardFault],
    timing: Timing {
        flash: Duration::from_secs(30),
        boot: Duration::from_secs(5),
        expect: Duration::from_secs(3),
    },
    open: Open::Physical(|dut| Box::new(NucleoF429zi::new(dut))),
};

/// Start of the flash, which holds the kernel `rom` and app `prog` regions (see
/// `boards/nucleo_f429zi/chip_layout.ld`).
const FLASH_START: u64 = 0x0800_0000;
/// Combined length of the kernel `rom` and app `prog` regions.
const IMAGE_LEN: usize = 0x8_0000;

/// Baud rate of the board's console.
const BAUD: u32 = 115_200;

pub struct NucleoF429zi {
    dut: DutConfig,
    console: Option<SerialUart>,
    gpios: DutGpios,
}

impl NucleoF429zi {
    pub fn new(dut: DutConfig) -> Self {
        NucleoF429zi {
            dut,
            console: None,
            gpios: DutGpios::default(),
        }
    }

    /// Construct an OpenOCD invocation which runs `commands` on the board.
    fn openocd(&self, commands: &[&str]) -> Command {
        // Attaching to running firmware which sleeps without debug-in-sleep
        // enabled fails the examination. Holding NRST makes attaching
        // independent of the currently flashed firmware:
        openocd::command(
            &["-f", "board/st_nucleo_f4.cfg"],
            self.dut.probe.as_deref(),
            &[
                "-c",
                "reset_config srst_only srst_nogate connect_assert_srst",
            ],
            commands,
        )
    }
}

impl Dut for NucleoF429zi {
    fn board(&self) -> &'static Board {
        &BOARD
    }

    // TODO: Replace the flash file assembly with `tockloader flash` and
    // `tockloader install --erase --openocd` once tockloader stops wiping apps
    // on the STM32F4. Tockloader read-modify-writes in 2 KiB `page_size`
    // blocks, while OpenOCD's `program` erases whole sectors of up to 128 KiB.
    // Thus, its trailing padding write erases the app it just wrote.
    fn flash(&mut self, kernel: &Path, apps: &[&Path]) -> Result<()> {
        let dir = tempfile::tempdir()?;
        let flash_file = dir.path().join("flash.img");
        let image = dir.path().join("image.bin");

        tockloader::write_flash_file(&flash_file, "nucleof4", FLASH_START, None, kernel, apps)?;
        // The flash file starts at `FLASH_START`, which is tockloader's
        // `flash_address` for this board:
        let region = tockloader::read_flash_region(&flash_file, 0, IMAGE_LEN)?;
        fs_err::write(&image, &region)?;

        command::run(&mut self.openocd(&[&format!(
            "program {} verify {FLASH_START:#x}",
            image.display()
        )]))?;

        self.console = Some(SerialUart::open(self.dut.console()?, BAUD)?);

        command::run(&mut self.openocd(&["init", "reset run"]))
    }

    fn reset(&mut self) -> Result<()> {
        self.console = None;
        self.gpios.close();
        Ok(())
    }

    fn uart(&mut self) -> Option<&mut dyn Uart> {
        self.console.as_mut().map(|c| c as &mut dyn Uart)
    }

    fn gpio(&mut self, name: &str) -> Option<&mut dyn Gpio> {
        self.gpios.get(&BOARD, &self.dut, name)
    }
}
