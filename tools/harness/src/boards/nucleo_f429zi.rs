// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use crate::board::{Board, BoardKind, Gpio, Uart};
use crate::boards::open_gpio;
use crate::boards::serial_uart::SerialUart;
use crate::hostspec::DutSpec;
use std::collections::BTreeMap;
use std::io::{Read, Seek, SeekFrom};
use std::path::Path;
use std::process::Command;

const PIN_MAP: &[(&str, &str)] = &[
    ("led0", "PB0"),
    ("led1", "PB7"),
    // User button B1 is active-high, unlike the nRF52840DK's buttons.
    ("button0", "PC13"),
    // Userspace GPIO pin 0 (Arduino D0).
    ("gpio0", "PG9"),
];

// Kernel `rom` + app `prog` regions from boards/nucleo_f429zi/chip_layout.ld.
const FLASH_START: u64 = 0x0800_0000;
const IMAGE_LEN: usize = 0x8_0000;

pub struct NucleoF429zi {
    dut: DutSpec,
    console: Option<SerialUart>,
    gpio_pins: BTreeMap<String, Box<dyn Gpio>>,
}

impl NucleoF429zi {
    pub fn new(dut: DutSpec) -> Self {
        NucleoF429zi {
            dut,
            console: None,
            gpio_pins: BTreeMap::new(),
        }
    }

    fn probe_serial(&self) -> Option<&str> {
        self.dut.debug.probe.serial.as_deref()
    }

    fn openocd(&self, commands: &[&str]) -> Result<(), String> {
        let mut cmd = Command::new("openocd");
        cmd.args(["-f", "board/st_nucleo_f4.cfg"]);
        if let Some(serial) = self.probe_serial() {
            cmd.args(["-c", &format!("adapter serial {serial}")]);
        }
        // Attaching to running firmware that sleeps without debug-in-sleep
        // enabled fails examination; holding NRST makes attach independent
        // of whatever is currently flashed.
        cmd.args([
            "-c",
            "reset_config srst_only srst_nogate connect_assert_srst",
        ]);
        for command in commands {
            cmd.args(["-c", command]);
        }
        cmd.args(["-c", "exit"]);
        run(&mut cmd)
    }
}

impl Board for NucleoF429zi {
    fn kind(&self) -> BoardKind {
        BoardKind::NucleoF429zi
    }

    // TODO: replace the flash-file assembly with plain `tockloader flash` +
    // `tockloader install --erase --openocd` once tockloader stops wiping apps
    // on STM32F4: it read-modify-writes in 2 KiB `page_size` blocks, but
    // OpenOCD's `program` erases whole (up to 128 KiB) sectors, so its trailing
    // padding write erases the app it just wrote.
    fn flash(&mut self, kernel: &Path, apps: &[&Path]) -> Result<(), String> {
        let dir = tempfile::tempdir().map_err(|e| e.to_string())?;
        let flash_file = dir.path().join("flash.img");
        let image = dir.path().join("image.bin");

        run(Command::new("tockloader")
            .arg("flash")
            .arg("--flash-file")
            .arg(&flash_file)
            .args([
                "--board",
                "nucleof4",
                "--address",
                &format!("{FLASH_START:#x}"),
            ])
            .arg(kernel))?;
        run(Command::new("tockloader")
            .arg("install")
            .arg("--flash-file")
            .arg(&flash_file)
            .args(["--board", "nucleof4"])
            .args(apps))?;

        // Without a `flash_address` for this board, tockloader uses absolute
        // MCU addresses as flash-file offsets.
        let mut region = Vec::with_capacity(IMAGE_LEN);
        std::fs::File::open(&flash_file)
            .and_then(|mut f| {
                f.seek(SeekFrom::Start(FLASH_START))?;
                f.take(IMAGE_LEN as u64).read_to_end(&mut region)
            })
            .map_err(|e| format!("failed to read assembled flash file: {e}"))?;
        region.resize(IMAGE_LEN, 0xff);
        std::fs::write(&image, &region).map_err(|e| e.to_string())?;

        self.openocd(&[&format!(
            "program {} verify {FLASH_START:#x}",
            image.display()
        )])?;

        let (device, baud) = {
            let console = self
                .dut
                .console
                .as_ref()
                .ok_or("DUT has no console configured")?;
            (console.device.clone(), console.baud)
        };
        self.console = Some(SerialUart::open(&device, baud)?);

        self.openocd(&["init", "reset run"])
    }

    fn reset(&mut self) -> Result<(), String> {
        self.console = None;
        self.gpio_pins.clear();
        Ok(())
    }

    fn uart(&mut self) -> Option<&mut dyn Uart> {
        self.console.as_mut().map(|c| c as &mut dyn Uart)
    }

    fn gpio(&mut self, name: &str) -> Option<&mut dyn Gpio> {
        let pin_name = PIN_MAP.iter().find(|(n, _)| *n == name)?.1;
        if !self.gpio_pins.contains_key(name) {
            let pin = open_gpio(self.dut.gpio.get(pin_name)?)?;
            self.gpio_pins.insert(name.to_string(), pin);
        }
        let pin: &mut dyn Gpio = self.gpio_pins.get_mut(name)?.as_mut();
        Some(pin)
    }
}

fn run(cmd: &mut Command) -> Result<(), String> {
    let status = cmd.status().map_err(|e| e.to_string())?;
    if status.success() {
        Ok(())
    } else {
        Err(format!("{cmd:?} failed: {status}"))
    }
}
