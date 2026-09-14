// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use crate::board::{Board, BoardKind, Gpio, Uart};
use crate::boards::open_gpio;
use crate::boards::serial_uart::SerialUart;
use crate::hostspec::DutSpec;
use std::collections::BTreeMap;
use std::path::Path;
use std::process::Command;

const PIN_MAP: &[(&str, &str)] = &[
    ("led0", "P0.13"),
    ("led1", "P0.14"),
    ("button0", "P0.11"),
    ("button1", "P0.12"),
    // Userspace GPIO pin 0.
    ("gpio0", "P1.01"),
];

pub struct Nrf52840Dk {
    dut: DutSpec,
    console: Option<SerialUart>,
    gpio_pins: BTreeMap<String, Box<dyn Gpio>>,
}

impl Nrf52840Dk {
    pub fn new(dut: DutSpec) -> Self {
        Nrf52840Dk {
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
        cmd.args(["-f", "interface/jlink.cfg"]);
        if let Some(serial) = self.probe_serial() {
            cmd.args(["-c", &format!("adapter serial {serial}")]);
        }
        cmd.args([
            "-c",
            "transport select swd",
            "-f",
            "target/nordic/nrf52.cfg",
        ]);
        for command in commands {
            cmd.args(["-c", command]);
        }
        cmd.args(["-c", "exit"]);
        run(&mut cmd)
    }
}

impl Board for Nrf52840Dk {
    fn kind(&self) -> BoardKind {
        BoardKind::Nrf52840Dk
    }

    fn flash(&mut self, kernel: &Path, apps: &[&Path]) -> Result<(), String> {
        self.openocd(&[
            "init",
            "if {[nrf52.dap apreg 1 0xc] != 1} { nrf52_recover }",
            "if {[nrf52.dap apreg 1 0xc] != 1} { shutdown error }",
        ])
        .map_err(|e| format!("failed to unlock APPROTECT: {e}"))?;

        let mut flash_cmd = Command::new("tockloader");
        flash_cmd.args(["flash", "--board", "nrf52dk", "--openocd"]);
        if let Some(serial) = self.probe_serial() {
            flash_cmd.args(["--openocd-serial-number", serial]);
        }
        flash_cmd.args(["--address", "0x00000"]).arg(kernel);
        run(&mut flash_cmd)?;

        let mut install_cmd = Command::new("tockloader");
        install_cmd.args(["install", "--erase", "--board", "nrf52dk", "--openocd"]);
        if let Some(serial) = self.probe_serial() {
            install_cmd.args(["--openocd-serial-number", serial]);
        }
        install_cmd.args(apps);
        run(&mut install_cmd)?;

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
