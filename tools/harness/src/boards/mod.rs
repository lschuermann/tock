// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

pub mod esp32_c3;
pub mod linux_gpiochip;
pub mod nrf52840dk;
pub mod nucleo_f429zi;
pub mod qemu_virt;
pub mod rp2xxx;
pub mod serial_uart;

use crate::board::Gpio;
use crate::hostspec::GpioPinSpec;
use linux_gpiochip::LinuxGpiochip;

/// Opens the host-side driver for a host-spec GPIO pin, or returns `None` if
/// this harness does not support the driver of the pin's controller.
pub fn open_gpio(pin: &GpioPinSpec) -> Option<Result<Box<dyn Gpio>, String>> {
    let controller = pin.controller_spec.as_ref()?;
    match controller.driver.as_str() {
        "linux-gpiochip" => Some(
            LinuxGpiochip::new(&controller.config, pin).map(|gpio| Box::new(gpio) as Box<dyn Gpio>),
        ),
        _ => None,
    }
}

/// How many of a failed tool's last output lines its error includes.
const FAILURE_CONTEXT_LINES: usize = 15;

/// Run a tool (tockloader, openocd, ...), logging its output at debug level
/// rather than passing it through. If it fails, the error includes the end of
/// its output.
pub fn run(cmd: &mut std::process::Command) -> Result<(), String> {
    let program = cmd.get_program().to_string_lossy().into_owned();
    log::debug!("  running {cmd:?}");
    let output = cmd
        .stdin(std::process::Stdio::null())
        .output()
        .map_err(|e| format!("failed to run {program}: {e}"))?;
    let text = String::from_utf8_lossy(&output.stdout) + String::from_utf8_lossy(&output.stderr);
    let lines: Vec<&str> = text.lines().collect();
    for line in &lines {
        log::debug!("  {program} | {line}");
    }
    if output.status.success() {
        return Ok(());
    }
    let tail = &lines[lines.len().saturating_sub(FAILURE_CONTEXT_LINES)..];
    Err(format!(
        "{program} failed ({}):\n{}",
        output.status,
        tail.join("\n")
    ))
}
