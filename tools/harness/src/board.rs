// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use std::path::Path;
use std::time::Duration;

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, serde::Serialize, serde::Deserialize,
)]
pub enum BoardKind {
    QemuVirt,
    Nrf52840Dk,
    NucleoF429zi,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum GpioMode {
    DigitalIn,
    DigitalOut,
}

pub trait Uart {
    fn write(&mut self, bytes: &[u8]) -> Result<(), String>;
    fn wait_for(&mut self, needle: &str, timeout: Duration) -> Result<String, String>;
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

pub trait Gpio {
    fn set_mode(&mut self, mode: GpioMode) -> Result<(), String>;
    fn write(&mut self, high: bool) -> Result<(), String>;
    fn read(&mut self) -> Result<bool, String>;
}

pub trait Board {
    fn kind(&self) -> BoardKind;
    fn flash(&mut self, kernel: &Path, apps: &[&Path]) -> Result<(), String>;
    fn reset(&mut self) -> Result<(), String>;
    fn uart(&mut self) -> Option<&mut dyn Uart>;
    fn gpio(&mut self, name: &str) -> Option<&mut dyn Gpio>;
}
