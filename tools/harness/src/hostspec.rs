// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use crate::boards::sysfs_gpio::SysfsGpioSpec;
use serde::Deserialize;
use std::collections::BTreeMap;
use std::path::Path;

#[derive(Debug, Clone, Deserialize)]
pub struct HostSpec {
    pub duts: Vec<DutSpec>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct DutSpec {
    pub board: String,
    pub debug: DebugSpec,
    pub console: Option<ConsoleSpec>,
    #[serde(default)]
    pub gpio: BTreeMap<String, GpioPinSpec>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct DebugSpec {
    pub protocol: String,
    pub probe: ProbeSpec,
}

#[derive(Debug, Clone, Deserialize)]
pub struct ProbeSpec {
    pub vendor: String,
    pub model: String,
    pub serial: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
pub struct ConsoleSpec {
    pub kind: String,
    pub device: String,
    pub baud: u32,
}

/// Host-spec GPIO entry, keyed by its `type` field.
#[derive(Debug, Clone, Deserialize)]
#[serde(tag = "type")]
pub enum GpioPinSpec {
    #[serde(rename = "sysfs-gpio")]
    Sysfs(SysfsGpioSpec),
    /// A GPIO type this harness does not know how to drive.
    #[serde(other)]
    Unsupported,
}

pub fn load(path: &Path) -> HostSpec {
    let data = std::fs::read_to_string(path).expect("failed to read host spec");
    serde_json::from_str(&data).expect("failed to parse host spec")
}
