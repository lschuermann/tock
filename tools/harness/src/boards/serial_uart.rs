// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use crate::board::{Uart, take_through, timeout_error};
use std::io::{Read, Write};
use std::time::{Duration, Instant};

pub struct SerialUart {
    port: Box<dyn serialport::SerialPort>,
    buf: Vec<u8>,
}

impl SerialUart {
    pub fn open(device: &str, baud: u32) -> Result<Self, String> {
        let port = serialport::new(device, baud)
            .timeout(Duration::from_millis(200))
            .open()
            .map_err(|e| e.to_string())?;
        port.clear(serialport::ClearBuffer::Input)
            .map_err(|e| e.to_string())?;
        Ok(SerialUart {
            port,
            buf: Vec::new(),
        })
    }
}

impl Uart for SerialUart {
    fn write(&mut self, bytes: &[u8]) -> Result<(), String> {
        self.port.write_all(bytes).map_err(|e| e.to_string())
    }

    fn wait_for(&mut self, needle: &str, timeout: Duration) -> Result<String, String> {
        let deadline = Instant::now() + timeout;
        let mut chunk = [0u8; 256];
        while Instant::now() < deadline {
            match self.port.read(&mut chunk) {
                Ok(n) if n > 0 => self.buf.extend_from_slice(&chunk[..n]),
                Ok(_) => {}
                Err(e) if e.kind() == std::io::ErrorKind::TimedOut => {}
                Err(e) => return Err(e.to_string()),
            }
            if let Some(out) = take_through(&mut self.buf, needle) {
                return Ok(out);
            }
        }
        Err(timeout_error(&self.buf, needle))
    }
}
