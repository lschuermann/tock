// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use crate::board::{Uart, take_through, timeout_error};
use std::io::{Read, Write};
use std::time::{Duration, Instant};

pub struct SerialUart {
    port: Box<dyn serialport::SerialPort>,
    buf: Vec<u8>,
    received: Vec<u8>,
}

impl SerialUart {
    pub fn open(device: &str, baud: u32) -> Result<Self, String> {
        Self::open_continuing(device, baud, Vec::new())
    }

    /// Open `device`, continuing the `transcript` of a console that was
    /// closed, e.g. while its board restarted.
    pub fn open_continuing(device: &str, baud: u32, transcript: Vec<u8>) -> Result<Self, String> {
        let port = serialport::new(device, baud)
            .timeout(Duration::from_millis(200))
            .open()
            .map_err(|e| e.to_string())?;
        port.clear(serialport::ClearBuffer::Input)
            .map_err(|e| e.to_string())?;
        Ok(SerialUart {
            port,
            buf: Vec::new(),
            received: transcript,
        })
    }

    /// Set the port's DTR and RTS lines, which some boards wire to their
    /// reset and boot-mode pins.
    pub fn set_control_lines(&mut self, dtr: bool, rts: bool) -> Result<(), String> {
        self.port
            .write_data_terminal_ready(dtr)
            .and_then(|()| self.port.write_request_to_send(rts))
            .map_err(|e| format!("failed to set DTR/RTS: {e}"))
    }

    /// Close the console, returning everything it received.
    pub fn into_transcript(self) -> Vec<u8> {
        self.received
    }
}

impl Uart for SerialUart {
    fn send(&mut self, bytes: &[u8]) -> Result<(), String> {
        self.port.write_all(bytes).map_err(|e| e.to_string())
    }

    fn receive_until(&mut self, needle: &str, timeout: Duration) -> Result<String, String> {
        let deadline = Instant::now() + timeout;
        let mut chunk = [0u8; 256];
        while Instant::now() < deadline {
            match self.port.read(&mut chunk) {
                Ok(n) if n > 0 => {
                    self.buf.extend_from_slice(&chunk[..n]);
                    self.received.extend_from_slice(&chunk[..n]);
                }
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

    fn transcript(&self) -> &[u8] {
        &self.received
    }
}
