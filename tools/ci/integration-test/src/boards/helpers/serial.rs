// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use super::console_buffer::ConsoleBuffer;
use crate::boards::Uart;
use anyhow::{Context, Result};
use std::io::{Read, Write};
use std::time::{Duration, Instant};

/// [`Uart`] backed by a physical board's serial port (exposed as a character
/// device or COM port).
pub struct SerialUart {
    port: Box<dyn serialport::SerialPort>,
    output: ConsoleBuffer,
}

impl SerialUart {
    /// Open the serial port `device` with the baud rate `baud`, and discard any
    /// input it has received before.
    pub fn open(device: &str, baud: u32) -> Result<Self> {
        Self::open_continuing(device, baud, Vec::new())
    }

    /// Open the serial port `device` like [`SerialUart::open`], and continue
    /// `transcript`, the output received by a previous console (e.g., before
    /// its board restarted).
    pub fn open_continuing(device: &str, baud: u32, transcript: Vec<u8>) -> Result<Self> {
        let port = serialport::new(device, baud)
            .timeout(Duration::from_millis(200))
            .open()
            .with_context(|| format!("failed to open {device}"))?;

        port.clear(serialport::ClearBuffer::Input)
            .with_context(|| format!("failed to clear the input of {device}"))?;

        Ok(SerialUart {
            port,
            output: ConsoleBuffer::with_transcript(transcript),
        })
    }

    /// Set the DTR and RTS lines of the port (e.g., for boards which wire them
    /// to their reset and boot mode pins).
    pub fn set_control_lines(&mut self, dtr: bool, rts: bool) -> Result<()> {
        self.port
            .write_data_terminal_ready(dtr)
            .and_then(|()| self.port.write_request_to_send(rts))
            .context("failed to set DTR/RTS")
    }

    /// Close the console, and return all output it received.
    pub fn into_transcript(self) -> Vec<u8> {
        self.output.into_transcript()
    }
}

impl Uart for SerialUart {
    fn send(&mut self, bytes: &[u8]) -> Result<()> {
        self.port.write_all(bytes)?;
        Ok(())
    }

    fn receive_until(&mut self, needle: &str, timeout: Duration) -> Result<String> {
        let deadline = Instant::now() + timeout;
        let mut chunk = [0u8; 256];

        while Instant::now() < deadline {
            match self.port.read(&mut chunk) {
                Ok(n) => self.output.push(&chunk[..n]),
                Err(e) if e.kind() == std::io::ErrorKind::TimedOut => {}
                Err(e) => return Err(e.into()),
            }

            if let Some(out) = self.output.take_through(needle) {
                return Ok(out);
            }
        }

        Err(self.output.timeout_error(needle))
    }

    fn transcript(&mut self) -> &[u8] {
        self.output.transcript()
    }
}
