// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use crate::board::{Board, BoardKind, Gpio, Uart, take_through, timeout_error};
use nix::fcntl::{fcntl, FcntlArg, OFlag};
use std::io::{Read, Write};
use std::os::unix::io::AsRawFd;
use std::path::Path;
use std::process::{Child, Command, Stdio};
use std::time::{Duration, Instant};

pub struct QemuVirt {
    flash_file: tempfile::NamedTempFile,
    child: Option<Child>,
    buf: Vec<u8>,
}

impl QemuVirt {
    pub fn new() -> Self {
        QemuVirt {
            flash_file: tempfile::NamedTempFile::new().expect("failed to create flash file"),
            child: None,
            buf: Vec::new(),
        }
    }
}

impl Board for QemuVirt {
    fn kind(&self) -> BoardKind {
        BoardKind::QemuVirt
    }

    fn flash(&mut self, kernel: &Path, apps: &[&Path]) -> Result<(), String> {
        run(Command::new("tockloader")
            .arg("flash")
            .arg("--flash-file")
            .arg(self.flash_file.path())
            .args(["--board", "qemu_rv64_virt", "--address", "0x80000000"])
            .arg(kernel))?;

        for app in apps {
            run(Command::new("tockloader")
                .arg("install")
                .arg("--flash-file")
                .arg(self.flash_file.path())
                .args(["--board", "qemu_rv64_virt"])
                .arg(app))?;
        }

        let mut child = Command::new("qemu-system-riscv64")
            .args(["-machine", "virt", "-semihosting"])
            .args(["-global", "driver=riscv-cpu,property=smepmp,value=true"])
            .args(["-global", "virtio-mmio.force-legacy=false"])
            .args(["-device", "virtio-rng-device"])
            .args(["-device", "virtio-keyboard-device"])
            .args(["-display", "none", "-serial", "stdio"])
            .arg("-bios")
            .arg(self.flash_file.path())
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .map_err(|e| e.to_string())?;

        let fd = child.stdout.as_ref().unwrap().as_raw_fd();
        let flags =
            OFlag::from_bits_truncate(fcntl(fd, FcntlArg::F_GETFL).map_err(|e| e.to_string())?);
        fcntl(fd, FcntlArg::F_SETFL(flags | OFlag::O_NONBLOCK)).map_err(|e| e.to_string())?;

        self.child = Some(child);
        self.buf.clear();
        Ok(())
    }

    fn reset(&mut self) -> Result<(), String> {
        if let Some(mut child) = self.child.take() {
            let _ = child.kill();
            let _ = child.wait();
        }
        Ok(())
    }

    fn uart(&mut self) -> Option<&mut dyn Uart> {
        Some(self)
    }

    fn gpio(&mut self, _name: &str) -> Option<&mut dyn Gpio> {
        None
    }
}

impl Uart for QemuVirt {
    fn write(&mut self, bytes: &[u8]) -> Result<(), String> {
        self.child
            .as_mut()
            .and_then(|c| c.stdin.as_mut())
            .ok_or("board is not flashed/running")?
            .write_all(bytes)
            .map_err(|e| e.to_string())
    }

    fn wait_for(&mut self, needle: &str, timeout: Duration) -> Result<String, String> {
        let deadline = Instant::now() + timeout;
        let mut chunk = [0u8; 256];
        while Instant::now() < deadline {
            let stdout = self
                .child
                .as_mut()
                .and_then(|c| c.stdout.as_mut())
                .ok_or("board is not flashed/running")?;
            match stdout.read(&mut chunk) {
                Ok(0) => {}
                Ok(n) => self.buf.extend_from_slice(&chunk[..n]),
                Err(e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                    std::thread::sleep(Duration::from_millis(50));
                }
                Err(e) => return Err(e.to_string()),
            }
            if let Some(out) = take_through(&mut self.buf, needle) {
                return Ok(out);
            }
        }
        Err(timeout_error(&self.buf, needle))
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
