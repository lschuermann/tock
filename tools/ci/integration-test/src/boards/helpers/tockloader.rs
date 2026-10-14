// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use super::command;
use anyhow::{Result, ensure};
use std::io::{Read, Seek, SeekFrom};
use std::path::Path;
use std::process::Command;

/// Construct a tockloader invocation with the subcommand `subcommand`.
pub fn command(subcommand: &str) -> Command {
    let mut cmd = Command::new("tockloader");
    cmd.arg(subcommand);
    cmd
}

/// Write `kernel` at `address`, and `apps`, to the tockloader flash file
/// `flash_file` of the board `board` (as named by tockloader).
///
/// The apps are written at `app_address` if given, and at the default app
/// address of the board otherwise.
pub fn write_flash_file(
    flash_file: &Path,
    board: &str,
    address: u64,
    app_address: Option<u64>,
    kernel: &Path,
    apps: &[&Path],
) -> Result<()> {
    command::run(
        command("flash")
            .arg("--flash-file")
            .arg(flash_file)
            .args(["--board", board, "--address", &format!("{address:#x}")])
            .arg(kernel),
    )?;
    if apps.is_empty() {
        return Ok(());
    }
    let mut install = command("install");
    install
        .arg("--flash-file")
        .arg(flash_file)
        .args(["--board", board]);
    if let Some(app_address) = app_address {
        install.args(["--app-address", &format!("{app_address:#x}")]);
    }
    command::run(install.args(apps))
}

/// Read `len` bytes from `offset` in the tockloader flash file `flash_file`.
///
/// The region is padded with erased flash, such that the kernel does not find
/// stale apps behind the new ones. This fails if the region is empty, which
/// happens if tockloader placed the image at another offset than expected
/// (relative to the board's `flash_address`, or absolute). Programming an
/// empty region would erase the board.
pub fn read_flash_region(flash_file: &Path, offset: u64, len: usize) -> Result<Vec<u8>> {
    let mut region = Vec::with_capacity(len);
    let mut file = fs_err::File::open(flash_file)?;
    file.seek(SeekFrom::Start(offset))?;
    file.take(len as u64).read_to_end(&mut region)?;
    ensure!(
        region.iter().any(|&b| b != 0 && b != 0xff),
        "assembled flash file has no image at offset {offset:#x}, which happens if \
         tockloader places it relative to another flash address"
    );
    region.resize(len, 0xff);
    Ok(region)
}
