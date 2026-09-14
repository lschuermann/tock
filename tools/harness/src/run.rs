// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use crate::board::{Board, BoardKind};
use crate::boards::esp32_c3::Esp32C3;
use crate::boards::nrf52840dk::Nrf52840Dk;
use crate::boards::nucleo_f429zi::NucleoF429zi;
use crate::boards::qemu_virt::QemuVirt;
use crate::boards::rp2xxx::{self, Rp2xxx};
use crate::build::Manifest;
use crate::hostspec::{DutSpec, HostSpec};
use crate::plan::Plan;
use crate::testcase::TestCtx;
use crate::tests;
use log::{info, warn};
use serde::Serialize;
use std::collections::BTreeMap;
use std::path::Path;

#[derive(Serialize)]
#[serde(tag = "status")]
pub enum TestOutcome {
    Passed,
    Failed { reason: String },
    NotApplicable { reason: String },
}

#[derive(Serialize)]
pub struct TestReport {
    pub id: String,
    pub outcome: TestOutcome,
}

/// Width of the summary table, the classic terminal width (which serial
/// consoles default to too); longer cells wrap.
const SUMMARY_WIDTH: u16 = 80;
/// Longer reasons are cut short with an ellipsis.
const SUMMARY_REASON_CHARS: usize = 200;

/// A table of `reports`: each test, its status, and the first line of the
/// reason it failed or was skipped.
pub fn summary_table(reports: &[TestReport]) -> comfy_table::Table {
    let mut table = comfy_table::Table::new();
    table.load_style(comfy_table::presets::ASCII_BORDERS_ONLY_CONDENSED);
    table.set_content_arrangement(comfy_table::ContentArrangement::Dynamic);
    table.set_width(SUMMARY_WIDTH);
    table.set_header(["test", "status", "reason"]);
    for report in reports {
        let (status, reason) = match &report.outcome {
            TestOutcome::Passed => ("passed", ""),
            TestOutcome::Failed { reason } => ("FAILED", reason.as_str()),
            TestOutcome::NotApplicable { reason } => ("skipped", reason.as_str()),
        };
        let mut reason = reason.lines().next().unwrap_or("").to_string();
        if let Some((cut, _)) = reason.char_indices().nth(SUMMARY_REASON_CHARS) {
            reason.truncate(cut);
            reason.push('…');
        }
        table.add_row([report.id.as_str(), status, &reason]);
    }
    // Leave reasons room: wrap only test ids longer than half the table.
    if let Some(column) = table.column_mut(0) {
        let half = comfy_table::Width::Percentage(50);
        column.set_constraint(comfy_table::ColumnConstraint::UpperBoundary(half));
    }
    table
}

pub struct RunOptions<'a> {
    pub host_spec: Option<HostSpec>,
    /// Directory to write each test's console transcript to, as
    /// `<test id>.log`.
    pub log_dir: &'a Path,
}

pub fn run(plan: &Plan, manifest: &Manifest, opts: RunOptions) -> Vec<TestReport> {
    let mut next_dut = BTreeMap::new();
    let mut reports = Vec::new();

    for planned in &plan.tests {
        info!("=== {} ===", planned.id);

        let tc = tests::lookup(&planned.id);

        let mut board = match make_board(planned.board, &opts, &mut next_dut) {
            Some(b) => b,
            None => {
                info!("  skipped: no matching DUT on this host");
                reports.push(TestReport {
                    id: planned.id.clone(),
                    outcome: TestOutcome::NotApplicable {
                        reason: "no matching DUT on this host".into(),
                    },
                });
                continue;
            }
        };

        let kernel_path = &manifest[&planned.kernel];
        let app_paths: Vec<&Path> = planned.apps.iter().map(|l| manifest[l].as_path()).collect();

        info!("  flashing...");
        let outcome = match board.flash(kernel_path, &app_paths) {
            Ok(()) => {
                info!("  running test body...");
                let mut ctx = TestCtx::new(&mut *board, tc.requires);
                let started = std::time::Instant::now();
                let result = tc.body.run(&mut ctx);
                let (took, limit) = (started.elapsed(), tc.body.time_limit());
                match result {
                    // Jobs are leased by the limits, so a test that exceeds
                    // its own fails, for its limit to be raised.
                    Ok(()) if took > limit => TestOutcome::Failed {
                        reason: format!(
                            "took {:.1}s, over its time limit of {:.1}s",
                            took.as_secs_f64(),
                            limit.as_secs_f64()
                        ),
                    },
                    Ok(()) => TestOutcome::Passed,
                    Err(reason) => TestOutcome::Failed { reason },
                }
            }
            Err(reason) => TestOutcome::Failed { reason },
        };
        if let Some(uart) = board.uart() {
            write_log(opts.log_dir, &planned.id, uart.transcript());
        }
        let _ = board.reset();

        match &outcome {
            TestOutcome::Passed => info!("  PASSED"),
            TestOutcome::Failed { reason } => info!("  FAILED: {reason}"),
            TestOutcome::NotApplicable { reason } => info!("  SKIPPED: {reason}"),
        }

        reports.push(TestReport {
            id: planned.id.clone(),
            outcome,
        });
    }

    for unsupported in &plan.unsupported {
        let reason = format!("unsupported on this board: {}", unsupported.reason);
        info!("=== {} ===", unsupported.id);
        info!("  SKIPPED: {reason}");
        reports.push(TestReport {
            id: unsupported.id.clone(),
            outcome: TestOutcome::NotApplicable { reason },
        });
    }

    reports
}

/// Writes a test's console transcript. A failure to write it is only a warning: the
/// test's outcome does not depend on it.
fn write_log(dir: &Path, id: &str, transcript: &[u8]) {
    let path = dir.join(format!("{id}.log"));
    match std::fs::create_dir_all(dir).and_then(|()| std::fs::write(&path, transcript)) {
        Ok(()) => info!("  console log: {}", path.display()),
        Err(e) => warn!("failed to write {}: {e}", path.display()),
    }
}

fn make_board(
    kind: BoardKind,
    opts: &RunOptions,
    next_dut: &mut BTreeMap<BoardKind, usize>,
) -> Option<Box<dyn Board>> {
    let host_spec = &opts.host_spec;
    let mut pick_dut = || -> Option<DutSpec> {
        let board = kind.host_spec_board()?;
        let duts: Vec<_> = host_spec
            .as_ref()?
            .duts
            .iter()
            .filter(|d| d.board == board)
            .collect();
        if duts.is_empty() {
            return None;
        }
        let next = next_dut.entry(kind).or_default();
        let dut = duts[*next % duts.len()].clone();
        *next += 1;
        Some(dut)
    };

    match kind {
        BoardKind::QemuVirt => Some(Box::new(QemuVirt::new())),
        BoardKind::Nrf52840Dk => Some(Box::new(Nrf52840Dk::new(pick_dut()?))),
        BoardKind::NucleoF429zi => Some(Box::new(NucleoF429zi::new(pick_dut()?))),
        // Each board kind's console is where its kernel, as its board
        // directory builds it, puts it: USB CDC-ACM on the Pico, UART0 on the
        // Pico 2. A kernel configured otherwise needs a board kind of its own.
        BoardKind::RaspberryPiPico => Some(Box::new(Rp2xxx::new(
            rp2xxx::Chip::Rp2040,
            rp2xxx::Console::UsbCdc,
            pick_dut()?,
        ))),
        BoardKind::RaspberryPiPico2 => Some(Box::new(Rp2xxx::new(
            rp2xxx::Chip::Rp2350,
            rp2xxx::Console::HostSpec,
            pick_dut()?,
        ))),
        BoardKind::Esp32C3DevkitM1 => Some(Box::new(Esp32C3::new(pick_dut()?))),
    }
}
