// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use crate::board::{Board, BoardKind};
use crate::boards::nrf52840dk::Nrf52840Dk;
use crate::boards::nucleo_f429zi::NucleoF429zi;
use crate::boards::qemu_virt::QemuVirt;
use crate::build::Manifest;
use crate::hostspec::{DutSpec, HostSpec};
use crate::plan::Plan;
use crate::testcase::TestCtx;
use crate::tests::TESTS;
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

pub struct RunOptions<'a> {
    pub host_spec: Option<HostSpec>,
    pub filter: Option<&'a [String]>,
}

pub fn run(plan: &Plan, manifest: &Manifest, opts: RunOptions) -> Vec<TestReport> {
    let mut next_dut = BTreeMap::new();
    let mut reports = Vec::new();

    for planned in &plan.tests {
        if let Some(filter) = opts.filter {
            if !filter.iter().any(|f| f == &planned.id) {
                continue;
            }
        }

        println!("=== {} ===", planned.id);

        let tc = TESTS
            .iter()
            .find(|t| planned.id.split('@').next() == Some(t.id))
            .expect("plan references an unknown test id");

        let mut board = match make_board(planned.board, &opts.host_spec, &mut next_dut) {
            Some(b) => b,
            None => {
                println!("  skipped: no matching DUT on this host");
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

        println!("  flashing...");
        let outcome = match board.flash(kernel_path, &app_paths) {
            Ok(()) => {
                println!("  running test body...");
                let mut ctx = TestCtx::new(&mut *board, tc.requires);
                match tc.body.run(&mut ctx) {
                    Ok(()) => TestOutcome::Passed,
                    Err(reason) => TestOutcome::Failed { reason },
                }
            }
            Err(reason) => TestOutcome::Failed { reason },
        };
        let _ = board.reset();

        match &outcome {
            TestOutcome::Passed => println!("  PASSED"),
            TestOutcome::Failed { reason } => println!("  FAILED: {reason}"),
            TestOutcome::NotApplicable { reason } => println!("  SKIPPED: {reason}"),
        }

        reports.push(TestReport {
            id: planned.id.clone(),
            outcome,
        });
    }

    reports
}

fn make_board(
    kind: BoardKind,
    host_spec: &Option<HostSpec>,
    next_dut: &mut BTreeMap<BoardKind, usize>,
) -> Option<Box<dyn Board>> {
    let mut pick_dut = |board: &str| -> Option<DutSpec> {
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
        BoardKind::Nrf52840Dk => Some(Box::new(Nrf52840Dk::new(pick_dut("nRF52840-DK")?))),
        BoardKind::NucleoF429zi => Some(Box::new(NucleoF429zi::new(pick_dut("NUCLEO-F429ZI")?))),
    }
}
