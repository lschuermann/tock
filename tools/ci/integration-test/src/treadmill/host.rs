// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

//! Matching of the hosts which can run the tests of a plan.

use crate::boards::Board;
use crate::host_spec::HostSpec;
use crate::plan::{Plan, PlannedTest};
use crate::test_cases::Requirement;
use log::warn;
use std::collections::BTreeSet;

/// Check if the host with the ID `host` can run every test of `plan`.
///
/// For each test, the host must have a DUT of the test's board which
/// satisfies all requirements of the test, as the host predicate of a new job
/// demands.
pub fn host_fits(plan: &Plan, host: &str) -> bool {
    let Some(spec) = host_spec(host) else {
        return false;
    };
    plan.tests.iter().all(|planned| can_run(&spec, planned))
}

/// Check if `host` has a DUT which can run `planned`.
fn can_run(host: &HostSpec, planned: &PlannedTest) -> bool {
    (host.duts.iter())
        .filter(|dut| dut.board == planned.board.name)
        .filter_map(|dut| dut.attached())
        .any(|dut| dut.can_run(planned.test))
}

/// Return the spec of the host with the ID or name `host`, or `None` with a
/// warning if it cannot be read.
fn host_spec(host: &str) -> Option<HostSpec> {
    #[derive(serde::Deserialize)]
    struct HostInfo {
        spec: Option<serde_json::Value>,
    }
    let spec = match super::tml::tml_json::<HostInfo>(&["host", "show", host]) {
        Ok(HostInfo { spec: Some(spec) }) => spec,
        Ok(HostInfo { spec: None }) => {
            warn!("host {host} has no spec");
            return None;
        }
        Err(e) => {
            warn!("failed to read the spec of host {host}: {e:#}");
            return None;
        }
    };
    HostSpec::from_value(spec)
        .map_err(|e| warn!("host {host}: {e:#}"))
        .ok()
}

/// Return the specs of the hosts which can take jobs, which are those that are
/// live and not in maintenance.
pub fn fleet() -> anyhow::Result<Vec<HostSpec>> {
    #[derive(serde::Deserialize)]
    struct Listed {
        host_id: String,
        live: bool,
        maintenance: bool,
    }
    let hosts: Vec<Listed> = super::tml::tml_json(&["host", "list"])?;
    Ok((hosts.iter())
        .filter(|h| h.live && !h.maintenance)
        .filter_map(|h| host_spec(&h.host_id))
        .collect())
}

/// Return the reason no host of `fleet` can run `planned`, or `None` if a host
/// has a DUT which can run it.
pub fn unrunnable(planned: &PlannedTest, fleet: &[HostSpec]) -> Option<String> {
    let board = planned.board;
    if board.is_virtual() {
        return Some(format!("{board} is virtual, so `run` runs it locally"));
    }
    if fleet.iter().any(|host| can_run(host, planned)) {
        return None;
    }
    Some(format!(
        "no Treadmill host has a {board} DUT with all of: {}",
        planned.test.describe_requirements(board)
    ))
}

/// Return the requirements which the host predicate demands of a host, with
/// one line per distinct set of requirements. For each line, a DUT must
/// provide all of its requirements.
pub fn host_requirements(plan: &Plan) -> BTreeSet<String> {
    (plan.tests.iter())
        .map(|planned| planned.test.describe_requirements(planned.board))
        .collect()
}

/// Return a CEL predicate which admits hosts that can run every test of
/// `plan`.
///
/// For each test, a DUT of the test's board must satisfy all requirements of
/// the test.
pub fn host_predicate(plan: &Plan) -> String {
    let clauses: BTreeSet<String> = (plan.tests.iter())
        .map(|planned| {
            let board = planned.board;
            let conditions: Vec<String> = std::iter::once(format!("d.board == {:?}", board.name))
                .chain(
                    planned
                        .test
                        .requires
                        .iter()
                        .map(|r| dut_condition(*r, board)),
                )
                .collect();
            format!("host.duts.exists(d, {})", conditions.join(" && "))
        })
        .collect();
    clauses.into_iter().collect::<Vec<_>>().join(" && ")
}

/// Return a CEL condition on a DUT `d` of `board`, on the host `host`, which
/// holds if the DUT satisfies `requirement`.
///
/// This condition must match [`crate::boards::AttachedDut::satisfies`].
fn dut_condition(requirement: Requirement, board: &Board) -> String {
    match requirement {
        Requirement::Uart => "has(d.console)".to_string(),
        Requirement::Gpio { name, mode } => match board.pin(name) {
            // Only GPIOs which `DutGpios` can drive:
            Some(pin) => format!(
                "{pin:?} in d.gpio && {:?} in d.gpio[{pin:?}].modes && \
                 host.gpio_controllers[d.gpio[{pin:?}].controller].driver == \"linux-gpiochip\"",
                mode.as_str()
            ),
            None => "false".to_string(),
        },
        Requirement::Feature(_) => "true".to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::boards::{nrf52, nucleo_f429zi, qemu};
    use crate::test_cases::TestCase;

    fn planned(id: &str, board: &'static Board) -> PlannedTest {
        PlannedTest {
            test: TestCase::from_id(id).unwrap(),
            board,
            kernel: String::new(),
            apps: Vec::new(),
        }
    }

    #[test]
    fn tests_no_host_can_run_are_unrunnable_and_say_why() {
        // One host, with an nRF52840DK which has a console but no GPIOs:
        let fleet = [HostSpec::from_value(serde_json::json!({
            "spec_version": "v2",
            "duts": [{"board": "nrf52840dk",
                      "console": {"kind": "uart", "device": "/dev/ttyACM0", "baud": 115200}}]
        }))
        .unwrap()];
        let nrf = &nrf52::NRF52840DK;
        assert_eq!(unrunnable(&planned("hello_world", nrf), &fleet), None);
        assert_eq!(
            unrunnable(&planned("blink", nrf), &fleet).unwrap(),
            "no Treadmill host has a nrf52840dk DUT with all of: \
             led0 at P0.13 (digital_in), led1 at P0.14 (digital_in)"
        );
        let nucleo = planned("hello_world", &nucleo_f429zi::BOARD);
        assert!(
            unrunnable(&nucleo, &fleet)
                .unwrap()
                .contains("nucleo_f429zi DUT")
        );
        let qemu = planned("hello_world", &qemu::RV64_VIRT);
        assert!(unrunnable(&qemu, &fleet).unwrap().contains("virtual"));
    }
}
