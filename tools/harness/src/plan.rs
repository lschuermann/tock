// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use crate::board::BoardKind;
use crate::tests::TESTS;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::BTreeMap;

pub type Label = String;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum BuildSpec {
    Kernel { board: BoardKind },
    App { name: String },
}

pub fn label_of(spec: &BuildSpec) -> Label {
    let json = serde_json::to_vec(spec).expect("BuildSpec always serializes");
    format!("{:x}", Sha256::digest(&json))
}

#[derive(Debug, Serialize, Deserialize)]
pub struct PlannedTest {
    pub id: String,
    pub board: BoardKind,
    pub kernel: Label,
    pub apps: Vec<Label>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Plan {
    pub artifacts: BTreeMap<Label, BuildSpec>,
    pub tests: Vec<PlannedTest>,
}

pub fn compute() -> Plan {
    let mut artifacts = BTreeMap::new();
    let mut tests = Vec::new();

    for tc in TESTS {
        for &board in tc.boards {
            let kernel_spec = BuildSpec::Kernel { board };
            let kernel = label_of(&kernel_spec);
            artifacts.insert(kernel.clone(), kernel_spec);

            let apps = tc
                .apps
                .iter()
                .map(|name| {
                    let spec = BuildSpec::App {
                        name: name.to_string(),
                    };
                    let label = label_of(&spec);
                    artifacts.insert(label.clone(), spec);
                    label
                })
                .collect();

            tests.push(PlannedTest {
                id: format!("{}@{:?}", tc.id, board),
                board,
                kernel,
                apps,
            });
        }
    }

    Plan { artifacts, tests }
}
