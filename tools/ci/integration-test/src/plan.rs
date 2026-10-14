// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use crate::boards::Board;
use crate::test_cases::{TESTS, TestCase};
use anyhow::{Context, Result, anyhow, bail, ensure};
use fs_err as fs;
use log::info;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};
use std::time::Duration;

#[derive(clap::Args)]
pub struct Args {
    #[command(flatten)]
    selection: Selection,
    #[arg(long)]
    out: PathBuf,
}

impl Args {
    /// Compute a plan for the selected tests and write it to `out`.
    pub fn run(self) -> Result<()> {
        let plan = Plan::compute(&self.selection)?;
        info!("{}", plan.summary().trim_end());
        let json = serde_json::to_string_pretty(&plan).unwrap();
        fs::write(&self.out, json)?;
        info!("wrote {}", self.out.display());
        Ok(())
    }
}

/// A plan is a set of tests and all of the artifacts required by them.
#[derive(Serialize, Deserialize)]
pub struct Plan {
    /// Artifacts to build, by label.
    pub artifacts: BTreeMap<String, BuildSpec>,
    pub tests: Vec<PlannedTest>,
    /// Selected tests which are unsupported on their board.
    pub unsupported: Vec<UnsupportedTest>,
}

/// A test, planned on a specific board.
#[derive(Clone, Serialize, Deserialize)]
pub struct PlannedTest {
    #[serde(with = "crate::test_cases::by_id")]
    pub test: &'static TestCase,
    #[serde(with = "crate::boards::by_name")]
    pub board: &'static Board,
    /// Label of the kernel artifact.
    pub kernel: String,
    /// Labels of the app artifacts.
    pub apps: Vec<String>,
}

impl PlannedTest {
    /// Return the id of the planned test, in the form `<test>@<board>`.
    pub fn id(&self) -> String {
        format!("{}@{}", self.test.id, self.board)
    }
}

/// A test which is unsupported on a board (see [`TestCase::unsupported`]).
#[derive(Clone, Serialize, Deserialize)]
pub struct UnsupportedTest {
    #[serde(with = "crate::test_cases::by_id")]
    pub test: &'static TestCase,
    #[serde(with = "crate::boards::by_name")]
    pub board: &'static Board,
}

impl UnsupportedTest {
    /// Return the id of the test on the board, in the form `<test>@<board>`.
    pub fn id(&self) -> String {
        format!("{}@{}", self.test.id, self.board)
    }

    /// Return the reason the test is unsupported on the board.
    ///
    /// This panics if the test is supported on the board in this build of the
    /// harness, which happens when the plan was created by a different build.
    pub fn reason(&self) -> &'static str {
        (self.test.unsupported_reason(self.board))
            .unwrap_or_else(|| panic!("{} is supported in this build", self.id()))
    }
}

/// Specification of how to build an artifact.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "kebab-case")]
pub enum BuildSpec {
    TockKernel {
        #[serde(with = "crate::boards::by_name")]
        board: &'static Board,
    },
    /// libtock-c example app, built as a TAB.
    LibtockCApp {
        /// Path of the app under libtock-c's `examples/`.
        name: String,
        /// libtock-c `TOCK_TARGETS` to build the app for.
        tock_targets: Vec<String>,
    },
}

impl BuildSpec {
    /// Return the file name of the artifact with `label` built from this spec.
    pub fn file_name(&self, label: &str) -> String {
        let extension = match self {
            BuildSpec::TockKernel { .. } => "bin",
            BuildSpec::LibtockCApp { .. } => "tab",
        };
        format!("{label}.{extension}")
    }

    /// Compute the label of the artifact built from this spec.
    ///
    /// The label consists of a readable name and the first 8 hex digits of the
    /// SHA-256 hash of the spec's JSON. Thus, identical specs map to the same
    /// label, and different specs sharing a readable name receive different
    /// labels.
    fn label(&self) -> String {
        let name = match self {
            BuildSpec::TockKernel { board } => format!("tock-kernel-{board}"),
            BuildSpec::LibtockCApp { name, .. } => format!("libtock-c-app-{name}"),
        };

        // Make sure we're producing valid file names:
        let name: String = name
            .chars()
            .map(|c| {
                if c.is_ascii_alphanumeric() || "-_.".contains(c) {
                    c
                } else {
                    '_'
                }
            })
            .collect();

        let json = serde_json::to_vec(self).unwrap();
        let hash = format!("{:x}", Sha256::digest(&json));

        format!("{name}-{}", &hash[..8])
    }
}

impl Plan {
    /// Read the plan at `path`.
    pub fn load(path: &Path) -> Result<Plan> {
        let text = fs::read_to_string(path)?;
        serde_json::from_str(&text).with_context(|| format!("failed to parse {}", path.display()))
    }

    /// Return the path of the artifact with `label` in the artifact directory
    /// `dir`.
    pub fn artifact_path(&self, dir: &Path, label: &str) -> PathBuf {
        dir.join(self.artifacts[label].file_name(label))
    }

    /// Compute a plan for the tests selected by `selection`.
    pub fn compute(selection: &Selection) -> Result<Plan> {
        let mut plan = Plan {
            artifacts: BTreeMap::new(),
            tests: Vec::new(),
            unsupported: Vec::new(),
        };
        for test in TESTS {
            for board in test.boards() {
                if test.unsupported_reason(board).is_some() {
                    plan.unsupported.push(UnsupportedTest { test, board });
                    continue;
                }
                let kernel = plan.add_artifact(BuildSpec::TockKernel { board });
                let tock_targets: Vec<String> =
                    board.tock_targets.iter().map(|t| t.to_string()).collect();
                let apps = (test.apps.iter())
                    .map(|name| {
                        plan.add_artifact(BuildSpec::LibtockCApp {
                            name: name.to_string(),
                            tock_targets: tock_targets.clone(),
                        })
                    })
                    .collect();
                plan.tests.push(PlannedTest {
                    test,
                    board,
                    kernel,
                    apps,
                });
            }
        }

        // The order of `TESTS` and `BOARDS` depends on the linker. Sort the
        // planned tests, such that a plan does not depend on link order:
        plan.tests.sort_by_key(|t| (t.test.id, t.board.name));
        plan.unsupported.sort_by_key(|u| (u.test.id, u.board.name));

        plan.select(selection)
    }

    /// Add an artifact from `spec`, returning its label.
    ///
    /// This implicitly deduplicates identical artifacts shared among tests in a
    /// plan.
    fn add_artifact(&mut self, spec: BuildSpec) -> String {
        let label = spec.label();
        let existing = (self.artifacts)
            .entry(label.clone())
            .or_insert_with(|| spec.clone());
        assert_eq!(*existing, spec, "artifact label {label:?} collides");
        label
    }

    /// Filter tests and artifacts by `selection`.
    ///
    /// An `anyboard` selector picks one board for each test which it matches
    /// and no other selector includes. It prefers boards which other selected
    /// tests already use, such that the plan needs fewer kernels (and hosts).
    /// Otherwise, it picks the test's first board.
    pub fn select(&self, selection: &Selection) -> Result<Plan> {
        for selector in &selection.tests {
            if self.tests.iter().any(|t| selector.matches(t.test, t.board)) {
                continue;
            }
            let unsupported: Vec<String> = (self.unsupported.iter())
                .filter(|u| selector.matches(u.test, u.board))
                .map(|u| format!("{} ({})", u.id(), u.reason()))
                .collect();
            ensure!(
                unsupported.is_empty(),
                "--test {selector} matches only unsupported tests: {}",
                unsupported.join(", ")
            );
            bail!("--test {selector} matches no planned test");
        }
        let (any, fixed): (Vec<&Selector>, Vec<&Selector>) =
            (selection.tests.iter()).partition(|s| s.board == BoardSelector::Any);
        let mut keep: Vec<bool> = (self.tests.iter())
            .map(|t| selection.tests.is_empty() || fixed.iter().any(|s| s.matches(t.test, t.board)))
            .collect();
        let mut boards: BTreeSet<&str> = (self.tests.iter().zip(&keep))
            .filter(|(_, keep)| **keep)
            .map(|(t, _)| t.board.name)
            .collect();
        for selector in any {
            // Indices of the planned tests which the selector may pick, by
            // test id:
            let mut candidates: BTreeMap<&str, Vec<usize>> = BTreeMap::new();
            for (i, t) in self.tests.iter().enumerate() {
                if selector.matches(t.test, t.board) {
                    candidates.entry(t.test.id).or_default().push(i);
                }
            }
            for indices in candidates.values() {
                if indices.iter().any(|&i| keep[i]) {
                    continue;
                }
                let pick = (indices.iter().copied())
                    .find(|&i| boards.contains(self.tests[i].board.name))
                    .unwrap_or(indices[0]);
                keep[pick] = true;
                boards.insert(self.tests[pick].board.name);
            }
        }
        Ok(self.part(|i, _| keep[i], |u| selection.matches(u.test, u.board)))
    }

    /// Return the part of this plan with the tests and unsupported tests for
    /// which `test` and `unsupported` return `true`, along with the artifacts
    /// these tests use.
    ///
    /// `test` receives the index of each test in [`Plan::tests`].
    pub fn part(
        &self,
        test: impl Fn(usize, &PlannedTest) -> bool,
        unsupported: impl Fn(&UnsupportedTest) -> bool,
    ) -> Plan {
        let tests: Vec<PlannedTest> = (self.tests.iter().enumerate())
            .filter(|(i, t)| test(*i, t))
            .map(|(_, t)| t.clone())
            .collect();
        let used: BTreeSet<&String> = (tests.iter())
            .flat_map(|t| std::iter::once(&t.kernel).chain(&t.apps))
            .collect();
        Plan {
            artifacts: (self.artifacts.iter())
                .filter(|(label, _)| used.contains(label))
                .map(|(label, spec)| (label.clone(), spec.clone()))
                .collect(),
            tests,
            unsupported: (self.unsupported.iter())
                .filter(|u| unsupported(u))
                .cloned()
                .collect(),
        }
    }

    /// Return a human-readable summary of the plan.
    ///
    /// It lists the number of artifacts to build, and the tests on each board
    /// along with their combined worst-case duration.
    pub fn summary(&self) -> String {
        let kernels = self
            .artifacts
            .values()
            .filter(|spec| matches!(spec, BuildSpec::TockKernel { .. }))
            .count();
        let count = |n: usize, what: &str| format!("{n} {what}{}", if n == 1 { "" } else { "s" });
        let mut out = format!(
            "{}, using {} and {}\n",
            count(self.tests.len(), "test"),
            count(kernels, "kernel"),
            count(self.artifacts.len() - kernels, "app")
        );
        let mut by_board: BTreeMap<&str, (Vec<&str>, Duration)> = BTreeMap::new();
        for t in &self.tests {
            let (tests, worst_case) = by_board.entry(t.board.name).or_default();
            tests.push(t.test.id);
            *worst_case += t.test.worst_case(t.board);
        }
        for (board, (tests, worst_case)) in by_board {
            out += &format!(
                "  {board} ({}, up to {}s): {}\n",
                tests.len(),
                worst_case.as_millis().div_ceil(1000),
                tests.join(", ")
            );
        }
        for u in &self.unsupported {
            out += &format!("  unsupported: {} ({})\n", u.id(), u.reason());
        }
        out
    }
}

/// Selection of planned tests.
///
/// A test is selected if any selector in `tests` matches it. An empty `tests`
/// selects all tests.
#[derive(Default, clap::Args)]
pub struct Selection {
    /// Select tests by `<test>@<board>` (`alltests` and `allboards` match any,
    /// and `anyboard` picks one board). May be repeated.
    #[arg(long = "test", value_name = "TEST@BOARD")]
    pub tests: Vec<Selector>,
}

impl Selection {
    /// Check if this selection includes `test` on `board`.
    fn matches(&self, test: &TestCase, board: &Board) -> bool {
        self.tests.is_empty() || self.tests.iter().any(|s| s.matches(test, board))
    }
}

/// A single `--test` selector, which matches one or all tests on one, all or
/// any boards.
#[derive(Clone)]
pub struct Selector {
    /// Selected test, or `None` to select all tests.
    pub test: Option<&'static TestCase>,
    pub board: BoardSelector,
}

/// Boards matched by a [`Selector`].
#[derive(Clone, Copy, PartialEq)]
pub enum BoardSelector {
    One(&'static Board),
    /// All boards (`allboards`).
    All,
    /// Any one board (`anyboard`), which [`Plan::select`] picks.
    Any,
}

impl Selector {
    /// Check if this selector matches `test` on `board`. An `anyboard`
    /// selector matches every board which it may pick.
    fn matches(&self, test: &TestCase, board: &Board) -> bool {
        self.test.is_none_or(|t| t.id == test.id)
            && match self.board {
                BoardSelector::One(b) => b == board,
                BoardSelector::All | BoardSelector::Any => true,
            }
    }
}

impl std::str::FromStr for Selector {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> Result<Self> {
        let (test, board) = s.split_once('@').ok_or_else(|| {
            anyhow!(
                "expected `<test>@<board>` (<test> can be `alltests`, \
                 <board> can be `allboards` or `anyboard` (e.g. `{s}@allboards`)"
            )
        })?;
        let test = match test {
            "alltests" => None,
            _ => Some(TestCase::from_id(test)?),
        };
        let board = match board {
            "allboards" => BoardSelector::All,
            "anyboard" => BoardSelector::Any,
            _ => BoardSelector::One(Board::from_name(board)?),
        };
        Ok(Selector { test, board })
    }
}

impl std::fmt::Display for Selector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let test = self.test.map_or("alltests", |t| t.id);
        let board = match self.board {
            BoardSelector::One(board) => board.name,
            BoardSelector::All => "allboards",
            BoardSelector::Any => "anyboard",
        };
        write!(f, "{test}@{board}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn select(selectors: &[&str]) -> Vec<String> {
        let selection = Selection {
            tests: selectors.iter().map(|s| s.parse().unwrap()).collect(),
        };
        Plan::compute(&selection)
            .unwrap()
            .tests
            .iter()
            .map(PlannedTest::id)
            .collect()
    }

    #[test]
    fn selectors_are_cumulative() {
        assert_eq!(
            select(&["hello_world@hifive1", "hello_world@qemu_rv64_virt"]),
            ["hello_world@hifive1", "hello_world@qemu_rv64_virt"]
        );
        assert_eq!(
            select(&["hello_world@allboards"]).len(),
            crate::boards::BOARDS.len()
        );
        assert_eq!(select(&[]).len(), select(&["alltests@allboards"]).len());
    }

    #[test]
    fn selectors_must_match_a_test() {
        let empty = Plan {
            artifacts: BTreeMap::new(),
            tests: Vec::new(),
            unsupported: Vec::new(),
        };
        let selection = Selection {
            tests: vec!["hello_world@allboards".parse().unwrap()],
        };
        let error = empty.select(&selection).err().unwrap();
        assert!(
            error.to_string().contains("matches no planned test"),
            "{error}"
        );
    }

    #[test]
    fn unsupported_tests_are_left_out_with_their_reason() {
        let plan = Plan::compute(&Selection::default()).unwrap();
        let id = "lua_hello@qemu_rv64_virt";
        assert!(plan.tests.iter().all(|t| t.id() != id));
        assert!(plan.unsupported.iter().any(|u| u.id() == id));
        // Selecting other tests also leaves it out of the unsupported tests:
        let plan = Plan::compute(&Selection {
            tests: vec!["hello_world@allboards".parse().unwrap()],
        })
        .unwrap();
        assert!(plan.unsupported.is_empty());
    }

    #[test]
    fn selecting_only_unsupported_tests_fails() {
        let selection = Selection {
            tests: vec!["lua_hello@qemu_rv64_virt".parse().unwrap()],
        };
        let error = Plan::compute(&selection).err().unwrap();
        assert!(
            error.to_string().contains("matches only unsupported tests"),
            "{error}"
        );
    }

    #[test]
    fn anyboard_picks_one_board_preferring_those_already_used() {
        assert_eq!(select(&["hello_world@anyboard"]).len(), 1);
        assert_eq!(
            select(&["blink@nrf52840dk", "hello_world@anyboard"]),
            ["blink@nrf52840dk", "hello_world@nrf52840dk"]
        );
        // A test which another selector already includes is not added again:
        assert_eq!(
            select(&["hello_world@nucleo_f429zi", "hello_world@anyboard"]),
            ["hello_world@nucleo_f429zi"]
        );
    }

    #[test]
    fn selectors_need_a_known_test_and_board() {
        assert!("hello_world".parse::<Selector>().is_err());
        assert!("no_such_test@allboards".parse::<Selector>().is_err());
        assert!("hello_world@no_such_board".parse::<Selector>().is_err());
        assert_eq!(
            "alltests@allboards"
                .parse::<Selector>()
                .unwrap()
                .to_string(),
            "alltests@allboards"
        );
    }
}
