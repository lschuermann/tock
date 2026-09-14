// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use crate::board::BoardKind;
use crate::tests::TESTS;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::{BTreeMap, BTreeSet};

/// An artifact's name, unique within a plan and usable as a file name (see
/// [`label_of`]).
pub type Label = String;

/// How to build one artifact. Serialized with its kind as `type`, next to that
/// kind's configuration.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "type", rename_all = "kebab-case")]
pub enum BuildSpec {
    /// A Tock kernel, built from the board's directory.
    TockKernel { board: BoardKind },
    /// A libtock-c example app, as a TAB.
    LibtockCApp {
        /// Path under libtock-c's `examples/`.
        name: String,
        /// The libtock-c `TOCK_TARGETS` to build the app for.
        tock_targets: Vec<String>,
    },
}

/// A readable name for `spec`, followed by the start of the SHA-256 of its
/// JSON: the same spec always gets the same label, and different specs sharing
/// a readable name still get different ones.
pub fn label_of(spec: &BuildSpec) -> Label {
    let name = match spec {
        BuildSpec::TockKernel { board } => format!("tock-kernel-{board}"),
        BuildSpec::LibtockCApp { name, .. } => format!("libtock-c-app-{name}"),
    };
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
    let json = serde_json::to_vec(spec).expect("BuildSpec always serializes");
    let hash = format!("{:x}", Sha256::digest(&json));
    format!("{name}-{}", &hash[..8])
}

/// Add `spec` to `artifacts`, returning its label. An identical spec is shared.
fn add_artifact(artifacts: &mut BTreeMap<Label, BuildSpec>, spec: BuildSpec) -> Label {
    let label = label_of(&spec);
    let existing = artifacts
        .entry(label.clone())
        .or_insert_with(|| spec.clone());
    assert_eq!(*existing, spec, "artifact label {label:?} collides");
    label
}

#[derive(Debug, Serialize, Deserialize)]
pub struct PlannedTest {
    pub id: String,
    pub board: BoardKind,
    pub kernel: Label,
    pub apps: Vec<Label>,
}

impl PlannedTest {
    /// The id of the test this plans, without its `@<board>`.
    pub fn test_id(&self) -> &str {
        self.id.split('@').next().unwrap_or(&self.id)
    }
}

/// A test left out of a plan for a board it is unsupported on (see
/// [`TestCase::unsupported`](crate::testcase::TestCase::unsupported)).
#[derive(Debug, Serialize, Deserialize)]
pub struct UnsupportedTest {
    pub id: String,
    pub board: BoardKind,
    pub reason: String,
}

impl UnsupportedTest {
    fn test_id(&self) -> &str {
        self.id.split('@').next().unwrap_or(&self.id)
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Plan {
    pub artifacts: BTreeMap<Label, BuildSpec>,
    pub tests: Vec<PlannedTest>,
    /// The selected tests left out as unsupported on their board, for
    /// reporting.
    #[serde(default)]
    pub unsupported: Vec<UnsupportedTest>,
}

impl Plan {
    /// Keep only the tests `selection` selects, and the artifacts they use.
    ///
    /// An `anyboard` selector picks, for each test it matches that no other
    /// selector already includes, one of the test's boards: one that other
    /// selected tests already use if possible, so that the plan needs fewer
    /// kernels (and hosts), and otherwise the first.
    pub fn select(mut self, selection: &Selection) -> Plan {
        if !selection.tests.is_empty() {
            let (fixed, any): (Vec<&Selector>, Vec<&Selector>) = selection
                .tests
                .iter()
                .partition(|s| s.board != BoardSelector::Any);
            let mut keep = vec![false; self.tests.len()];
            for selector in &fixed {
                let mut matched = false;
                for (keep, test) in keep.iter_mut().zip(&self.tests) {
                    if selector.matches(test.test_id(), test.board) {
                        *keep = true;
                        matched = true;
                    }
                }
                self.assert_matched(selector, matched);
            }

            let mut boards: BTreeSet<BoardKind> = (self.tests.iter().zip(&keep))
                .filter(|(_, keep)| **keep)
                .map(|(t, _)| t.board)
                .collect();
            for selector in &any {
                // Each test's candidate planned tests, in plan order.
                let mut candidates: BTreeMap<&str, Vec<usize>> = BTreeMap::new();
                for (i, test) in self.tests.iter().enumerate() {
                    if selector.matches(test.test_id(), test.board) {
                        candidates.entry(test.test_id()).or_default().push(i);
                    }
                }
                self.assert_matched(selector, !candidates.is_empty());
                for indices in candidates.values() {
                    if indices.iter().any(|&i| keep[i]) {
                        continue;
                    }
                    let pick = *indices
                        .iter()
                        .find(|&&i| boards.contains(&self.tests[i].board))
                        .unwrap_or(&indices[0]);
                    keep[pick] = true;
                    boards.insert(self.tests[pick].board);
                }
            }

            let tests = std::mem::take(&mut self.tests);
            self.tests = tests
                .into_iter()
                .zip(keep)
                .filter_map(|(test, keep)| keep.then_some(test))
                .collect();
            self.unsupported.retain(|u| {
                selection
                    .tests
                    .iter()
                    .any(|s| s.matches(u.test_id(), u.board))
            });
        }

        let used: BTreeSet<&Label> = self
            .tests
            .iter()
            .flat_map(|t| std::iter::once(&t.kernel).chain(&t.apps))
            .collect();
        self.artifacts.retain(|label, _| used.contains(label));
        self
    }

    /// Panic unless `selector` `matched` a planned test: a selector that
    /// matches nothing is almost certainly a typo, or selects only tests that
    /// are unsupported on their boards, which says why.
    fn assert_matched(&self, selector: &Selector, matched: bool) {
        if matched {
            return;
        }
        let unsupported: Vec<String> = self
            .unsupported
            .iter()
            .filter(|u| selector.matches(u.test_id(), u.board))
            .map(|u| format!("{} ({})", u.id, u.reason))
            .collect();
        if unsupported.is_empty() {
            panic!("--test {selector} matches no planned test");
        }
        panic!(
            "--test {selector} matches only unsupported tests: {}",
            unsupported.join(", ")
        );
    }

    /// A human-readable summary: what to build, and the tests per board.
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
        let mut by_board: BTreeMap<BoardKind, Vec<&str>> = BTreeMap::new();
        for t in &self.tests {
            by_board.entry(t.board).or_default().push(t.test_id());
        }
        for (board, tests) in by_board {
            out += &format!("  {board} ({}): {}\n", tests.len(), tests.join(", "));
        }
        for u in &self.unsupported {
            out += &format!("  unsupported: {} ({})\n", u.id, u.reason);
        }
        out
    }
}

/// Which planned tests to include: those any of `tests` selects, or every
/// test if there are none.
#[derive(Default, clap::Args)]
pub struct Selection {
    /// Include `<test>@<board>`, where the test may be `alltests`, and the
    /// board `allboards`, or `anyboard` for one board the test runs on (e.g.
    /// `hello_world@nrf52840dk`, `alltests@nucleo_f429zi`,
    /// `blink@anyboard`). May be repeated, selecting all of them.
    #[arg(long = "test", value_name = "TEST@BOARD")]
    pub tests: Vec<Selector>,
}

/// One `--test`: a test, or all of them, on a board, all of them, or any one.
#[derive(Debug, Clone, PartialEq)]
pub struct Selector {
    /// `None` for `alltests`.
    pub test: Option<&'static str>,
    pub board: BoardSelector,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BoardSelector {
    One(BoardKind),
    All,
    Any,
}

impl std::str::FromStr for Selector {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, String> {
        let (test, board) = s.split_once('@').ok_or_else(|| {
            format!(
                "expected `<test>@<board>`, where the test may be `alltests`, \
                 and the board `allboards` or `anyboard` (e.g. `{s}@anyboard`)"
            )
        })?;
        let test = match test {
            "alltests" => None,
            _ => Some(
                TESTS
                    .iter()
                    .find(|tc| tc.id == test)
                    .ok_or_else(|| format!("unknown test {test:?}"))?
                    .id,
            ),
        };
        let board = match board {
            "allboards" => BoardSelector::All,
            "anyboard" => BoardSelector::Any,
            _ => BoardSelector::One(board.parse()?),
        };
        Ok(Selector { test, board })
    }
}

impl std::fmt::Display for Selector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let test = self.test.unwrap_or("alltests");
        match self.board {
            BoardSelector::One(board) => write!(f, "{test}@{board}"),
            BoardSelector::All => write!(f, "{test}@allboards"),
            BoardSelector::Any => write!(f, "{test}@anyboard"),
        }
    }
}

impl Selector {
    /// Whether test `test_id` on `board` is one of the tests, on one of the
    /// boards, this selects. For `anyboard`, that is every board it may pick.
    fn matches(&self, test_id: &str, board: BoardKind) -> bool {
        self.test.is_none_or(|id| id == test_id)
            && match self.board {
                BoardSelector::One(b) => b == board,
                BoardSelector::All | BoardSelector::Any => true,
            }
    }
}

/// Plan the tests `selection` selects.
pub fn compute(selection: &Selection) -> Plan {
    let mut artifacts = BTreeMap::new();
    let mut tests = Vec::new();
    let mut unsupported = Vec::new();

    for tc in TESTS {
        for &board in tc.boards {
            if let Some(reason) = tc.unsupported_reason(board) {
                unsupported.push(UnsupportedTest {
                    id: format!("{}@{board}", tc.id),
                    board,
                    reason: reason.to_string(),
                });
                continue;
            }
            let kernel = add_artifact(&mut artifacts, BuildSpec::TockKernel { board });
            let tock_targets: Vec<String> =
                board.tock_targets().iter().map(|t| t.to_string()).collect();
            let apps = tc
                .apps
                .iter()
                .map(|name| {
                    let spec = BuildSpec::LibtockCApp {
                        name: name.to_string(),
                        tock_targets: tock_targets.clone(),
                    };
                    add_artifact(&mut artifacts, spec)
                })
                .collect();

            tests.push(PlannedTest {
                id: format!("{}@{board}", tc.id),
                board,
                kernel,
                apps,
            });
        }
    }

    Plan {
        artifacts,
        tests,
        unsupported,
    }
    .select(selection)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn select(selectors: &[&str]) -> Vec<String> {
        let selection = Selection {
            tests: selectors.iter().map(|s| s.parse().unwrap()).collect(),
        };
        compute(&selection)
            .tests
            .into_iter()
            .map(|t| t.id)
            .collect()
    }

    #[test]
    fn selectors_are_cumulative() {
        assert_eq!(
            select(&["hello_world@nrf52840dk", "blink@nrf52840dk"]),
            ["hello_world@nrf52840dk", "blink@nrf52840dk"]
        );
        let all_nrf = select(&["alltests@nrf52840dk"]);
        assert!(all_nrf.iter().all(|id| id.ends_with("@nrf52840dk")));
        assert!(all_nrf.len() > 1);
        let hello_everywhere = select(&["hello_world@allboards"]);
        assert!(hello_everywhere.len() > 1);
        assert!(
            hello_everywhere
                .iter()
                .all(|id| id.starts_with("hello_world@"))
        );
        assert_eq!(select(&[]).len(), select(&["alltests@allboards"]).len());
    }

    #[test]
    fn anyboard_picks_one_board_preferring_those_already_used() {
        // hello_world runs on every board; its first is qemu_rv64_virt.
        assert_eq!(select(&["hello_world@anyboard"]).len(), 1);
        assert_eq!(
            select(&["blink@nrf52840dk", "hello_world@anyboard"]),
            ["hello_world@nrf52840dk", "blink@nrf52840dk"]
        );
        // A test another selector already includes is not added again.
        assert_eq!(
            select(&["hello_world@nucleo_f429zi", "hello_world@anyboard"]),
            ["hello_world@nucleo_f429zi"]
        );
    }

    #[test]
    fn unsupported_tests_are_left_out_with_their_reason() {
        let plan = compute(&Selection::default());
        let id = "console_timeout@esp32-c3-devkitm-1";
        assert!(plan.tests.iter().all(|t| t.id != id));
        assert!(plan.unsupported.iter().any(|u| u.id == id));
        // Selecting other tests leaves it out of the unsupported ones too.
        let plan = compute(&Selection {
            tests: vec!["hello_world@allboards".parse().unwrap()],
        });
        assert!(plan.unsupported.is_empty());
    }

    #[test]
    #[should_panic(expected = "matches only unsupported tests")]
    fn selecting_only_unsupported_tests_says_why() {
        select(&["console_timeout@esp32-c3-devkitm-1"]);
    }

    #[test]
    fn unsupported_boards_are_boards_of_the_test() {
        for tc in TESTS {
            for (board, _) in tc.unsupported {
                assert!(tc.boards.contains(board), "{}: {board}", tc.id);
            }
        }
    }

    #[test]
    fn selectors_need_a_known_test_and_board() {
        assert!("hello_world".parse::<Selector>().is_err());
        assert!("no_such_test@anyboard".parse::<Selector>().is_err());
        assert!("hello_world@no_such_board".parse::<Selector>().is_err());
        assert_eq!(
            "alltests@anyboard".parse::<Selector>().unwrap().to_string(),
            "alltests@anyboard"
        );
    }
}
