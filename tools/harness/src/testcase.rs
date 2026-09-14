// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

use crate::board::{Board, BoardKind, Gpio, Uart};
use crate::requirement::Requirement;
use std::time::Duration;

pub struct TestCase {
    pub id: &'static str,
    pub boards: &'static [BoardKind],
    pub requires: &'static [Requirement],
    pub apps: &'static [&'static str],
    /// Boards of `boards` the test cannot pass on although they have what it
    /// requires, e.g. for a feature their kernel lacks or a known bug, and
    /// why. `plan` leaves them out, recording the reason.
    pub unsupported: &'static [(BoardKind, &'static str)],
    pub body: TestBody,
}

impl TestCase {
    /// Why the test cannot pass on `board`, if it is unsupported there.
    pub fn unsupported_reason(&self, board: BoardKind) -> Option<&'static str> {
        self.unsupported
            .iter()
            .find(|(b, _)| *b == board)
            .map(|(_, reason)| *reason)
    }
}

pub enum TestBody {
    UartSequence {
        needles: &'static [&'static str],
        timeout: Duration,
    },
    Run {
        run: fn(&mut TestCtx) -> Result<(), String>,
        /// The longest `run` may take, with all of its waits timing out: the
        /// test fails if it takes longer, and `treadmill` leases jobs by it.
        time_limit: Duration,
    },
}

impl TestBody {
    pub fn run(&self, ctx: &mut TestCtx) -> Result<(), String> {
        match self {
            TestBody::UartSequence { needles, timeout } => needles
                .iter()
                .try_for_each(|needle| ctx.uart().wait_for(needle, *timeout).map(drop)),
            TestBody::Run { run, .. } => run(ctx),
        }
    }

    /// The longest the body may take to run.
    pub fn time_limit(&self) -> Duration {
        match self {
            TestBody::UartSequence { needles, timeout } => *timeout * needles.len() as u32,
            TestBody::Run { time_limit, .. } => *time_limit,
        }
    }
}

pub struct TestCtx<'a> {
    board: &'a mut dyn Board,
    requires: &'static [Requirement],
}

impl<'a> TestCtx<'a> {
    pub fn new(board: &'a mut dyn Board, requires: &'static [Requirement]) -> Self {
        TestCtx { board, requires }
    }

    /// The kind of board the test runs on, for tests whose expectations
    /// differ between boards.
    pub fn board(&self) -> BoardKind {
        self.board.kind()
    }

    pub fn uart(&mut self) -> &mut dyn Uart {
        assert!(
            self.requires.contains(&Requirement::Uart),
            "test used the UART without declaring `Requirement::Uart`"
        );
        self.board.uart().expect("board does not provide a UART")
    }

    /// Reset the board, keeping its flashed image, and reopen its console.
    pub fn restart(&mut self) -> Result<(), String> {
        self.board.restart()
    }

    pub fn gpio(&mut self, name: &str) -> &mut dyn Gpio {
        assert!(
            self.requires
                .iter()
                .any(|r| matches!(r, Requirement::Gpio { name: n, .. } if *n == name)),
            "test used GPIO {name:?} without declaring it in `requires`"
        );
        self.board
            .gpio(name)
            .unwrap_or_else(|| panic!("board does not provide GPIO {name:?}"))
    }
}
