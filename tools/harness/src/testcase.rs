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
    pub body: TestBody,
}

pub enum TestBody {
    UartSequence {
        needles: &'static [&'static str],
        timeout: Duration,
    },
    Run(fn(&mut TestCtx) -> Result<(), String>),
}

impl TestBody {
    pub fn run(&self, ctx: &mut TestCtx) -> Result<(), String> {
        match self {
            TestBody::UartSequence { needles, timeout } => needles
                .iter()
                .try_for_each(|needle| ctx.uart().wait_for(needle, *timeout).map(drop)),
            TestBody::Run(f) => f(ctx),
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

    pub fn uart(&mut self) -> &mut dyn Uart {
        assert!(
            self.requires.contains(&Requirement::Uart),
            "test used the UART without declaring `Requirement::Uart`"
        );
        self.board.uart().expect("board does not provide a UART")
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
