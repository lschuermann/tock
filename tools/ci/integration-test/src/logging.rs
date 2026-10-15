// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

//! Logger which writes to stderr.
//!
//! Each line is prefixed with a tag for its log level (e.g., `[INFO]`). On a
//! terminal, these tags are colored unless `NO_COLOR` is set. Without color
//! (e.g., in the pipe through which `treadmill` reads the output of a remote
//! `run`), [`split_tag`] parses the tags back.
//!
//! A thread which works for one of several jobs running at once labels its
//! lines with the job's label (see [`set_label`]), following the tag (e.g.,
//! `[INFO] nrf52840dk | ...`).

use log::{Level, LevelFilter};
use std::cell::RefCell;
use std::io::{IsTerminal, Write};

thread_local! {
    /// Label of the lines of this thread, padded, and its color.
    static LABEL: RefCell<Option<(String, &'static str)>> = const { RefCell::new(None) };
}

/// SGR parameters of the labels on a terminal, by the label's index. These
/// colors differ from those of the tags.
const LABEL_COLORS: &[&str] = &["36", "35", "34", "96", "95", "94"];

/// Indentation of continuation lines in multi-line messages. It is as wide as a
/// tag and the following space, such that all lines of a message align.
const CONTINUATION: &str = "       ";

/// Return the tag of `level`.
fn tag(level: Level) -> &'static str {
    match level {
        Level::Error => "[ ERR]",
        Level::Warn => "[WARN]",
        Level::Info => "[INFO]",
        Level::Debug => "[ DBG]",
        Level::Trace => "[TRCE]",
    }
}

/// Return the SGR parameters to color the tag of `level` on a terminal.
fn color(level: Level) -> &'static str {
    match level {
        Level::Error => "1;31",
        Level::Warn => "1;33",
        Level::Info => "32",
        Level::Debug | Level::Trace => "2",
    }
}

/// Label the lines which this thread logs from now on with `label`, padded to
/// `width`, and colored by `index` on a terminal.
pub fn set_label(label: &str, width: usize, index: usize) {
    let color = LABEL_COLORS[index % LABEL_COLORS.len()];
    LABEL.with(|l| *l.borrow_mut() = Some((format!("{label:<width$}"), color)));
}

/// Initialize the logger.
///
/// The default level is info. A `verbose` of 1 selects debug, and 2 or more
/// selects trace. The `RUST_LOG` environment variable overrides this level.
pub fn init(verbose: u8) {
    let level = match verbose {
        0 => LevelFilter::Info,
        1 => LevelFilter::Debug,
        _ => LevelFilter::Trace,
    };
    let colored = std::io::stderr().is_terminal() && std::env::var_os("NO_COLOR").is_none();
    env_logger::Builder::new()
        .filter_level(level)
        .parse_default_env()
        .format(move |buf, record| {
            let level = record.level();
            let message = record.args().to_string();
            // Label every line of the message, such that the lines of several
            // jobs can be told apart when they interleave:
            let label = LABEL.with(|label| match &*label.borrow() {
                Some((label, color)) if colored => format!("\x1b[{color}m{label}\x1b[0m | "),
                Some((label, _)) => format!("{label} | "),
                None => String::new(),
            });
            let mut lines = message.lines();
            let first = lines.next().unwrap_or("");
            if colored {
                write!(buf, "\x1b[{}m{}\x1b[0m ", color(level), tag(level))?;
            } else {
                write!(buf, "{} ", tag(level))?;
            }
            write!(buf, "{label}{first}")?;
            for line in lines {
                write!(buf, "\n{CONTINUATION}{label}{line}")?;
            }
            writeln!(buf)
        })
        .init();
}

/// Split a line which this logger wrote without color into its level and
/// message.
///
/// For a line which continues a message, the level is `None`. This returns
/// `None` for a line which this logger did not write.
pub fn split_tag(line: &str) -> Option<(Option<Level>, &str)> {
    if let Some(rest) = line.strip_prefix(CONTINUATION) {
        return Some((None, rest));
    }
    [
        Level::Error,
        Level::Warn,
        Level::Info,
        Level::Debug,
        Level::Trace,
    ]
    .into_iter()
    .find_map(|level| {
        let rest = line.strip_prefix(tag(level))?;
        let rest = rest.strip_prefix(' ').or((rest.is_empty()).then_some(""))?;
        Some((Some(level), rest))
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tagged_lines_split_into_level_and_message() {
        assert_eq!(split_tag("[ ERR] boom"), Some((Some(Level::Error), "boom")));
        assert_eq!(split_tag("[INFO]"), Some((Some(Level::Info), "")));
        assert_eq!(split_tag("       more"), Some((None, "more")));
        assert_eq!(split_tag("[INFO]x"), None);
        assert_eq!(split_tag("+--------+"), None);
    }
}
