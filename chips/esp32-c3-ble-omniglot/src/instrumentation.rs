// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

//! Debug instrumentation for the sandboxed BLE controller. The controller
//! does not need any of it; build with `BLE_DEBUG=0` to turn it off.
//!
//! - Every 10 s: calls into the sandbox per kind (rate, average and longest
//!   duration, CPU share), interrupts per BT source (rate and longest gap
//!   between two), and emulated peripheral accesses per page. The EM
//!   mapping registers are in page `0x60031000`.
//! - At once: a gap of at least `GAP_REPORT_US` between two interrupts from
//!   the same BT source, and packets for the host or log output that the
//!   sandbox dropped (checked every `DROPS_CHECK_US`).
//! - After the bring-up: its duration, the sandbox's heap and stack use,
//!   and the cost of an empty call.
//! - Fault injection: built with `BLE_FAULT_TEST=<kind>` (one digit, an
//!   `SANDBOX_TEST_*` value from `sandbox.h`), the sandbox faults in that way
//!   20 s after the bring-up.
//! - `DISCOVERY`: also emulate peripheral accesses that are not on the
//!   allowlist, and report them.

use core::cell::Cell;

use kernel::debug;

use super::bindings::{self as b, Sandbox as _};
use super::{BT_SOURCES, BleOmniglot, Call, int, now_us};

/// State shared between the main loop and the trap handler, which emulates
/// the sandbox's accesses.
///
/// The trap handler only runs sandbox-related code while the main loop is
/// inside a call into the sandbox, so the two never access it concurrently
/// on this single-core chip.
struct TrapShared<T>(T);

// ### Safety
//
// See `TrapShared`: never accessed concurrently.
unsafe impl<T> Sync for TrapShared<T> {}

impl<T> core::ops::Deref for TrapShared<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.0
    }
}

pub const ENABLED: bool = match option_env!("BLE_DEBUG") {
    Some(v) => !(v.len() == 1 && v.as_bytes()[0] == b'0'),
    None => true,
};

/// Also emulate (and report) peripheral accesses outside the allowlist,
/// except to `DENY_MMIO`.
const DISCOVERY: bool = false;

/// Never emulated, even in discovery mode: flash (SPI0/1), GDMA, permission
/// control, the interrupt matrix, the cache/MMU, and the world controller.
const DENY_MMIO: &[(usize, usize)] = &[
    (0x6000_2000, 0x6000_4000),
    (0x6003_F000, 0x6004_0000),
    (0x600C_1000, 0x600D_1000),
];

const REPORT_INTERVAL_US: u64 = 10_000_000;

/// A gap between two interrupts from one BT source that is reported at
/// once. During a connection, RWBLE interrupts at least once per connection
/// event (7.5 ms to 4 s apart, as the central chooses). While advertising,
/// it interrupts once per advertising event (NimBLE's default: 30-60 ms).
/// While neither runs, any gap is normal.
const GAP_REPORT_US: u64 = 100_000;

/// How often to check whether the sandbox dropped packets or log output.
const DROPS_CHECK_US: u64 = 100_000;

const FAULT_TEST: u32 = match option_env!("BLE_FAULT_TEST") {
    Some(kind) if kind.len() == 1 => (kind.as_bytes()[0] - b'0') as u32,
    _ => 0,
};
const FAULT_TEST_AFTER_US: u64 = 20_000_000;

/// The CPU runs at 160 MHz.
const CYCLES_PER_US: u64 = 160;

/// Calls of one kind into the sandbox.
#[derive(Clone, Copy, Default)]
struct CallStats {
    count: u32,
    cycles: u64,
    max: u32,
}

const NO_CALLS: CallStats = CallStats {
    count: 0,
    cycles: 0,
    max: 0,
};

/// Interrupts from one BT source.
#[derive(Clone, Copy)]
struct IrqStats {
    /// When the last one was serviced (us), or 0.
    last: u64,
    /// Since the last report.
    count: u32,
    /// The longest gap between two since the last report (us).
    max_gap: u64,
}

const NO_IRQS: IrqStats = IrqStats {
    last: 0,
    count: 0,
    max_gap: 0,
};

/// Accesses to one page (or, in discovery mode, one address): (key, reads,
/// writes).
type AccessCount = (u32, u32, u32);

struct State {
    calls: [Cell<CallStats>; Call::COUNT],
    since: Cell<u64>,
    /// Indexed like `BT_SOURCES`.
    irqs: [Cell<IrqStats>; BT_SOURCES.len()],
    emulated: [Cell<AccessCount>; 32],
    unlisted: [Cell<AccessCount>; 32],
    /// The sandbox's drop counters (packets for the host, log bytes) when
    /// last checked, and when that was (us).
    drops: Cell<(u32, u32)>,
    drops_checked: Cell<u64>,
    /// When the fault test is due, or 0.
    fault_test_at: Cell<u64>,
}

static STATE: TrapShared<State> = TrapShared(State {
    calls: [const { Cell::new(NO_CALLS) }; Call::COUNT],
    since: Cell::new(0),
    irqs: [const { Cell::new(NO_IRQS) }; BT_SOURCES.len()],
    emulated: [const { Cell::new((0, 0, 0)) }; 32],
    unlisted: [const { Cell::new((0, 0, 0)) }; 32],
    drops: Cell::new((0, 0)),
    drops_checked: Cell::new(0),
    fault_test_at: Cell::new(0),
});

fn irq_name(irq: u32) -> &'static str {
    match irq {
        esp32_c3::interrupts::IRQ_BT_BB => "BT_BB",
        esp32_c3::interrupts::IRQ_RWBT => "RWBT",
        esp32_c3::interrupts::IRQ_RWBLE => "RWBLE",
        _ => "?",
    }
}

/// Account for a BT interrupt that is about to be serviced, and report a
/// long gap since the previous one from the same source.
pub fn interrupt(irq: u32) {
    if !ENABLED {
        return;
    }
    let Some(i) = BT_SOURCES.iter().position(|&s| s == irq) else {
        return;
    };
    let now = now_us();
    let cell = &STATE.irqs[i];
    let mut s = cell.get();
    if s.last != 0 {
        let gap = now - s.last;
        s.max_gap = s.max_gap.max(gap);
        if gap >= GAP_REPORT_US {
            debug!(
                "ble: {} interrupt after {} ms without",
                irq_name(irq),
                gap / 1000
            );
        }
    }
    s.last = now;
    s.count += 1;
    cell.set(s);
}

/// CPU cycles (Espressif's machine performance counter, `mpccr`).
fn cycles() -> u32 {
    let c: usize;
    // ### Safety
    //
    // Reading the performance counter has no side effects.
    unsafe { core::arch::asm!("csrr {0}, 0x7e2", out(reg) c, options(nomem, nostack)) };
    c as u32
}

/// Run `f`, a call of kind `kind` into the sandbox, and account for its
/// duration.
pub fn time_call<R>(kind: Call, f: impl FnOnce() -> R) -> R {
    if !ENABLED {
        return f();
    }
    let start = cycles();
    let r = f();
    let elapsed = cycles().wrapping_sub(start);
    let cell = &STATE.calls[kind as usize];
    let mut s = cell.get();
    s.count += 1;
    s.cycles += elapsed as u64;
    s.max = s.max.max(elapsed);
    cell.set(s);
    r
}

fn count(table: &[Cell<AccessCount>], key: u32, store: bool) {
    if let Some(cell) = table.iter().find(|c| c.get().0 == key || c.get().0 == 0) {
        let (_, reads, writes) = cell.get();
        cell.set(if store {
            (key, reads, writes + 1)
        } else {
            (key, reads + 1, writes)
        });
    }
}

/// Count an emulated access (from the trap handler).
pub fn count_emulated(addr: usize, store: bool) {
    if ENABLED {
        count(&STATE.emulated, (addr & !0xFFF) as u32, store);
    }
}

/// In discovery mode, whether to emulate an access outside the allowlist;
/// it is reported as unlisted.
pub fn discover(addr: usize) -> bool {
    let allow = ENABLED
        && DISCOVERY
        && (0x6000_0000..0x600D_1000).contains(&addr)
        && !DENY_MMIO.iter().any(|&(s, e)| (s..e).contains(&addr));
    if allow {
        count(&STATE.unlisted, addr as u32, false);
    }
    allow
}

/// One of the sandbox's `SANDBOX_STAT_*` values, or 0 if the call fails.
fn stat(bt: &BleOmniglot, which: u32) -> u32 {
    bt.call(Call::Stat, |sandbox, alloc, access| {
        int(sandbox.sandbox_stat(which, alloc, access))
    })
    .unwrap_or(0)
}

/// Report the bring-up, which took `us`, and measure an empty call.
pub fn started(bt: &BleOmniglot, us: u64) {
    if !ENABLED {
        return;
    }
    debug!(
        "ble: started in {} ms; heap {}/{} B (peak), stacks: task {} B, controller {} B",
        us / 1000,
        stat(bt, b::SANDBOX_STAT_HEAP_USED),
        stat(bt, b::SANDBOX_STAT_HEAP_PEAK),
        stat(bt, b::SANDBOX_STAT_TASK_STACK),
        stat(bt, b::SANDBOX_STAT_CONTROLLER_STACK),
    );

    const N: u32 = 1000;
    let start = cycles();
    for _ in 0..N {
        stat(bt, b::SANDBOX_STAT_RX_DROPS);
    }
    let per_call = cycles().wrapping_sub(start) / N;
    debug!(
        "ble: empty call {} cycles ({} ns)",
        per_call,
        per_call as u64 * 1000 / CYCLES_PER_US
    );

    let now = now_us();
    for c in &STATE.calls {
        c.set(NO_CALLS);
    }
    for c in &STATE.irqs {
        c.set(NO_IRQS);
    }
    // A reload resets the sandbox's counters.
    STATE.drops.set((
        stat(bt, b::SANDBOX_STAT_RX_DROPS),
        stat(bt, b::SANDBOX_STAT_LOG_DROPS),
    ));
    STATE.drops_checked.set(now);
    STATE.since.set(now);
    if FAULT_TEST != 0 {
        STATE.fault_test_at.set(now + FAULT_TEST_AFTER_US);
    }
}

/// From the deferred call: run the fault test once it is due, and report
/// every `REPORT_INTERVAL_US`.
pub fn tick(bt: &BleOmniglot) {
    if !ENABLED {
        return;
    }
    let now = now_us();
    let at = STATE.fault_test_at.get();
    if at != 0 && now >= at {
        STATE.fault_test_at.set(0);
        debug!("ble: fault test {}", FAULT_TEST);
        let r = bt.call(Call::Test, |sandbox, alloc, access| {
            int(sandbox.sandbox_test(FAULT_TEST, alloc, access))
        });
        debug!("ble: fault test {} returned {:?}", FAULT_TEST, r);
        return;
    }
    if now - STATE.drops_checked.get() >= DROPS_CHECK_US {
        STATE.drops_checked.set(now);
        check_drops(bt);
    }
    let window = now - STATE.since.get();
    if window >= REPORT_INTERVAL_US {
        STATE.since.set(now);
        report(window);
    }
}

/// Report packets for the host and log output that the sandbox dropped
/// since the last check. A dropped Number Of Completed Packets event costs
/// the host one of the controller's ACL buffers for good.
fn check_drops(bt: &BleOmniglot) {
    let rx = stat(bt, b::SANDBOX_STAT_RX_DROPS);
    let log = stat(bt, b::SANDBOX_STAT_LOG_DROPS);
    let (rx_before, log_before) = STATE.drops.replace((rx, log));
    if rx != rx_before {
        debug!(
            "ble: the sandbox dropped {} packets for the host ({} in total)",
            rx.wrapping_sub(rx_before),
            rx
        );
    }
    if log != log_before {
        debug!(
            "ble: the sandbox dropped {} bytes of log output ({} in total)",
            log.wrapping_sub(log_before),
            log
        );
    }
}

fn report(window_us: u64) {
    let per_s = |n: u64| n * 1_000_000 / window_us;

    for (irq, cell) in BT_SOURCES.iter().zip(&STATE.irqs) {
        let s = cell.get();
        cell.set(IrqStats {
            last: s.last,
            ..NO_IRQS
        });
        if s.count == 0 {
            continue;
        }
        debug!(
            "ble: irq {:5} {:5}/s, longest gap {} us",
            irq_name(*irq),
            per_s(s.count as u64),
            s.max_gap
        );
    }

    let mut total = NO_CALLS;
    for kind in Call::ALL {
        let s = STATE.calls[kind as usize].take();
        if s.count == 0 {
            continue;
        }
        total.count += s.count;
        total.cycles += s.cycles;
        debug!(
            "ble: {:5} {:5}/s avg {:4} us max {:5} us",
            kind.name(),
            per_s(s.count as u64),
            s.cycles / CYCLES_PER_US / s.count as u64,
            s.max as u64 / CYCLES_PER_US
        );
    }
    let us = total.cycles / CYCLES_PER_US;
    debug!(
        "ble: total {:5}/s, {} us/s in the sandbox ({}.{:02}% CPU)",
        per_s(total.count as u64),
        per_s(us),
        us * 100 / window_us,
        us * 10_000 / window_us % 100
    );

    for cell in &STATE.emulated {
        let (page, reads, writes) = cell.take();
        if page != 0 {
            debug!(
                "ble: emulated {:#010x}: {} reads, {} writes ({}/s)",
                page,
                reads,
                writes,
                per_s((reads + writes) as u64)
            );
        }
    }
    for cell in &STATE.unlisted {
        let (addr, reads, writes) = cell.get();
        if addr != 0 {
            debug!(
                "ble: unlisted {:#010x}: {} reads, {} writes (total)",
                addr, reads, writes
            );
        }
    }
}
