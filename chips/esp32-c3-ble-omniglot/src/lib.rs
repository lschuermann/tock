// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

//! Espressif's ESP32-C3 BLE controller, run in an Omniglot sandbox.
//!
//! The controller (mask ROM plus the archives) and its OS adapter are linked
//! into the sandbox (`sandbox/`, built by `sandbox.mk`), which runs in user
//! mode under the PMP. A board that uses this crate writes the sandbox into
//! its kernel binary and passes its memory regions to
//! [`BleOmniglot::create`]. Inside the sandbox, the controller task and the
//! bring-up are green threads, so every call into it runs to completion and
//! returns a status word saying what to do next:
//!
//! - BT interrupts are serviced through Tock's deferred interrupt path, by
//!   calling `sandbox_isr`.
//! - A deferred call runs the green threads (`sandbox_run`) whenever one can
//!   run, or waits with a timeout (polled, as only the bring-up does that).
//! - After every call, packets for the host and log output are drained.
//!
//! The sandbox may access its own flash, RAM and `.iram1` code, the ROM's
//! RAM, the mask ROM, and the two peripheral pages it uses most (see
//! `pmp_layout`). Its other peripheral accesses fault, and the kernel
//! performs those that [`Emulation`] allows. That includes stores to the
//! baseband's DMA mapping registers that keep every DMA window inside the
//! sandbox's RAM.
//!
//! The driver implements [`HciTransport`]. The controller is stopped after
//! boot. [`HciTransport::reset`] runs the bring-up asynchronously.
//!
//! If a call into the sandbox fails (a fault, or an access the kernel would
//! not emulate), the kernel stops the radio and reports
//! the fault. The transport then reports that it stopped. A later reset
//! reloads the sandbox from flash. It also restores the ROM's RAM to the
//! contents that the sandbox found at its first start. The bring-up then
//! runs again.

#![no_std]

use core::cell::Cell;

use kernel::ErrorCode;
use kernel::debug;
use kernel::deferred_call::{DeferredCall, DeferredCallClient};
use kernel::hil::bluetooth_hci::{HciTransport, HciTransportClient, MAX_PACKET_LEN, PacketType};
use kernel::platform::mpu::Permissions::{ReadExecuteOnly, ReadOnly, ReadWriteOnly};
use kernel::utilities::cells::{MapCell, OptionalCell, TakeCell};

use esp32_c3::intc::Intc;
use esp32_c3::interrupts;
use esp32_c3::syscon::Syscon;
use esp32_c3::systimer::SysTimer;
use omniglot::foreign_memory::og_copy::OGCopy;
use omniglot::rt::OGRuntime;
use omniglot::{OGError, OGResult};
use omniglot_tock::rv32i_c_rt::{ForeignMemory, OGProtection};
use rv32i::pmp::{TORUserPMP, UserPMPEntry};

// Generated from `sandbox/sandbox.h` (see `sandbox.mk`).
#[allow(
    non_upper_case_globals,
    non_camel_case_types,
    dead_code,
    clippy::redundant_static_lifetimes,
    clippy::redundant_field_names
)]
mod bindings;
mod emulation;
mod instrumentation;

use bindings::{self as b, Sandbox as _};
pub use emulation::Emulation;

/// Interrupt sources (and, as Tock maps them 1:1, CPU lines) of the BT blocks.
const BT_SOURCES: [u32; 3] = [
    interrupts::IRQ_BT_BB,
    interrupts::IRQ_RWBT,
    interrupts::IRQ_RWBLE,
];

/// The mask ROM's RAM (start, end), which the sandbox shares: the ROM's BT
/// and PHY code keep their state there.
const ROM_RAM: (usize, usize) = (0x3FCD_F000, 0x3FCE_0000);

/// A copy of the ROM's RAM.
///
/// The driver copies the ROM's RAM before the first bring-up. It restores
/// this copy before every later bring-up.
pub type RomRamSnapshot = [u8; ROM_RAM.1 - ROM_RAM.0];

/// SRAM's instruction-bus alias minus its data-bus alias.
const IRAM_DBUS_OFFSET: usize = 0x70_0000;

/// Bring-up must complete within this time.
const START_TIMEOUT_US: u64 = 15_000_000;

/// The memory the board reserves for the sandbox, each as (start, end).
///
/// The addresses must match `sandbox/layout.ld`.
#[derive(Clone, Copy)]
pub struct Regions {
    /// Flash that only the kernel reads: the sandbox's header, its function
    /// table, and the load copies of its `.data` and `.iram1`.
    pub load: (usize, usize),
    /// Flash with the sandbox's code (`.text` and `.rodata`).
    pub flash: (usize, usize),
    /// SRAM for the sandbox's `.iram1` code, at its instruction-bus alias.
    pub iram: (usize, usize),
    /// SRAM for the sandbox's data, stacks and heap.
    pub ram: (usize, usize),
}

// ---------------------------------------------------------------------------
// Omniglot runtime

/// Omniglot instance ID for the sandbox.
///
/// `OGLifetimeBranding` ties a runtime to a closure; a board-local ID type
/// lets the runtime and its scopes live in `'static` storage instead.
#[derive(Debug)]
pub struct SandboxId(());

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd)]
pub struct SandboxIdImprint;

// ### Safety
//
// Omniglot requires each `OGID` type to be used for at most one runtime
// instance. `SandboxId` has a private field, and is only constructed in
// `BleOmniglot::create`, which may only be called once.
unsafe impl omniglot::id::OGID for SandboxId {
    type Imprint = SandboxIdImprint;

    fn get_imprint(&self) -> SandboxIdImprint {
        SandboxIdImprint
    }
}

// ### Safety
//
// There is only one `SandboxId` instance, so a constant numeric ID is unique.
unsafe impl omniglot::id::OGIDImprint for SandboxIdImprint {
    fn numeric_id(&self) -> u64 {
        0
    }
}

pub type SandboxRuntime = omniglot_tock::rv32i_c_rt::TockRv32iCRt<SandboxId, SandboxPmp>;
pub type Sandbox = b::SandboxRt<SandboxId, SandboxRuntime, &'static SandboxRuntime>;
pub type SandboxAllocScope = omniglot::markers::AllocScope<
    'static,
    omniglot_tock::rv32i_c_rt::TockRv32iCRtAllocChain<'static>,
    SandboxId,
>;
pub type SandboxAccessScope = omniglot::markers::AccessScope<SandboxId>;

/// The integer a call returned. Every bit pattern is a valid integer, so only
/// the call itself can fail.
fn int<T: omniglot::bit_pattern_validate::BitPatternValidate + 'static>(
    r: OGResult<OGCopy<T>>,
) -> OGResult<T> {
    r?.validate().map_err(|_| OGError::InternalError)
}

// ---------------------------------------------------------------------------
// Memory protection

/// The chip's PMP, and how many user-mode entries it has.
pub type Pmp = rv32i::pmp::simple::SimplePMP<PMP_ENTRIES>;
const PMP_ENTRIES: usize = 14;

/// The RWBLE (link layer) page, (start, end).
const RWBLE: (usize, usize) = (0x6003_1000, 0x6003_2000);

/// The BT baseband page.
const BT_BB: usize = 0x6001_1000;

/// The sandbox's PMP entries: all 14. The C3 does not honour the priority
/// of overlapping entries (a read-only entry ahead of an overlapping RW one
/// does not restrict user mode), so no two regions overlap.
fn pmp_layout(
    (flash, flash_end): (usize, usize),
    (iram, iram_end): (usize, usize),
    (ram, ram_end): (usize, usize),
) -> [UserPMPEntry; PMP_ENTRIES] {
    let napot = |start: usize, end: usize, perm| {
        UserPMPEntry::napot(start as *const u8, end - start, perm).unwrap()
    };
    let tor_start = |start: usize| UserPMPEntry::tor_start(start as *const u8);
    let tor = |end: usize, perm| UserPMPEntry::tor(end as *const u8, perm);
    let [(bank1, bank1_end), (bank2, bank2_end)] = emulation::EM_BANKS;
    [
        napot(flash, flash_end, ReadExecuteOnly),
        // Sandbox RAM. It ends below the kernel's `.iram` code (layout.ld),
        // which the sandbox must not reach, so it cannot share a NAPOT
        // region with the ROM's RAM above.
        tor_start(ram),
        tor(ram_end, ReadWriteOnly),
        napot(ROM_RAM.0, ROM_RAM.1, ReadWriteOnly),
        // Mask ROM data, through the DROM alias.
        napot(0x3FF0_0000, 0x3FF2_0000, ReadOnly),
        // Mask ROM code (384 KiB, rounded up).
        napot(0x4000_0000, 0x4008_0000, ReadExecuteOnly),
        napot(iram, iram_end, ReadExecuteOnly),
        // RWBLE, accessed ~4000 times/s with a connection: RW, the EM banks
        // read-only, RW.
        tor_start(RWBLE.0),
        tor(bank1, ReadWriteOnly),
        tor(bank1_end, ReadOnly),
        tor(bank2, ReadWriteOnly),
        tor(bank2_end, ReadOnly),
        tor(RWBLE.1, ReadWriteOnly),
        // The BT baseband, accessed once per radio interrupt.
        napot(BT_BB, BT_BB + 0x1000, ReadWriteOnly),
    ]
}

/// The sandbox's PMP layout, written as raw entries before a call into the
/// sandbox (the kernel rewrites the process layout before a process next
/// runs).
pub struct SandboxPmp {
    pmp: &'static Pmp,
    entries: [UserPMPEntry; PMP_ENTRIES],
    /// Whether the PMP may still hold `entries`.
    loaded: Cell<bool>,
}

impl OGProtection for SandboxPmp {
    fn enable(&self) {
        // `configure_raw` clears the PMP's record of the process
        // configuration it holds, so a record means a process configuration
        // was written since. (The record is the same for any region count.)
        if self.loaded.get()
            && TORUserPMP::<{ PMP_ENTRIES / 2 }>::configured_for(self.pmp).is_none()
        {
            return;
        }
        self.loaded.set(true);
        // ### Safety
        //
        // The entries only grant user mode what the sandbox may access (see
        // `pmp_layout`), and only the sandbox runs in user mode until the
        // kernel rewrites the process layout.
        unsafe { self.pmp.configure_raw(&self.entries) }.unwrap();
    }

    unsafe fn disable(&self) {
        // The entries never restrict machine mode.
    }
}

// ---------------------------------------------------------------------------
// Loading

/// The header at the start of `Regions::load`: addresses the sandbox is
/// linked for (`sandbox/layout.ld`).
#[repr(C)]
#[derive(Clone, Copy)]
struct SandboxHeader {
    magic: u32,
    fntab: usize,
    /// In pointers.
    fntab_length: usize,
    stack_top: usize,
    data_load: usize,
    data: usize,
    data_size: usize,
    iram_load: usize,
    iram: usize,
    iram_size: usize,
    bss: usize,
    bss_size: usize,
}

const SANDBOX_MAGIC: u32 = 0x4345_4C42; // "BLEC"

/// Load the sandbox from flash, and return where everything is.
///
/// The header, function table and load copies are in `load`, the code in
/// `flash`. Checks that everything the header points to lies in these
/// regions, zeroes the sandbox's RAM, and copies `.data` there and `.iram1`
/// to `iram` (through the data-bus alias).
///
/// ### Safety
///
/// Must run before the sandbox does, and the regions must be reserved for
/// it.
unsafe fn load(
    Regions {
        load,
        flash,
        iram,
        ram,
    }: Regions,
) -> Result<ForeignMemory, &'static str> {
    // ### Safety
    //
    // `load` starts with the header, if the sandbox was written into the
    // kernel binary; otherwise the magic does not match.
    let img = unsafe { core::ptr::read(load.0 as *const SandboxHeader) };
    if img.magic != SANDBOX_MAGIC {
        return Err("no sandbox in flash");
    }
    let within = |(start, end): (usize, usize), addr: usize, size: usize| {
        addr >= start && addr.checked_add(size).is_some_and(|e| e <= end)
    };
    let valid = within(load, img.fntab, img.fntab_length * 4)
        && within(ram, img.stack_top, 0)
        && within(load, img.data_load, img.data_size)
        && within(ram, img.data, img.data_size)
        && within(load, img.iram_load, img.iram_size)
        && within(iram, img.iram, img.iram_size)
        && within(ram, img.bss, img.bss_size)
        && [
            img.data_load,
            img.data,
            img.data_size,
            img.iram_load,
            img.iram,
            img.iram_size,
        ]
        .iter()
        .all(|v| v.is_multiple_of(4));
    if !valid {
        return Err("bad sandbox header");
    }
    // ### Safety
    //
    // All ranges were checked to lie in the sandbox's regions, which nothing
    // else uses.
    unsafe {
        core::ptr::write_bytes(ram.0 as *mut u8, 0, ram.1 - ram.0);
        core::ptr::copy_nonoverlapping(
            img.data_load as *const u32,
            img.data as *mut u32,
            img.data_size / 4,
        );
        core::ptr::copy_nonoverlapping(
            img.iram_load as *const u32,
            (img.iram - IRAM_DBUS_OFFSET) as *mut u32,
            img.iram_size / 4,
        );
    }
    debug!(
        "ble: sandbox code {:#x}..{:#x}, iram {} B, ram {:#x}..{:#x}",
        flash.0, flash.1, img.iram_size, ram.0, ram.1
    );
    Ok(ForeignMemory {
        code: (flash.0 as *const (), flash.1 - flash.0),
        ram: (ram.0 as *mut (), ram.1 - ram.0),
        stack_top: img.stack_top as *mut (),
        fntab: (img.fntab as *const *const (), img.fntab_length),
    })
}

/// Count CPU cycles in Espressif's performance counter (`mpcer`: cycles,
/// `mpcmr`: enabled), as ESP-IDF does at startup: the ROM's `ets_delay_us`
/// reads it, as `upccr`.
fn enable_cycle_counter() {
    // ### Safety
    //
    // These Espressif CSRs only configure the performance counter.
    unsafe {
        core::arch::asm!("csrw 0x7e0, {0}", "csrw 0x7e1, {0}", in(reg) 1usize, options(nomem, nostack))
    };
}

/// Microseconds since boot, from SYSTIMER unit 0. The sandbox also latches
/// unit 0, but never runs concurrently with this.
fn now_us() -> u64 {
    SysTimer::new().unit0_us()
}

// ---------------------------------------------------------------------------
// Driver

/// The kinds of calls into the sandbox.
#[derive(Clone, Copy)]
enum Call {
    Start,
    Run,
    Isr,
    Send,
    Recv,
    Log,
    Stat,
    Test,
}

impl Call {
    const COUNT: usize = 8;
    const ALL: [Call; Call::COUNT] = [
        Call::Start,
        Call::Run,
        Call::Isr,
        Call::Send,
        Call::Recv,
        Call::Log,
        Call::Stat,
        Call::Test,
    ];

    fn name(self) -> &'static str {
        match self {
            Call::Start => "start",
            Call::Run => "run",
            Call::Isr => "isr",
            Call::Send => "send",
            Call::Recv => "recv",
            Call::Log => "log",
            Call::Stat => "stat",
            Call::Test => "test",
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
enum State {
    /// The sandbox does not run. Calls into it fail with `OFF`.
    Stopped,
    /// The bring-up runs. It started at the given time in microseconds.
    Starting(u64),
    /// The bring-up has completed.
    Ready,
}

/// Stop the radio for good: disconnect the BT interrupt sources from the
/// CPU, close the baseband's EM windows so that its DMA no longer reaches
/// SRAM, and hold the modem blocks in reset.
///
/// Only the sandbox writes these registers otherwise: the BT sources' map
/// registers (`interrupt_alloc`) and the modem resets (the bring-up).
fn stop_radio(intc: &Intc, emulation: &Emulation) {
    for src in BT_SOURCES {
        intc.disconnect_source(src);
    }
    emulation.disable_em_windows();
    Syscon::new().hold_modem_in_reset();
}

/// Returns the type and the length of the H4 packet at the start of
/// `packet`, as its header declares them.
fn declared_len(packet: &[u8]) -> Option<(PacketType, usize)> {
    let packet_type = PacketType::try_from(*packet.first()?).ok()?;
    let len = match packet_type {
        PacketType::Command => 4 + *packet.get(3)? as usize,
        PacketType::AclData => 5 + u16::from_le_bytes([*packet.get(3)?, *packet.get(4)?]) as usize,
        PacketType::Event => 3 + *packet.get(2)? as usize,
    };
    Some((packet_type, len))
}

/// Returns whether `packet` is one well-formed H4 packet of one of the types
/// in `types`.
fn well_formed(packet: &[u8], types: &[PacketType]) -> bool {
    declared_len(packet).is_some_and(|(t, len)| types.contains(&t) && len == packet.len())
}

pub struct BleOmniglot {
    sandbox: Sandbox,
    regions: Regions,
    emulation: &'static Emulation,
    intc: &'static Intc,
    /// The ROM's RAM before the first bring-up.
    rom_ram: TakeCell<'static, RomRamSnapshot>,
    scopes: MapCell<(SandboxAllocScope, SandboxAccessScope)>,
    deferred_call: DeferredCall,
    client: OptionalCell<&'static dyn HciTransportClient>,
    state: Cell<State>,
    /// Whether `fail` runs. Calls into the sandbox stay allowed while it
    /// runs.
    failing: Cell<bool>,
    /// The sandbox has run since it was last loaded.
    ran: Cell<bool>,
    /// A bring-up has started before. `rom_ram` holds the snapshot.
    started_once: Cell<bool>,
    /// How long the last bring-up took, in microseconds.
    start_us: Cell<u64>,
    /// The result for `reset_done`, once due.
    reset_result: OptionalCell<Result<(), ErrorCode>>,
    /// `stopped` is due to the client.
    stopped_pending: Cell<bool>,
    receiving: Cell<bool>,
    /// Received packets wait in the sandbox.
    rx_pending: Cell<bool>,
    /// The packet from `transmit`, until `transmit_done` returns it.
    tx: TakeCell<'static, [u8]>,
    tx_len: Cell<usize>,
    /// The sandbox has accepted the packet in `tx`.
    tx_sent: Cell<bool>,
    /// The last status from the sandbox had `SANDBOX_TX_READY` set.
    tx_ready: Cell<bool>,
    /// The result for `transmit_done`, once due.
    tx_result: OptionalCell<Result<(), ErrorCode>>,
    /// Log output up to the next newline.
    line: MapCell<([u8; 128], usize)>,
}

impl BleOmniglot {
    /// Load the sandbox and create its Omniglot runtime.
    ///
    /// ### Safety
    ///
    /// Call once, before any process runs. `regions` must be reserved for
    /// the sandbox, and nothing else may use them.
    pub unsafe fn create(
        pmp: &'static Pmp,
        intc: &'static Intc,
        regions: Regions,
        prot_buf: &'static mut core::mem::MaybeUninit<SandboxPmp>,
        emulation_buf: &'static mut core::mem::MaybeUninit<Emulation>,
        rt_buf: &'static mut core::mem::MaybeUninit<SandboxRuntime>,
        rom_ram_buf: &'static mut core::mem::MaybeUninit<RomRamSnapshot>,
    ) -> Result<Self, &'static str> {
        enable_cycle_counter();
        let emulation: &'static Emulation =
            emulation_buf.write(Emulation::new(regions.iram, regions.ram)?);
        let mem = unsafe { load(regions) }?;

        let prot: &'static SandboxPmp = prot_buf.write(SandboxPmp {
            pmp,
            entries: pmp_layout(regions.flash, regions.iram, regions.ram),
            loaded: Cell::new(false),
        });
        let (rt, alloc, access) = unsafe { SandboxRuntime::new(prot, mem, SandboxId(())) };
        let rt: &'static SandboxRuntime = rt_buf.write(rt);
        rt.set_access_emulation(emulation);
        let sandbox = Sandbox::new(rt).ok_or("function table too short")?;

        Ok(Self {
            sandbox,
            regions,
            emulation,
            intc,
            rom_ram: TakeCell::new(rom_ram_buf.write([0; ROM_RAM.1 - ROM_RAM.0])),
            scopes: MapCell::new((alloc, access)),
            deferred_call: DeferredCall::new(),
            client: OptionalCell::empty(),
            state: Cell::new(State::Stopped),
            failing: Cell::new(false),
            ran: Cell::new(false),
            started_once: Cell::new(false),
            start_us: Cell::new(0),
            reset_result: OptionalCell::empty(),
            stopped_pending: Cell::new(false),
            receiving: Cell::new(false),
            rx_pending: Cell::new(false),
            tx: TakeCell::empty(),
            tx_len: Cell::new(0),
            tx_sent: Cell::new(false),
            tx_ready: Cell::new(false),
            tx_result: OptionalCell::empty(),
            line: MapCell::new(([0; 128], 0)),
        })
    }

    /// Run `f` in the sandbox. If the call fails (the sandbox faulted, or
    /// the kernel refused to continue it), the controller stops.
    fn call<R>(
        &self,
        kind: Call,
        f: impl FnOnce(&Sandbox, &mut SandboxAllocScope, &mut SandboxAccessScope) -> OGResult<R>,
    ) -> Result<R, ErrorCode> {
        if self.state.get() == State::Stopped && !self.failing.get() {
            return Err(ErrorCode::OFF);
        }
        let r = self
            .scopes
            .map(|(alloc, access)| {
                instrumentation::time_call(kind, || f(&self.sandbox, alloc, access))
            })
            .expect("ble: reentrant call into the sandbox");
        r.map_err(|_| {
            self.fail(kind);
            ErrorCode::FAIL
        })
    }

    /// Stop the controller after a call into the sandbox failed.
    ///
    /// This stops the radio and reports why the call failed. The client
    /// learns about the failure from the deferred call, because this method
    /// can run in a downcall.
    fn fail(&self, kind: Call) {
        if self.failing.get() || self.state.get() == State::Stopped {
            return;
        }
        let was = self.state.get();
        self.failing.set(true);
        self.state.set(State::Stopped);
        stop_radio(self.intc, self.emulation);
        match self.sandbox.rt().take_fault() {
            Some(f) => debug!(
                "ble: sandbox stopped in {}: {}, mcause {:#x} mtval {:#010x} mepc {:#010x} sp {:#010x}",
                kind.name(),
                f.refused.unwrap_or("fault"),
                f.mcause,
                f.mtval,
                f.mepc,
                f.sp
            ),
            None => debug!("ble: sandbox stopped in {}", kind.name()),
        }
        match self.emulation.take_refused() {
            Some((addr, Some(value))) => {
                debug!("ble: refused store of {:#010x} to {:#010x}", value, addr)
            }
            Some((addr, None)) => debug!("ble: refused load from {:#010x}", addr),
            None => {}
        }
        // What the sandbox logged before, such as an assert, if it still
        // can.
        self.drain_log();
        self.flush_line();
        self.failing.set(false);
        self.stop(was);
    }

    /// Record that the controller stopped while it was in state `was`. The
    /// radio must already be stopped.
    fn stop(&self, was: State) {
        self.state.set(State::Stopped);
        if self.tx.is_some() && self.tx_result.is_none() {
            self.tx_result.set(Err(ErrorCode::FAIL));
        }
        match was {
            State::Starting(_) => self.reset_result.set(Err(ErrorCode::FAIL)),
            State::Ready => self.stopped_pending.set(true),
            State::Stopped => {}
        }
        self.deferred_call.set();
    }

    fn flush_line(&self) {
        self.line.map(|(line, len)| {
            if *len > 0 {
                debug!("{}", core::str::from_utf8(&line[..*len]).unwrap_or("?"));
                *len = 0;
            }
        });
    }

    /// Act on the status that a call returned.
    ///
    /// Log output is printed at once. Packets for the host, completed
    /// transmissions and green threads that can run wait for the deferred
    /// call. This keeps client callbacks out of downcalls.
    fn handle_status(&self, status: u32) {
        if status & b::SANDBOX_LOG != 0 {
            self.drain_log();
        }
        if status & b::SANDBOX_RX != 0 {
            self.rx_pending.set(true);
        }
        self.tx_ready.set(status & b::SANDBOX_TX_READY != 0);
        if let State::Starting(t0) = self.state.get() {
            if status & b::SANDBOX_FAILED != 0 {
                debug!("ble: bring-up failed");
                stop_radio(self.intc, self.emulation);
                self.stop(State::Starting(t0));
            } else if status & b::SANDBOX_STARTED != 0 {
                self.start_us.set(now_us() - t0);
                self.state.set(State::Ready);
                self.reset_result.set(Ok(()));
            }
        }
        let tx_due = self.tx.is_some() && self.tx_ready.get();
        if tx_due
            || self.reset_result.is_some()
            || status & (b::SANDBOX_RX | b::SANDBOX_RUNNABLE | b::SANDBOX_DEADLINE) != 0
        {
            self.deferred_call.set();
        }
    }

    /// Copy what `sandbox_hci_recv` (`Call::Recv`) or `sandbox_log`
    /// (`Call::Log`) returns into `out`, through a buffer of `out.len()` bytes
    /// on the sandbox's stack, and return its length.
    fn read(&self, kind: Call, out: &mut [u8]) -> Result<usize, ErrorCode> {
        let cap = out.len();
        self.call(kind, |sandbox, alloc, access| {
            sandbox
                .rt()
                .allocate_stacked_slice_mut::<u8, _, _>(cap, alloc, |buf, alloc| {
                    let n = match kind {
                        Call::Recv => {
                            sandbox.sandbox_hci_recv(buf.as_ptr(), cap as u32, alloc, access)
                        }
                        _ => sandbox.sandbox_log(buf.as_ptr(), cap as u32, alloc, access),
                    };
                    let n = (int(n)? as usize).min(cap);
                    let buf = buf.validate(access).ok_or(OGError::InternalError)?;
                    out[..n].copy_from_slice(&buf[..n]);
                    Ok(n)
                })
                .and_then(|r| r)
        })
    }

    /// Deliver the packets that wait in the sandbox while receive is
    /// enabled.
    fn drain_rx(&self) {
        let mut packet = [0u8; MAX_PACKET_LEN];
        while self.state.get() == State::Ready && self.receiving.get() {
            match self.read(Call::Recv, &mut packet) {
                Ok(len @ 1..) => {
                    let packet = &packet[..len];
                    if well_formed(packet, &[PacketType::Event, PacketType::AclData]) {
                        self.client.map(|c| c.packet_received(packet));
                    } else {
                        debug!("ble: discarding malformed packet from the controller");
                    }
                }
                _ => return,
            }
        }
        // Receive was disabled, or the controller stopped. More packets can
        // wait in the sandbox.
        self.rx_pending.set(true);
    }

    /// Print the sandbox's log output, line by line.
    fn drain_log(&self) {
        // Bounded: more than the sandbox's log ring holds.
        for _ in 0..64 {
            let mut chunk = [0u8; 64];
            let Ok(n @ 1..) = self.read(Call::Log, &mut chunk) else {
                break;
            };
            self.line.map(|(line, len)| {
                for &c in &chunk[..n] {
                    if c != b'\n' && *len < line.len() {
                        line[*len] = c;
                        *len += 1;
                    }
                    if c == b'\n' || *len == line.len() {
                        debug!("{}", core::str::from_utf8(&line[..*len]).unwrap_or("?"));
                        *len = 0;
                    }
                }
            });
        }
    }

    fn run(&self) {
        if let Ok(status) = self.call(Call::Run, |sandbox, alloc, access| {
            int(sandbox.sandbox_run(alloc, access))
        }) {
            self.handle_status(status);
        }
    }

    fn isr(&self, irq: u32) {
        if let Ok(status) = self.call(Call::Isr, |sandbox, alloc, access| {
            int(sandbox.sandbox_isr(irq, alloc, access))
        }) {
            self.handle_status(status);
        }
    }

    /// Service a BT interrupt from Tock's deferred interrupt path. While the
    /// controller is stopped, the BT sources are disconnected, and any
    /// interrupt still pending is dropped.
    pub fn service_interrupt(&self, irq: u32) -> bool {
        if !BT_SOURCES.contains(&irq) {
            return false;
        }
        if self.state.get() != State::Stopped {
            instrumentation::interrupt(irq);
            self.isr(irq);
        }
        true
    }

    /// Pass the packet in `tx` to the sandbox, which copies it. If the
    /// sandbox is busy, the packet stays in `tx` until the sandbox reports
    /// `SANDBOX_TX_READY`.
    fn try_send(&self) {
        if self.tx_sent.get() || self.state.get() != State::Ready {
            return;
        }
        let len = self.tx_len.get();
        let Some(ret) = self.tx.map(|buffer| {
            let packet = &buffer[..len];
            self.call(Call::Send, |sandbox, alloc, access| {
                sandbox
                    .rt()
                    .allocate_stacked_slice_mut::<u8, _, _>(len, alloc, |buf, alloc| {
                        buf.copy_from_slice(packet, access);
                        int(sandbox.sandbox_hci_send(buf.as_ptr(), len as u32, alloc, access))
                    })
                    .and_then(|r| r)
            })
        }) else {
            return;
        };
        match ret {
            // `fail` has recorded the result.
            Err(_) => {}
            Ok(b::SANDBOX_ERR_BUSY) => {}
            Ok(r) if r < 0 => {
                self.tx_result.set(Err(ErrorCode::FAIL));
                self.deferred_call.set();
            }
            Ok(status) => {
                self.tx_sent.set(true);
                self.handle_status(status as u32);
            }
        }
    }

    /// Start the bring-up.
    fn start(&self) {
        self.state.set(State::Starting(now_us()));
        self.ran.set(true);
        if let Ok(status) = self.call(Call::Start, |sandbox, alloc, access| {
            int(sandbox.sandbox_start(alloc, access))
        }) {
            self.handle_status(status);
        }
        self.deferred_call.set();
    }

    /// Return the sandbox to the state it had before its first bring-up.
    ///
    /// This stops the radio first. It then restores the ROM's RAM from the
    /// snapshot and loads the sandbox from flash again.
    fn reload(&self) -> Result<(), &'static str> {
        stop_radio(self.intc, self.emulation);
        self.rom_ram.map(|snapshot| {
            // ### Safety
            //
            // The sandbox owns the ROM's RAM (see `ROM_RAM`). The sandbox does
            // not run during a reset. The kernel uses the ROM's routines only
            // during boot, before the snapshot was taken.
            unsafe {
                core::ptr::copy_nonoverlapping(
                    snapshot.as_ptr(),
                    ROM_RAM.0 as *mut u8,
                    snapshot.len(),
                )
            };
        });
        // ### Safety
        //
        // The sandbox does not run, and the regions are reserved for it.
        // `create` loaded from the same regions.
        unsafe { load(self.regions) }?;
        self.ran.set(false);
        self.line.map(|(_, len)| *len = 0);
        Ok(())
    }

    /// Copy the ROM's RAM into the snapshot.
    fn take_snapshot(&self) {
        self.rom_ram.map(|snapshot| {
            // ### Safety
            //
            // The sandbox has not run yet, so nothing writes to the ROM's RAM.
            unsafe {
                core::ptr::copy_nonoverlapping(
                    ROM_RAM.0 as *const u8,
                    snapshot.as_mut_ptr(),
                    snapshot.len(),
                )
            };
        });
    }
}

impl HciTransport<'static> for BleOmniglot {
    fn set_client(&self, client: &'static dyn HciTransportClient) {
        self.client.set(client);
    }

    fn reset(&self) -> Result<(), ErrorCode> {
        if matches!(self.state.get(), State::Starting(_)) {
            return Err(ErrorCode::BUSY);
        }
        if self.tx.is_some() && self.tx_result.is_none() {
            self.tx_result.set(if self.tx_sent.get() {
                Ok(())
            } else {
                Err(ErrorCode::CANCEL)
            });
        }
        self.state.set(State::Stopped);
        self.rx_pending.set(false);
        self.stopped_pending.set(false);
        if !self.started_once.get() {
            self.take_snapshot();
            self.started_once.set(true);
        } else if self.ran.get() {
            if let Err(e) = self.reload() {
                debug!("ble: reload failed: {}", e);
                self.reset_result.set(Err(ErrorCode::FAIL));
                self.deferred_call.set();
                return Ok(());
            }
        }
        self.start();
        Ok(())
    }

    fn transmit(
        &self,
        buffer: &'static mut [u8],
        len: usize,
    ) -> Result<(), (ErrorCode, &'static mut [u8])> {
        if self.state.get() != State::Ready {
            return Err((ErrorCode::OFF, buffer));
        }
        if self.tx.is_some() {
            return Err((ErrorCode::BUSY, buffer));
        }
        if len > buffer.len() || len > MAX_PACKET_LEN {
            return Err((ErrorCode::SIZE, buffer));
        }
        if !well_formed(&buffer[..len], &[PacketType::Command, PacketType::AclData]) {
            return Err((ErrorCode::INVAL, buffer));
        }
        self.tx.replace(buffer);
        self.tx_len.set(len);
        self.tx_sent.set(false);
        self.try_send();
        Ok(())
    }

    fn enable_receive(&self) {
        self.receiving.set(true);
        self.deferred_call.set();
    }

    fn disable_receive(&self) {
        self.receiving.set(false);
    }
}

impl DeferredCallClient for BleOmniglot {
    fn handle_deferred_call(&self) {
        if let State::Starting(t0) = self.state.get() {
            if now_us() - t0 > START_TIMEOUT_US {
                debug!("ble: bring-up timed out");
                stop_radio(self.intc, self.emulation);
                self.stop(State::Starting(t0));
            }
        }

        if self.tx_sent.get() && self.tx_ready.get() && self.tx_result.is_none() {
            self.tx_result.set(Ok(()));
        }
        if let Some(result) = self.tx_result.take() {
            self.tx_sent.set(false);
            if let Some(buffer) = self.tx.take() {
                self.client.map(|c| c.transmit_done(buffer, result));
            }
        }
        if let Some(result) = self.reset_result.take() {
            if result.is_ok() {
                instrumentation::started(self, self.start_us.get());
            }
            self.client.map(|c| c.reset_done(result));
        }
        if self.stopped_pending.take() {
            self.client.map(|c| c.stopped());
        }

        // The client can have reset or stopped the controller meanwhile.
        match self.state.get() {
            State::Stopped => {}
            State::Starting(_) => self.run(),
            State::Ready => {
                if self.tx_ready.get() {
                    self.try_send();
                }
                if self.receiving.get() && self.rx_pending.take() {
                    self.drain_rx();
                }
                self.run();
                instrumentation::tick(self);
            }
        }
    }

    fn register(&'static self) {
        self.deferred_call.register(self);
    }
}
