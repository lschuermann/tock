# Isolating the ESP32-C3 BLE stack with Omniglot: findings and plan

Working notes (2026-10-07, updated 2026-10-08 and 2026-10-09). Context: `ESP32C3_BLE.md` (the BLE bring-up),
`boards/esp32-c3-devkitM-1-omniglot` (Omniglot demo, commit `aba305d37`), and
`libraries/omniglot-tock` (vendored runtime).

## TL;DR

- Running the controller (ROM link layer + `libbtdm_app`/`libphy`/`libbtbb`/
  `libcoexist`) in user mode under the PMP looks feasible. Everything it
  needs was checked from PMP-restricted U-mode on the board: executing the
  mask ROM, reading ROM-owned RAM, the cycle counter `ets_delay_us` uses, and
  MMIO reads and writes. None of the code it runs uses an M-mode-only
  instruction.
- 4 PMP regions (TOR) cover it, of the 7 available on this chip.
- **The coroutine can leave the kernel, but Omniglot callbacks are not the
  tool for it.** The controller task blocks in exactly one place. Recommended:
  move the scheduling into the sandbox (two small "green threads" on foreign
  stacks), so every kernel→sandbox call is ordinary run-to-completion. In the
  steady state, no callbacks are needed at all.
- Measured load is small (~1.5% CPU with a connection), so per-call
  sandboxing overhead should be affordable.
- The radio's DMA targets (exchange memory) all lie in the controller heap,
  so they stay inside sandbox RAM if the heap moves there.
- **Since then (2026-10-08):** phase 1 works with the same timing as the
  coroutine kernel; the baseband's DMA is confined (the EM mapping
  registers are read-only for the sandbox, and slots are mapped through two
  static Omniglot callbacks that keep every window in sandbox RAM); the
  kernel stays in flash, which now runs QIO at 80 MHz, and the sandbox
  takes ~1% CPU under GATT load. See "Status".
- **2026-10-09:** rebased onto the upstream XIP flash PR (tock/tock#5260);
  kernel, sandbox and apps are now written with Tockloader's esptool
  backend (tock/tockloader#149), and apps use libtock-c's new ESP32-C3
  addresses (tock/libtock-c#599). See "Rebase onto tock/tock#5260".
  Then restructured: `dev/ble` is the base plus a few clean commits, and
  the `-bt` and `-omniglot` boards are gone.
- **2026-10-10:** cleaned up, with the BLE support folded into the stock
  `esp32-c3-devkitM-1` board, new names, the image merged into the kernel
  binary, and apps moved to `0x42100000`. See "Cleanup and new layout
  (2026-10-10)" and "Branch layout".
- **2026-10-10, later:** the driver and the sandbox moved into their own
  crate, `chips/esp32-c3-ble-omniglot`. The EM mapping registers are back
  to trap-and-emulate, with a stateless filter. The static Omniglot
  callbacks are gone. See "Refactor into a chip crate (2026-10-10)".

## Status (2026-10-10)

- **Names.** Since 2026-10-10 (see "Cleanup and new layout"), earlier
  sections use the old names: board `esp32-c3-devkitM-1-bt-og` = the stock
  `esp32-c3-devkitM-1`; `og_bt.rs`/`OgBt` = `esp_ble_controller_driver`
  (`EspBleController`); `og_bt_em.rs` = `exchange_memory.rs`; `og_bt/` =
  `esp_ble_controller_image/` (`og_bt.c` = `os_adapter.c`, `og_bt_ctx.S`
  = `green_threads.S`, `og_bt.h` = `sandbox.h`); `og_bt_*`/`OG_BT_*` =
  `sandbox_*`/`SANDBOX_*`; regions `ogflash`/`ogiram`/`ogram` =
  `ble_flash`/`ble_iram`/`ble_ram`; `make og-bt` = `make ble-image`;
  `OG_BT_FAULT_TEST` = `BLE_FAULT_TEST`; log prefix `og_bt:` = `ble:`.
  Since the refactor of 2026-10-10 (see "Refactor into a chip crate"):
  `esp_ble_controller_driver` = the crate `chips/esp32-c3-ble-omniglot`
  (`EspBleController` = `BleOmniglot`); `exchange_memory.rs` and the
  driver's `Emulation` = `emulation.rs`; `esp_ble_controller_image/` =
  `chips/esp32-c3-ble-omniglot/sandbox/` (`os_adapter.c` = `main.c`,
  `osi.c`, `controller.c` and `debug.c`; `green_threads.S` = `threads.c`;
  `emi.c` was removed); "image" = "sandbox" (`ble_image` = `_ble_load_*`,
  `make ble-image` = `make ble-sandbox`, `BLE_IMAGE=0` = `BLE_SANDBOX=0`).

- **Branch `dev/ble`** (in tock, libtock-c, tock-firmware and
  tockloader; see "Branch layout"). In tock it sits on
  `dev/ble-base`: origin/master `238630bf9` plus `--no-ff` merges of the
  upstream branches `dev/esp32-c3-scheduler-timer-fix`,
  `dev/esp32-c3-deferred-interrupt` and `dev/esp32-c3-flash-xip`
  (tock/tock#5260), and one commit that switches the stock board's
  Makefile and README to Tockloader. These replace this work's own TIMG,
  interrupt, XIP and flash-setup commits. Commit hashes in this document
  that predate 2026-10-09 refer to `esp32c3-ble-pre-rebase` (before
  2026-10-08) or `esp32c3-ble-pre-xip-rebase` (2026-10-08); those in
  "Rebase onto tock/tock#5260" to `esp32c3-ble-pre-restructure`, and the
  restructure of 2026-10-09 to `dev/ble-pre-cleanup`.

- **Phase 1 done.** Board `esp32-c3-devkitM-1-bt-og`: the controller runs in
  user mode in an Omniglot sandbox (green threads inside, every kernel call
  runs to completion). `nimble_bleprph` syncs, advertises, connects; GATT
  load test 127 round trips, median 195 ms (= coroutine baseline); pairing
  and bonded re-encryption work. ~2.1% CPU with a connection.
- **Phase 2 in progress.** Raw PMP entries plus trap-and-emulate: the sandbox
  gets all 14 PMP entries since 2026-10-09 (12 before; its flash, its RAM
  as a TOR region and the ROM's RAM separately, ROM data/code, `.iram1`,
  the BT baseband, and RWBLE as a 6-entry TOR chain around the two EM
  register banks). All other peripheral accesses are emulated against an
  allowlist (measured access profile + static scan); flash controller,
  GDMA, permission control, cache/MMU and the interrupt matrix (except 3
  value-checked words) are unreachable.
- **Baseband DMA confined** (2026-10-08, "EM window closed" below): the EM
  mapping registers are read-only for the sandbox (writes fault), and the
  sandbox's own `emi.c` replaces the controller's `emi.o` and maps and
  unmaps slots through two static Omniglot callbacks (new in
  `omniglot-tock`), which only map a slot's whole window into `ogram`.
  ~20 callbacks/s under GATT load, ~2,900 cycles each (18 us at QIO
  80 MHz); a refused mapping ends the sandbox call (see "Fault containment").
- **Flash at QIO 80 MHz** (2026-10-08, "Flash at 80 MHz" below). Since
  2026-10-09 this is upstream's `esp32_c3::flash::configure` (tock/tock#5260):
  QIO 80 if the flash's QE bit is set, DIO 80 otherwise, with no read-back
  check and no DIO 40 fallback (this branch's own `esp32-c3-flash` crate is
  gone). Against DIO 40: sandbox calls 1.8-3x faster (`og_bt_run` 130 →
  45 us), sandbox CPU 2.53 → 1.00%. The kernel stays in flash; no hot-path
  SRAM placement is needed for now.
- **Found and fixed on the way:** a nested-trap bug in the ESP32-C3 chip
  crate (`handle_interrupt` re-enabled `mstatus.MIE` inside the trap
  handler), seen as a kernel `InstructionFault` under BLE load; and
  (2026-10-08) three more ESP32-C3 bugs, see "Timing and pairing
  investigation" and "Fixes and stress tests":
  - a lost-interrupt race on the way into a process, which caused the
    pairing failures (P-256 in the app starved the controller past the
    420 ms supervision timeout); fixed by keeping deferred interrupts
    pending in hardware with `MIE` clear (`b1e7ee7ba`);
  - TIMG1 (scheduler timer) interrupt never mapped, so round-robin did not
    preempt a busy process (`af269765d`);
  - TIMG alarms auto-reloaded the counter (`e669c2bad`).
- **Verified with the fixes** (kernel in flash): pairing 20/20 under both
  schedulers with a busy loop, a 1 ms ticker and a console/RNG app running
  next to NimBLE; GATT median 195 ms over 120 s; 50/50 connect/disconnect
  cycles; 20/20 bonded reconnects.
- **Verified after the EM and flash changes** (QIO 80): GATT 25 s 127 and
  120 s 614 round trips (median 195 ms), pairing 5/5, 5/5 boots in QIO.
  Full interleaved stress set (stress apps, both schedulers, against the
  pre-change kernel): pairing 80/80, all GATT, connect and bonded-reconnect
  runs clean; see "Stress set after the EM and flash changes".
- **Open issues:** (1) per-call cost is still dominated by flash-cache
  misses, now ~3x cheaper with QIO 80; the worst offender is the PMP code
  (~150 us to rewrite 7 regions cold at DIO 40); a faster PMP write
  (`39c6c2b64`) and the kernel hot path in SRAM are drafted on
  `esp32c3-ble-hotpath`, not merged, and not needed so far (see "If the
  kernel in flash is ever too slow"); (2) bring-up 115 ms instead of 37 ms
  because PHY calibration is emulated; (3) a sandbox fault stops the
  controller for good (contained since "Fault containment", but there is no
  recovery yet); (4) other DMA-capable blocks beyond GDMA and the Wi-Fi MAC
  not reviewed (§5, "Still open for DMA").

**What's left** (2026-10-08, in rough priority order):

1. ~~Fault containment (step 9)~~ **Done**, see "Fault containment".
2. Recovery (step 10): the radio is already stopped on a fault (BT
   sources unmapped, EM windows closed, modem in reset); remaining: reset
   sandbox RAM, re-run `omniglot_init` and the bring-up, and keep the host
   alive through the downtime (NimBLE asserts on a failed ACL send, see
   "Fault containment"). Plus a call watchdog.
3. Tail latency: measure worst-case BT-interrupt-to-handler latency under
   a max-throughput load (shortest connection interval, DLE, MTU 256) and
   scanning bursts; explain `ticker`'s 24-63 ms worst gaps.
4. The rest of step 10's stress list: hours-long runs, HCI reset
   mid-connection, peer restarts, fault injection, long idle (timer wraps).
5. Remaining DMA review (§5 "Still open for DMA"): confirm no other
   DMA-capable master is reachable through the MMIO allowlist; optionally
   turn the allowlist into PMP entries (step 8, §6) — no entries left
   since the sandbox RAM moved below the kernel's `iram` (2026-10-09), so
   this would need entries freed elsewhere first.
6. Optional speed-ups, only if 3 shows a need: straight-line PMP writes
   (`39c6c2b64`), kernel hot path in SRAM (`a2e5ba1a6`), and bring-up
   (115 ms vs 37 ms, from emulated PHY calibration).
7. Housekeeping: move `omniglot-tock` out of `libraries/` (it is unsafe
   code). (The ESP32-C3 chip fixes and the QE fix are upstream PRs now:
   the TIMG, deferred-interrupt and XIP flash branches merged into the
   base.)
8. ~~Restructure the branch~~ **Done** (2026-10-09/10), see "Branch
   layout" and "Cleanup and new layout (2026-10-10)". Rerun the stress set
   (`whileone` needs a new address, see "Test tooling") and fault injection
   on `dev/ble`.

## What was measured / verified

### On hardware, from U-mode under the PMP (Omniglot demo board, uncommitted probes in `og_demo/`)

| Check | Result |
|---|---|
| Read `upccr` (CSR `0x802`, Espressif user-mode cycle counter) | works (2031 cycles over a busy loop) |
| Call ROM `ets_delay_us` (0x40047e8e) | works; reads `s_ticks_per_us` at `0x3FCDF6DC` (ROM RAM) |
| Same call, timing | 400012 cycles for "20000 us" = 2.5 ms real: ROM still assumes 20 ticks/us. The sandbox must call `ets_update_cpu_frequency(160)`, which **writes** ROM RAM → ROM RAM must be RW |
| MMIO read (eFuse MAC) and write+read (SYSTIMER latch) | works, values match the kernel's |
| Extra PMP regions through `TockRv32iCRt::new(.., addl_mpu_regions, ..)` | works (ROM RX, ROM RAM R, MMIO RW, plus the library's flash and RAM = 5 regions) |

**Interrupts while sandboxed: verified (step 1, commit `1ebce6353`).** From
deferred calls in the main loop, 1000 sandboxed 2 ms busy loops
(`og_probe_irq`: a distinct pattern in every register except sp/x30/x31)
ran while UART output drained and a TIMG0 alarm fired every 300 us. All
1000 calls returned with interrupts recorded as pending
(`has_pending_interrupts()`), 0 had a changed register (incl. sp, gp, tp),
and the alarm fired exactly once per call: masked during the call,
serviced by the main loop afterwards. Also from that test:

| | |
|---|---|
| `ets_delay_us(20000)` before `ets_update_cpu_frequency(160)` | 400012 upccr cycles, 2524 us (SYSTIMER) |
| same, after (ROM RAM RW) | 3200013 upccr cycles, **20002 us** |
| empty sandboxed call (`og_demo_add`, PMP reconfig + mret + ecall trap) | **~1.97 us** (1000 calls in 1967 us) |

**Nested-trap bug (found, fixed in `785e8ba91`).** The chip's
`handle_interrupt` set `mstatus.MIE` before returning, still inside the trap
handler. A second interrupt in that window nested, overwrote `mepc`, and
its `mret` reset `MPP` to U, so the outer `mret` resumed kernel code in
U-mode: `fatal exception: InstructionFault: 0x420001ee` (in `_start_trap`'s
restore sequence), seen once on the `-bt-og` board under GATT load. A
pre-existing Tock bug on the kernel trap path (the `-bt` kernel has the same
window). The fix leaves `mstatus` alone; `mret` restores `MIE` from `MPIE`.

Side note: the C3 core implements neither `mie` nor `mip`; `csrr mip` is an
illegal instruction, even in M-mode.

### Static analysis (blobs and ROM)

ROM ELF: Espressif `esp-rom-elfs` 20260528, `esp32c3_rev101_rom.elf` (ECO7).
It matches the chip; sampled words were compared.

- **Archives** (`libbtdm_app`, `libbtbb`, `libphy`, `libcoexist`): no CSR
  accesses, `ecall` or `fence.i`, and no `gp`/`tp`-relative code. The only CSR
  uses, `csrs/csrc mstatus`, are in `phy_ftm_delay_cal` (Wi-Fi FTM), which BLE
  doesn't use. The objects carry `R_RISCV_RELAX`, so link the sandbox with
  `--no-relax` to keep the linker from introducing `gp`-relative accesses.
- **ROM BT code** (`.bt_text`): no CSR accesses and no `gp`/`tp` use.
  `ebreak` appears only in the `r_ke_state_set/get` assert paths; it would show
  up as a sandbox fault.
- **ROM helpers the stack calls directly**:
  - `ets_delay_us` uses `upccr`, which works in U-mode.
  - `ets_printf`: no CSRs. The blobs call it 146 times; the board overrides it
    anyway.
  - `ets_intr_lock/unlock` write `mstatus` (that would trap), but their only
    ROM caller is `gpio_pin_wakeup_enable`, which is not on the BT path.
- The ROM's BT state lives in `0x3FCDF098..0x3FCE0000`: `.data_btdm`,
  `.bss_btdm`, `.data/.bss.interface.bluetooth`, the `r_*_funcs_p` tables,
  `.data_phyrom`, and `.data_ets_delay`. ROM `.rodata` is read through the
  DROM alias `0x3FF19F70..` (its load address is the IROM alias `0x40059F70`).
- **The task blocks in exactly one place.** `btdm_controller_task` is:
  `loop { semphr_take(sem, FOREVER); while queue_recv(g_rw_schd_queue, &m, 0)
  { dispatch(m); btdm_rw_run(); } btdm_rw_run(); }`. The queue receive does
  not block (timeout 0).

### Runtime behaviour of the current kernel (`-bt` board, temporary instrumentation)

| | Advertising (30-60 ms interval) | Connected, GATT write+read loop |
|---|---|---|
| BT ISR invocations | ~30/s, avg 36 us, max 67 us | ~72/s, avg ~30 us, max 69 us |
| Task resumes | ~30/s, avg ~10 us | ~82/s, avg ~150 us (includes HCI delivery to the process) |
| Task stack high-water mark | 924 B | 1020 B |
| Blocking waits with a timeout | 1 (boot) | 1 (boot) |

Blocking waits seen (wait kind, timeout, context):

- `semphr_take`, 10 s, **kernel context**: `btdm_controller_init` waits for
  the task's init.
- `semphr_take`, FOREVER, **kernel context**: `btdm_controller_enable`.
- `semphr_take`, FOREVER, **task**: the loop head (every other wait).
- No `queue_send` or `queue_recv` ever blocked.

GATT test from the Pi (bleak): 127 write+read round trips in 25 s, median
195 ms, max 561 ms, no errors.

**Exchange memory (DMA):** after enable, 40 EM base registers
(`0x6003_1204..`) are non-zero. All 17 with an address field point into the
controller heap arena; the other 23 have an empty address field. So the
baseband only DMAs into memory the controller `malloc`ed.

## Design

### 1. Sandbox image

Link the four archives, the ROM linker scripts, and a C glue layer into one
standalone Omniglot binary (like `og_demo`, no TBF). Layout:

| What | Where | PMP |
|---|---|---|
| `.text`/`.rodata` of the blobs and glue | flash, e.g. `0x42300000` (shrink `prog`) | RX |
| `.iram1` (~27 KiB) | IRAM `0x40380000..` (DRAM alias `0x3FC80000..`) | X (via the ROM region below) |
| `.data`/`.dram1`/`.bss`, heap (32 KiB), green-thread stacks, Omniglot stack, HCI/log rings | SRAM **directly below `0x3FCDF000`** | RW, merged with ROM RAM |
| ROM RAM | `0x3FCDF000..0x3FCE0000` | RW (same region) |
| Mask ROM code + DROM data + IRAM | one region `0x3FF00000..0x40388000` (includes unmapped space) | RX |
| Peripherals | `0x60000000..0x600D1000` | RW |

That's 4 TOR regions (8 of the 14 user PMP entries). There's room left to
split them up later, and no need for NA4/NAPOT for now.

Rough sandbox RAM: heap 32 KiB (24.7 KiB peak), task stack 2-3 KiB,
init/main stack 4 KiB, Omniglot stack 4 KiB, data/bss under 2 KiB, plus rings,
about 48 KiB in all. Traps now land on the kernel stack (Omniglot swaps
stacks), so the green-thread stacks no longer need room for trap frames. The
kernel loses its 32 KiB heap and 6 KiB task stack, so net RAM use is about
unchanged.

**Loading `.iram1`:** `omniglot_init` only copies `.data`. Have the kernel
copy `.iram1` through the DRAM alias before creating the runtime (extend the
binary header with an "iram" segment, or use board-specific symbols). The
sandbox never gets write access to its own code.

**Invariant:** no kernel code may be executable inside these regions. Callback
detection and the return springboard rely on kernel code faulting on
instruction fetch. So nothing of the kernel stays in IRAM; today `bt.rs` puts
`service_interrupt` and OSI functions in `.iram1`, and those move out.

### 2. OS adapter inside the sandbox

Implement the `btdm_osi_funcs` table in C inside the sandbox instead of as
kernel callbacks: queues, semaphores, mutexes, `malloc`/`free`, timing,
eFuse MAC, rand, sleep/coex stubs, `interrupt_alloc` (records the handler and
writes the interrupt matrix itself), `ets_printf`/`phy_printf` into a log
ring, and `assert` (record, then fault on purpose).

**Critical sections are no-ops.** In the sandbox, `mstatus` is inaccessible
and irrelevant: in U-mode, M-mode interrupts are always taken. They are also
unnecessary: an interrupt taken while foreign code runs is masked and recorded
by the runtime, and the foreign ISR only runs later as a *separate*
invocation. So foreign code never preempts foreign code. This is the same
reasoning that already holds for today's deferred interrupts.

### 3. Replacing the coroutine

**Why callbacks don't do it.** In `omniglot-tock`, a callback runs
synchronously inside the trap handler and has to return to its foreign caller.
Parking a foreign call halfway and returning to the kernel main loop would
require a suspendable invocation. That conflicts with how Omniglot scopes
`AllocScope`/`AccessScope` to one invocation, and with the runtime's single
foreign stack.

**Recommended: green threads in the sandbox.** The glue keeps two contexts on
stacks in foreign RAM: *main* (bring-up and API calls) and *task*. A blocking
wait with nothing to run switches context inside the sandbox. The context
switch is about 20 lines of asm, the same as `task.rs`, but in untrusted
memory. When no context can run, the entry point returns a status to the
kernel. To the kernel and to Omniglot, every call runs to completion:

| fntab entry | Purpose |
|---|---|
| `og_bt_start(cfg)` | Bring-up; may return `WAITING` (main suspended) |
| `og_bt_run()` | Run runnable contexts until all block; returns status bits (`TX_DONE`, `RX_PENDING`, `LOG_PENDING`, `WAITING_DEADLINE`, …) |
| `og_bt_isr(line)` | Run the handler registered for that line (on the Omniglot stack) |
| `og_bt_hci_send(ptr, len)` | Copy into the sandbox's static TX buffer (the controller reads it asynchronously) and post to the task |
| `og_bt_hci_recv(buf, cap)` | Drain one packet from the RX ring filled by `notify_host_recv` |
| `og_bt_log(buf, cap)` | Drain the log ring |

To call registered ISRs, pass the function pointer to a fixed trampoline
(`og_bt_isr`). That avoids runtime changes for dynamic function pointers.

The kernel side becomes a capsule-like driver. Its deferred call does `run()`
and drains RX/log. Its interrupt service does `isr(line)` and then schedules
the deferred call. The HCI syscall does `hci_send`. Timed waits (only the boot
one so far) report their deadline through the status, and the kernel re-arms
via deferred call or alarm. Bring-up is the same loop as today's
kernel-context `wait()`: service BT interrupts, `run()`, repeat.

**Alternatives considered:**

- **B. Suspendable invocations in the runtime.** On a designated callback,
  save the full foreign register file and return from `generic_invoke` with a
  "suspended" status, then later resume with a return value. That would be
  closest to a Tock process. It means larger asm changes, plus rethinking the
  Omniglot scope types. Not worth it for one blocking point.
- **C. Restart the task function** after each block, since all its loop state
  is reloaded from globals at the loop head. It also clears one static word
  (`.LANCHOR7`) on entry, so it depends on blob internals. Fragile.

### 4. Omniglot integration details

- **A `'static` runtime:** `OGLifetimeBranding` is closure-scoped. Define a
  board-local singleton `OGID` (`unsafe impl`, constructed once). `runtime_id`
  needs `AtomicU64`, which RV32IMC lacks.
- Store `AllocScope`/`AccessScope` in the driver (`MapCell`) and use them per
  call. Copy HCI packets in and out through Omniglot slices
  (`allocate_stacked_slice_mut`, `validate`).
- Bindings come from bindgen over a small `og_bt.h`, like `og_demo`.

### 5. Binding the radio's DMA (EM mapping registers)

The baseband reaches SRAM only through the "EMI" remapping. The 16-bit
exchange-memory (EM) offset space is split into slots, and each slot's
register holds the SRAM address its window maps to.

**Registers** (BLE block, `0x6003_1xxx`):

| Address | Contents |
|---|---|
| `0x6003_1204 + 4*i`, i = 0..47 | slot base registers |
| `0x6003_12E0 + 4*(i-48)`, i = 48..55 | slot base registers |
| `0x6003_12C4` / `0x6003_12C8` / `0x6003_1300` | enable bitmaps for slots 0-31 / 32-47 / 48-55 |

Register format: bits `[17:0]` = SRAM word address relative to
`0x3FC0_0000` (so any DRAM address in that 1 MiB, kernel RAM included); bits
`[31:18]` = the slot's EM offset / 4. The ROM's LUT `em_base_reg_lut`
(`0x3FF1F518`, 51 entries, one per 1 KiB EM granule) maps offsets to slots.
Only slots 0-38 are used, each covering 1 or 2 KiB.

**Hot neighbours sit *between* the banks:** `0x6003_12CC..0x6003_12DC`
(written by `r_rwble_isr`, `r_lld_update_rxbuf*` per packet) and
`0x6003_1304+` (filters, timers at `0x6003_138C`, CCA at `0x6003_1360/64`).
These must stay RW.

**Measured:** after enable, 40 base registers are non-zero. All 17 with an
address field point into the controller heap; the rest have an empty address
field.

**The mapping is not static**, so it can't simply be set once.
`r_ble_util_buf_{acl_tx,adv_tx}_alloc` and `…acl_tx_free` (ROM) remap slots at
runtime, for each ACL/adv TX buffer. Each mapping does: `malloc_internal(size)`,
RMW of the slot's low 18 bits, RMW to set the enable bit.

**All writers of the window** (static scan of ROM and archives; the other
`0x600312xx` users only touch the neighbours above):

| Writer | Defined in | Reached via |
|---|---|---|
| `r_emi_alloc_em_mapping_by_offset` | `emi.o` | `r_plf_funcs` +156 (ROM callers) |
| `r_emi_free_em_mapping_by_offset` | `emi.o` | +180 |
| `r_emi_overwrite_em_mapping_by_offset` | `emi.o` | +196 |
| `r_emi_em_base_init` / `_deinit` / `r_emi_init` / `r_emi_deinit` | `emi.o` | +168 / +164 / +192 / +160 |
| `emi_reset_em_mapping_by_offset` | `emi.o` | **direct call** from `ble_util_buf_reset` (`ble_util_buf.o`) on HCI Reset |
| ROM's own `r_emi_*` copies | ROM | no direct ROM callers of alloc/free/overwrite found; ROM dispatches through `r_plf_funcs_p` |

Every reference to the `emi.o` writers comes from *other* objects, so the
sandbox can define all of `emi.o`'s globals itself. Then `emi.o` is never
pulled from the archive (no `--wrap` and no runtime table patching needed),
and the table and the direct call both resolve to our versions.

**Design: paravirtualize, deny writes in the PMP.** *Implemented
2026-10-08; see "EM window closed" for what was built. Differences from
the design below: the writer table above was wrong (the ROM's copies did
the runtime remapping, through a linker-script override); the sandbox
reads the registers directly instead of keeping a shadow copy; there are
only two callbacks, `em_map(slot, addr)` and `em_unmap(slot, em_offset)`,
with no bulk versions (init and reset loop in the sandbox); and the PMP
uses option B, as option A does not work on the C3.*

- Sandbox `emi.c`: same API. Lookups (offset → slot, address translation) run
  in the sandbox, reading the LUT and the registers (reads stay allowed).
  `malloc` happens in the sandbox. Each register write becomes a kernel
  callback:
  - `em_map(slot, addr)`: the kernel checks that slot ≤ 38 and that
    `[addr, addr + slot_size)` lies inside the sandbox's own RAM (not ROM RAM,
    not the `.iram1` code's DRAM alias). It then does the RMW (keeping bits
    `[31:18]`) and sets the enable bit.
  - `em_unmap(slot)`: clears the enable bit.
  - `em_reset()` / `em_deinit()`: bulk versions.

  Slot sizes come from the ROM LUT, or are hard-coded from it in the kernel.
- PMP: read-only over `[0x6003_1204, 0x6003_12CC)` and
  `[0x6003_12E0, 0x6003_1304)`, RW for the rest of the MMIO range. A missed
  writer then faults, which is fail-stop, never a silent bypass.
- **Runtime extension needed: "static" callbacks.** The stubs' targets must be
  stable addresses that fault into `callback_handler` across invocations.
  Today callbacks are scoped to one invocation through the alloc chain. Add a
  board-registered table of springboard slots (kernel memory, not executable
  by the sandbox) that `callback_handler` checks first. The callbacks are a
  few register writes, so running them in the trap handler is fine.
- Rate: about one or two callbacks per ACL/adv TX buffer, plus a burst at
  init and reset. Negligible.
- **PMP budget** (to be settled when the PMP code is refactored for raw
  access; notes only): Tock's `PMPUserMPU` rejects overlapping regions
  (`pmp.rs:1182`).
  - Splitting the MMIO range instead would need 5 regions = 10 entries
    (16 in total > 14).
  - Option A: allow *prioritised* overlap. The read-only carve-outs get lower
    PMP indices than the MMIO RW region: flash 2 + ROM/IRAM 2 + RAM 2 +
    2 carve-outs 4 + MMIO 2 = 12 entries. **Ruled out on hardware:** the
    C3 does not honour PMP priority between overlapping entries (0 faults
    in 82 writes).
  - Option B: TOR chaining (adjacent regions share boundary entries). The
    5-piece MMIO run costs 6 entries, 12 in total. **Built.**
  - NAPOT doesn't help here: the windows aren't power-of-two aligned, and
    covering them costs ~6 entries.
- Alternative: **trap-and-emulate** with the same PMP layout. Decode
  `sw`/`c.sw` at `mepc`, validate, perform the store, skip the instruction.
  This needs no knowledge of the call graph, but needs a full-register-save
  fault path in the runtime and an instruction decoder. Paravirtualization is
  preferred, because the writer set is small and we control the link.
  (Both were built and measured on 2026-10-08; callbacks won on cost and on
  the trusted interface. See "EM window closed".)
- Teardown: before reclaiming sandbox RAM, the kernel must clear all enable
  bitmaps (and stop the radio).

**Still open for DMA:** other DMA-capable masters reachable through MMIO
(GDMA and its peripherals: SPI2, I2S, UHCI, AES/SHA, ADC; the Wi-Fi MAC). The
MMIO RW region must exclude them. Derive the allowlist of peripheral pages
the stack touches (static scan plus a fault-driven run), and check whether
the C3's PMS/permission-control DMA constraints can additionally fence GDMA in
hardware.

### 6. MMIO the stack touches, and PMP entry budget

Static scan: all four archives, plus the 1130 ROM functions reachable through
direct calls from `.bt_text` and from the archives' ROM imports. It collects
`lui`-based constants and resolves negative offsets. `lui`+`addi -1` mask
artifacts are dropped. Accesses through pointers loaded from tables are
**not** covered, so this is a lower bound; confirm it with a fault-driven run.

| Block (esp-idf `reg_base.h`) | Range used | Notes |
|---|---|---|
| GPIO `0x60004000` | `+0x08` | `btdm_gpio_set_level` (debug pin, probably unused) |
| FE2/FE `0x60005000/0x60006000` | FE `+0x000..0x1E8` | RF front end (PHY) |
| RTC_CNTL `0x60008000`, eFuse `0x60008800` | `0x60008158`, eFuse `+0x000..0x044` | PHY; MAC read |
| RTC_I2C `0x6000E000` | `+0x000..0x1B4` | analog I2C master (PHY register access) |
| BT BB `0x60011000` (not in reg_base.h) | `+0x080..0x8FC` | ISR (`r_bt_bb_isr_hack`), intr mask, recorrect |
| `0x6001C000` / NRX `0x6001CC00` / BB `0x6001D000` | `0x6001C050..0x6001D0A8` | AGC/RX (PHY) |
| SYSCON `0x60026000` | `+0xD4` (+ `0xB0` RNG and `0x14/0x18` modem clocks for the glue) | |
| RWBT `0x60030000`, RWBLE `0x60031000` | `0x60030920`, `0x60031000..0x600316D4` | minus the EM window (§5) |
| Wi-Fi MAC (probably) `0x60033000` | only `0x60033C18`, `0x60033C68`, `0x60033C6C` | coex bits (`bt_bb_v2_rx_set`, `hal_*_extern_coex`, `mac_debug`). **The MAC has its own DMA: do not open the block** |
| coex `0x60035000` | `+0x000..0x120` | `coex_pti_v2`, coex HW timers |
| APB_SARADC `0x60040000` | `+0x07C..0x218` | temperature sensor (PHY). ADC DMA needs GDMA, which stays closed |
| `0x60042000` | `+0x0..0xC` | sleep/wakeup timer |
| SYSTEM `0x600C0000` | `+0x20`, `+0x24` | low-power clock select/div (init) |
| glue: SYSTIMER `0x60023000`, INTMATRIX `0x600C2000` | | `get_time_us`, `interrupt_alloc` (make the latter a kernel callback) |

`btdm_ble_power_down_dma_copy` (sleep retention) goes through the OSI
`ets_backup_dma_copy`, which the sandbox stubs out. It is not a separate DMA
path.

**Not just DMA:** a few non-DMA blocks are equally dangerous and must stay
closed:

- SPI0/SPI1 `0x60002000-0x60003FFF`: flash erase/program would let the
  sandbox overwrite the kernel image.
- EXTMEM `0x600C4000` and the cache MMU table: remapping flash would make the
  kernel execute sandbox-chosen code.
- SENSITIVE `0x600C1000` (PMS) and WORLD_CNTL `0x600D0000`.
- INTMATRIX `0x600C2000`: control over kernel interrupts.

**PMP entries** (16 in total, 2 reserved for the trap-vector quirk → 14
usable). Every layout below is **non-overlapping**, so it doesn't depend on
the C3's broken PMP priority handling.

- *Least privilege* (each range above separately, NAPOT where aligned,
  TOR otherwise): ~17 entries for MMIO, plus 4-5 for code/RAM, ~22 in total.
  **Doesn't fit.**
- *DMA-safe, coarse* (one TOR chain; each boundary costs 1 entry):

  ```
  0x60004000                 base (excludes UART0, SPI1, SPI0)
  0x60031204  RW   GPIO … RWBT … RWBLE (below EM bank 1)
  0x600312CC  R    EM bank 1 + enable bitmaps
  0x600312E0  RW   hot RWBLE regs (ISR, RX buffer)
  0x60031304  R    EM bank 2 + bitmap
  0x60031700  RW   rest of RWBLE
  0x60035000  --   0x60031700..0x60034FFF, incl. the Wi-Fi MAC
  0x6003F000  RW   coex, AES/SHA/RSA/DS/HMAC (no DMA without GDMA)
  0x60040000  --   GDMA
  0x600C1000  RW   SARADC, USB-JTAG, 0x60042000, SYSTEM
                   (SENSITIVE, INTMATRIX, EXTMEM, … closed)
  ```

  That is 10 entries for MMIO. Code and RAM add 4: sandbox flash NAPOT,
  `[0x3FF00000, 0x40388000)` TOR RX (DROM, ROM, IRAM; 2 entries), and RAM
  64 KiB NAPOT `0x3FCD0000`. **14 entries: an exact fit.** It requires:
  - the 3 Wi-Fi MAC coex registers as a kernel callback (or
    trap-and-emulate);
  - `interrupt_alloc` as a callback;
  - the two EM-bank entries read-only instead of no-access (same entry
    count; the sandbox `emi.c` reads the registers, as built on
    2026-10-08);
  - `SYSTEM` either left in (as above) or done by the kernel at init.
- More slack: per-phase PMP configs. Calibration/init touches FE, RTC_I2C,
  SARADC and RTC_CNTL. If those aren't on the runtime path, a runtime-only
  config can drop them. Omniglot already reprograms the PMP on every call, so
  switching configs per call costs nothing extra. Statically, the runtime set
  can't be separated from the init set: ROM dispatches channel and RF setup
  through RAM function-pointer tables. Measure it instead.
- **Measuring the real set:** run the stack in the sandbox with MMIO closed
  except for known blocks, log each faulting address (and `mepc`), add it,
  repeat. Do this separately for init and for steady state (advertising, a
  connection, scanning).

## Risks and mitigations

| Risk | Status / mitigation |
|---|---|
| U-mode can't run ROM, read cycles or reach MMIO | **Retired** (hardware probes) |
| Privileged instructions in the stack | **Retired** (static scan); FTM calibration and the GPIO wakeup lock are off-path |
| `gp`/`tp` assumptions | **Retired** for ROM and blobs; link with `--no-relax` |
| Interrupts taken while sandboxed | **Retired** (step 1: 1000 calls, 2 sources, no corruption). The nested-trap bug in `handle_interrupt` is fixed (`785e8ba91`) |
| Timing: per-call PMP reconfiguration (~12 CSR writes) plus trap/mret | Measured: dominated by flash-cache misses, not the CSR writes. 2.5% sandbox CPU at DIO 40, **1.0% at QIO 80**; GATT median unchanged (195 ms) either way |
| ISR latency | Unchanged design (deferred), but radio ISRs now wait for the current sandbox call to return. Calls are short (≤ ~150 us avg, PHY calibration only at boot) |
| A fault in the sandbox panics the kernel (`encode_return` panics) | Acceptable at first; later return an error and mark the stack dead |
| DMA escapes the PMP | BLE baseband: **closed** (EM registers read-only, mapping through checked static callbacks; refused mappings and direct stores verified on hardware). Other masters (GDMA etc.): closed by the MMIO allowlist (emulation) |
| The sandbox can corrupt other ROM state in `0x3FCDF000..0x3FCE0000` (e.g. the spi-flash/cache ROM tables at `0x3FCDFFxx`) | Only matters if the kernel uses those ROM routines after boot (today: once, at boot). Accept for now |
| PMP budget (14 user entries) | 12 used (RWBLE's TOR chain around the EM banks takes 6); overlapping entries can't save any on the C3 |
| Boot-time flash reconfiguration hangs or corrupts the flash | Each mode is checked on SPI1 before SPI0 switches; the switch runs from SRAM; QE is never written by the kernel, and the ROM call that would clear it gets `a1 = 0` |
| Blob/ROM version match (ECO7 chip, ECO3 scripts) | Unchanged from today; works |
| Unresolved symbols when linking standalone | Trial link: only `phy_printf` (provided by the glue). Expect `ets_printf` override and libgcc via ROM scripts |

## Plan

**Phase 1 comes first and deliberately ignores the PMP granularity work
(§5, §6).** Its only goal is to answer: does the stack work at all from U-mode
under Omniglot, and does the timing hold? Use the coarse 4-region layout
from §1:

- sandbox flash RX;
- `[0x3FF00000, 0x40388000)` RX (DROM, ROM, IRAM);
- sandbox RAM through `0x3FCE0000` RW (includes ROM RAM);
- **all** of MMIO `[0x60000000, 0x600D1000)` RW.

With that layout, phase 1 needs **no Omniglot callbacks and no runtime asm
changes**:

- the sandbox writes the EM registers and the interrupt matrix directly;
- the blob's `emi.o` is linked unchanged;
- there's no MMIO allowlist.

### Phase 1: make it run

1. **Interrupt-while-sandboxed test** (open de-risk item). In the
   `esp32-c3-devkitM-1-omniglot` board, make the runtime `'static` with a
   board-local singleton `OGID`. From a deferred call in the main loop, run a
   long sandboxed `ets_delay_us` while debug output drains (UART interrupts).
   Check that the interrupts get masked, recorded, and serviced afterwards,
   with no corruption. Also run `ets_update_cpu_frequency(160)` with ROM RAM
   RW, and check that `ets_delay_us` is then accurate.
2. **Standalone sandbox link**: the four archives, the ROM linker scripts
   (same set as `boards/esp32-c3-devkitM-1-bt/build.rs`), and stub glue.
   - Bind `memcpy/memset/memmove/memcmp/strlen/strncmp` to their ROM addresses
     (`0x40058bdc`, `0x40058d00`, `0x40058c92`, `0x40058b94`, `0x400592ae`,
     `0x400593c8`). Today Rust's compiler-builtins provide them.
   - Soft-float and `__divdi3` already come from `esp32c3.rom.libgcc.ld`.
   - `phy_printf` and `ets_printf` come from the glue.
   - Link flags: `--no-relax`, `--gc-sections`, `-march=rv32imc_zicsr`.
   - Check sizes, unresolved symbols, and placement: `.iram1` at
     `0x40380000`; `.data`/`.dram1`/`.bss` in sandbox RAM.
   - Add the kernel-side `.iram1` copy (via the DRAM alias) before
     `TockRv32iCRt::new`.
3. **Glue v1** (C, in the sandbox): OSI table, heap, two green threads
   (main and task), log ring, HCI RX ring, the entry points from §3.
   - Bring-up (modem power/clocks, `ets_update_cpu_frequency`, LP clock,
     `btdm_controller_init`, PHY calibration, `enable`, `coex_pti_v2`,
     VHCI registration) moves from `bt.rs` into the glue, with the same
     values.
   - The kernel drives start / run / isr until bring-up completes.
4. **Board** `esp32-c3-devkitM-1-bt-og`, copied from `-bt`:
   - drop `bt.rs`/`task.rs`/`heap.rs`/`printf.rs`;
   - add a sandbox driver (deferred call → `og_bt_run`; BT interrupt lines →
     `og_bt_isr`; drain HCI RX and the log after every call);
   - keep `hci_driver.rs` with the `Bt` interface reimplemented on the
     sandbox.
   - Run `nimble_bleprph`: sync, advertise, connect from the Pi, GATT load
     test (`tools/gatt-load.py`), pairing.
5. **Measure**: the same numbers as above (ISR/run calls per second, µs per
   call), plus the Omniglot overhead (PMP reconfiguration + trap), and the
   worst case of radio interrupt latency while a sandbox call is running.
   Compare with the coroutine kernel.

### Phase 1 results (2026-10-07)

Board `esp32-c3-devkitM-1-bt-og` (commits `f5c3bf152`..`e8f169758`, plus
the chip fix `785e8ba91`). `make -C boards/esp32-c3-devkitM-1-bt-og og-bt
flash` writes the kernel at 0x0 and the sandbox at 0x300000.

- **Works on air.** `nimble_bleprph` syncs, advertises, accepts the Pi's
  connection (MTU 256). `tools/gatt-load.py 25`: 126-128 round trips, median
  195 ms, max 354-566 ms, no errors (6 runs). Baseline: 127, 195 ms, 561 ms.
  Pairing not retested.
- **Sandbox image** (`og_bt/`): 111 KiB text + 11 KiB rodata in flash
  (0x42300000, 256 KiB reserved), `.iram` 26.4 KiB, RAM ~53 of 60 KiB
  (4 KiB Omniglot stack, 1.4 KiB data, 47 KiB bss: 32 KiB heap, 2x4 KiB
  green-thread stacks, 1.9 KiB PHY cal, 2+2 KiB rings). Unresolved at
  link: only `memcpy/memset/memcmp` (bound to ROM) and `phy_printf`
  (glue). `ets_vprintf` has no ROM jump-table entry; it is bound to its ECO7
  address `0x40049e2e` and used to format the archives' printfs into the
  log ring.
- **Bring-up** inside the sandbox: 36 ms; heap peak 24.7 KiB; stack
  high-water: task 416 B, main 1360 B. Only waits seen: as before (the
  boot-time semaphore).
- **Per-call timings** (kernel-side, `mpccr` cycles at 160 MHz, which
  matches SYSTIMER), 10 s windows:

  | | Advertising | Connected, GATT write+read loop |
  |---|---|---|
  | `og_bt_isr` | 30/s, avg 40-43 us, max ~150 us | 71/s, avg 76-80 us, max ~250 us |
  | `og_bt_run` | 30/s, avg 14 us, max ~150 us | 71/s, avg 90-97 us, max ~300 us |
  | `og_bt_hci_send` | - | 10/s, avg ~670 us, max ~690 us |
  | `og_bt_hci_recv` | - | 40/s, avg 55 us, max ~130 us |
  | total | 61/s, 0.17% CPU | 194/s, 2.1% CPU |

  Empty call (`og_bt_stat`, back-to-back at boot): 358 cycles = 2.24 us.
  Longest call (= worst-case extra latency for a radio interrupt that
  arrives while the sandbox runs): ~300 us steady state, 36 ms once at
  bring-up (PHY calibration).
- **Where the time goes** (temporary in-sandbox `upccr` profiling): the
  registered BT handler itself takes 29-30 us (same as the coroutine
  kernel), and all of `og_bt_isr` inside the sandbox 34-37 us. The other
  ~40 us per call are the kernel<->sandbox transition under load, vs 2.2 us
  hot at boot. Not yet explained: suspect flash-cache misses in the
  runtime's kernel code and the sandbox glue (16 KiB cache shared with the
  app) and/or the full PMP reprogramming after a process ran. For
  `og_bt_hci_send`, the green threads run ~380 us of the ~670 us (the
  controller processing the packet); the remaining ~300 us are not yet
  accounted for. Worth a look before phase 2; latency is unaffected.

### Phase 2 progress: scoped PMP (2026-10-07)

Commits `b35fb0509`..`d38b6729d`.

- **Raw PMP:** the user-config record (`last_configured_for`) moved from
  `PMPUserMPU` into the `TORUserPMP` implementations (`configured_for()`);
  `SimplePMP::configure_raw()` writes `UserPMPEntry`s (OFF/TOR/NAPOT, never
  locked) and clears the record, so the next process switch rewrites the
  PMP. The runtime is generic over `OGProtection` (`MpuProtection` keeps the
  old behaviour for the demo board). The `-bt-og` board skips the write
  while the PMP still holds its layout. Rewriting all 14 entries costs
  ~4 us per call on this chip (empty call 6.5 us vs 2.5 us cached).
- **Trap-and-emulate** in the runtime for load/store access faults
  (decode at `mepc`, board filter on address/width/stored value). One
  emulated access costs ~2.4 us. The kernel can read ROM0 instructions
  (`0x4000_0000..`) as data.
- **Measured access profile** (everything emulated and counted per page):

  | Page | When | Rate |
  |---|---|---|
  | RWBLE `0x60031000` | connected | ~4000/s |
  | BT BB `0x60011000` | per radio IRQ | ~60/s |
  | SYSCON `0x60026000` | RNG | 3-17/s |
  | RTC_I2C `0x6000E000`, FE `0x60006000` | PHY calibration at boot (~17k each), then ~6/s | |
  | SARADC `0x60040000` | temperature | ~1/s |
  | AGC `0x6001C000`/`0x6001D000`, RTC_CNTL/eFuse, SYSTEM, SYSTIMER, coex, INTMATRIX (3 words) | boot only | |
  | Wi-Fi MAC `0x60033400` (`rom_mac_tx_chan_offset`, missed by the static scan) and coex `0x60033C18..` | boot only | |

  Never touched after boot: RWBT `0x60030000`, SYSTIMER, coex block.
- **Layout now:** 7 of 14 entries (flash RX, RAM+ROM RAM RW, DROM R, ROM
  RX, .iram1 RX, RWBLE RW, BT BB RW); everything else emulated through an
  allowlist (static scan + observed), deny-list for SPI0/1, GDMA, PMS,
  INTMATRIX (except the 3 BT map words, value-checked), EXTMEM. Bring-up
  115 ms (was 37 ms). GATT load test unchanged (127, 195 ms); pairing and
  bonded re-encryption work, but see below.
- **Not done** (done 2026-10-08, "EM window closed"): the EM window is still inside the RWBLE page, so baseband
  DMA is not confined (needs the `emi.o` paravirtualization, step 7).
- **Open: pairing reliability** (solved 2026-10-08: a lost-interrupt race,
  see "Timing and pairing investigation"). Just Works pairing from the Pi (fresh
  boot, `bluetoothctl pair`): coroutine kernel 4/4, sandbox coarse 3/4,
  sandbox scoped 2/5. Failures: `encryption change status=7`, then
  supervision timeout (reason 520), no access fault. Small samples, but it
  suggests the sandbox's radio-IRQ latency matters while the app runs
  P-256. Suspect: the coroutine's ISR glue was in IRAM, while the
  sandbox's transition (kernel flash) and glue (sandbox flash) suffer
  flash-cache misses under load (~40 us/call transition overhead). Next:
  measure IRQ-to-handler latency during pairing; try moving the hot path
  into IRAM (needs a kernel IRAM region outside the sandbox's).
- **Scheduler matters** (commit `b777603c8`): with round-robin instead of
  the priority scheduler, pairing succeeded 15/16 vs 13/21 (scoped
  layout, same script; Fisher p ~ 0.03). Not understood: both schedulers
  leave the process as soon as an interrupt or deferred call is pending;
  round-robin additionally arms TIMG1 for 10 ms timeslices. Worth
  explaining before relying on it (e.g. trace kernel-loop timing around
  the encryption start; check whether the app's mbedtls computation and
  NimBLE's host timers interact with timeslicing).

### Timing and pairing investigation (2026-10-08)

Experiment branch `esp32c3-ble-iram-kernel` (commits `038f96aea`..`51dce5126`,
all marked EXPERIMENT; not for merging). Pairing tests use
`tools/pair-c3.py` (fresh boot, bond removed, `bluetoothctl` with a
NoInputNoOutput agent; console and `btmon` captured per try). Arms were
interleaved in blocks of 5, at least 20 tries each.

**1. The pairing failures are a lost-interrupt race in the switch to a
process.** `Kernel::do_process` checks `continue_process` (pending
interrupts or deferred calls), then writes the process's PMP
(`setup_mpu`), arms the scheduler timer and calls `switch_to`, whose
`mret` enables interrupts in user mode. An interrupt that arrives in
between traps in machine mode, and the ESP32-C3 trap handler records it
and **masks the line** (`handle_interrupt`: `save_interrupt` +
`disable`). The process then runs with the line masked, and the kernel
only sees it at the process's next trap. Evidence:

- Latency tracing (trap → `og_bt_isr`, `mpccr` stamp and `mepc` in the
  trap handler): with the flash kernel and the priority scheduler, RWBLE
  (line 8) interrupts taken in kernel mode waited **130-330 ms**, several
  per pairing. Every such outlier's `mepc` was inside the process PMP
  code (`PMPUserMPU::configure_mpu`, `SimplePMP::configure_pmp`,
  `CSR::pmpconfig_set`).
- The window is wide because a sandbox call always leaves the sandbox's
  PMP layout behind, so the next process switch rewrites the process
  layout, and from flash with a cold cache that takes ~155 us (below).
- `btmon` on the Pi, failed tries: Pairing Request/Response, the Pi's
  Pairing Public Key, then nothing from the board until the Pi reports
  **Connection Timeout (0x08)** exactly 1.51 s later. In successful tries,
  the board's Public Key follows the Pi's after 1.51 s: that is the app's
  P-256 work (mbedtls), during which it makes no syscall. The connection
  uses a 48.75 ms interval and a **420 ms supervision timeout**. So a BT
  interrupt lost at the start of the computation starves the controller
  for up to 1.5 s.
- This explains the scheduler effect: round-robin arms a 10 ms timeslice,
  whose timer interrupt ends the stall; the priority scheduler runs the
  process without a timeslice.
- **A/B** (uninstrumented `esp32c3-ble` kernel, priority scheduler, kernel
  in flash, interleaved, `btmon` on the Pi): without the re-check below
  **6/20**, with it **20/20** (Fisher p ~ 1e-6). All 14 failures end with
  Connection Timeout right after the Pi's Pairing Public Key; no timeouts
  with the re-check.

**Fix (`b1e7ee7ba`, chip-level, on `esp32c3-ble`):** keep deferred
interrupts pending in hardware instead of masking them. An interrupt taken
in the kernel or in a process only clears `mstatus.MPIE`, so the trap's
`mret` returns with interrupts disabled and the line stays pending in the
interrupt controller. `has_pending_interrupts` reads the controller
(`Intc::pending`: enabled lines pending in `eip`, plus recorded ones), the
service loop masks each line around its handler and sets `MIE` when all
are done, and entering a process (whose `mret` sets `MIE`) takes a pending
interrupt at once. Nothing can be lost between the kernel's last check and
the process: there is no software state to lose.

```rust
/// Defer an interrupt taken in the kernel or in a process. [...]
fn defer_interrupt() {
    CSR.mstatus.modify(csr::mstatus::mstatus::mpie::CLEAR);
}

fn service_pending_interrupts(&self) {
    while let Some(irq) = self.intc.next_pending_or_saved() {
        self.service_one(irq); // disable(irq); service; complete(irq)
    }
    self.intc.enable_all();
    CSR.mstatus.modify(csr::mstatus::mstatus::mie::SET);
}
```

Details that matter:

- **Foreign code still masks.** The Omniglot runtime resumes the sandbox
  after an interrupt, and user mode takes machine interrupts regardless of
  `MIE`, so an unmasked pending line would re-trap forever. The runtime
  now calls `_disable_interrupt_trap_rust_from_foreign` (record and mask,
  the old behaviour) instead of the process entry point. That is safe
  because a sandbox call runs to completion inside kernel code, so the
  record is visible before the kernel next decides what to run.
- **Lines are masked while serviced** (`Esp32C3::service_one`): a handler
  that calls into the sandbox runs in user mode, where the still-asserted
  line would be taken again and serviced twice.
- The kernel runs with `MIE` clear from an interrupt until it is serviced
  (one extra trap per interrupt that arrives in kernel mode: the line is
  taken again after `MIE` is set, then found serviced).
- The BT boards' bring-up waits (`og_bt.rs` / `bt.rs` `poll_interrupts`)
  poll `Intc::pending` instead of the recorded set.

Rejected first attempt (experiment `3848d9c68`): re-check
`has_pending_interrupts` with interrupts disabled right before
`switch_to` in `Kernel::do_process`. It also gave 20/20, but it is a
protocol every path into user mode must follow, lives in generic kernel
code, and is only correct because RISC-V's `mret` enables interrupts
atomically with entering user mode.

**2. What dominates the time: flash-cache misses, and the PMP code most
of all.** Boot-time microbenchmark (`c86003ed7`), average of 16, "cold" =
after reading 32 KiB of app flash through the 16 KiB cache:

| Kernel in flash | warm | cold |
|---|---|---|
| sandbox PMP write (`SimplePMP::configure_raw`, 7 entries) | 1889 cyc (12 us) | 23562 cyc (147 us) |
| process PMP write (`configure_pmp`, 7 TOR regions) | 2681 cyc (17 us) | 24893 cyc (155 us) |
| empty sandbox call (`og_bt_stat`) | 720 cyc (4.5 us) | 38815 cyc (243 us) |

`og_bt_isr` split (every 4th call; `upccr` in the sandbox is the same
counter as `mpccr`), during pairing, flash kernel, priority scheduler:

| | entry (of which PMP) | handler | exit | total |
|---|---|---|---|---|
| warm (no process ran since the last call) | 1.6-2.2 us (0) | 24-27 us | 1.3 us | 28-30 us |
| cold (a process ran: PMP rewritten) | 90-97 us (49-52 us) | 45-49 us | 27-31 us | 174-188 us |
| cold, whole kernel in SRAM | 5.2 us (4.1 us) | 25 us | 1.3 us | 33 us |

The PMP code is slow for its own sake: `CSR::pmpaddr_set(index, value)` is
a non-inlined 64-arm `match` dispatched through a jump table in flash
rodata, called once per entry, and the pmpcfg words are assembled by the
generic `PmpConfigCSRIter`; even warm, 14 entries take ~1900 cycles.
Straight-line writes (`51dce5126`: build all pmpaddr values and pmpcfg
words, then `csrw` with constant CSR numbers; RMW only for the partial
`pmpcfg3`, which keeps the trap-vector entries) give, in cycles:

| | sandbox PMP warm / cold | process PMP warm / cold | empty call warm / cold |
|---|---|---|---|
| flash, before | 1889 / 23562 | 2681 / 24893 | 720 / 38815 |
| flash, straight-line | 906 / 12700 | 679 / 11672 | 723 / 40283 |
| whole kernel in SRAM, straight-line | 371 / 371 | 359 / 359 | 723 / 4544 |

From flash, a cold call is still ~28k cycles besides the PMP write (the
Omniglot invoke and return path, the driver, the sandbox glue): a flash
cache miss costs roughly 600 cycles at the current DIO 40 MHz setting.

**3. Whole kernel in SRAM (experiment `038f96aea`, `1a052ddf8`).** Kernel
`.text` through the instruction-bus alias at `0x40388000` (above the
sandbox's `.iram1`), `.rodata` after it, copied by a stub at the
direct-boot entry before the start code runs; kernel stack/data/bss moved
above app memory (`0x3FCC8000`), as the installed app is linked for RAM at
`0x3FCA0000`. PMP audit (both layouts, every 10 s): no user entry other
than the trap vector reaches kernel code or RAM.

| Per call, GATT load (`gatt-load.py 25`) | flash (phase 1) | SRAM |
|---|---|---|
| `og_bt_isr` | 76-80 us | 31 us |
| `og_bt_run` | 90-97 us | 36 us |
| `og_bt_hci_send` | ~670 us | 355 us |
| `og_bt_hci_recv` | 55 us | 5 us |
| CPU in the sandbox | 2.1% | 0.87% |

GATT median unchanged (127 round trips, 195 ms). Empty call at boot (warm):
2.9 us vs 2.5 us. The ~40 us per-call overhead and the ~300 us unexplained
in `og_bt_hci_send` were cache misses.

Pairing (instrumented builds, 20 each): SRAM 20/20 (round-robin) and 20/20
(priority); flash 19/20 (round-robin, uninstrumented) and 18/20 (priority,
instrumented); coroutine kernel 19/20 (priority). The instrumented flash
build fails far less often than the uninstrumented one, presumably because
the extra calls shift timing; the uninstrumented kernel is what the A/B in
point 1 uses. The coroutine kernel's single failure had the same signature;
presumably its window is small, as nothing else writes the PMP there, so
`configure_mpu` skips an unchanged process layout.

**Ruled out:** access faults or emulation refusals (none logged);
interrupt latency in process context (1 us) or sandbox context (≤ 0.7 ms);
a stack overflow (the kernel stack did overflow, but only in the
instrumented builds: 0x900 is too small for the profiling, and with the
kernel in SRAM it ran into `.rodata`; the stack is now 0x1800 on the
experiment branch); loss of a saved interrupt in `Intc::complete` (it runs
with interrupts disabled).

### Fixes and stress tests (2026-10-08)

Branch `esp32c3-ble` now has (on top of the plan commit `dd7ea150f`):

| Commit | Change |
|---|---|
| `b1e7ee7ba` | `chips/esp32-c3`: keep deferred interrupts pending, with `MIE` clear (above); runtime calls `_disable_interrupt_trap_rust_from_foreign` |
| `af269765d` | `chips/esp32-c3`: map TIMG1's timer interrupt from source 34 |
| `e669c2bad` | `chips/esp32`: TIMG alarms without auto-reload; `stop()` only stops |

**Two more upstream ESP32-C3 bugs, found with a CPU-bound process** (the
priority vs round-robin question in the task: a busy process must never
starve others except under the cooperative scheduler):

- With `whileone` installed, round-robin starved every other process (and
  the NimBLE app never advertised), on the stock `esp32-c3-devkitM-1` board
  too, i.e. not related to BT or Omniglot. Cause: the interrupt matrix map
  register for TIMG1's timer is at `0x088` (source 34, `TG1_T0_LEVEL`);
  `intc.rs` wrote `0x084` (TIMG0's watchdog, source 33). TIMG1 is the
  scheduler timer, so a timeslice only ended when some other interrupt
  happened to arrive. Measured directly: after a 10 ms TIMG1 alarm,
  `ALARM_EN` self-cleared and `INT_RAW` was set, but `eip` bit 31 stayed 0
  until the map offset was fixed.
- `TimG::set_alarm` left `T0CONFIG.AUTORELOAD` at its reset value (1), so
  every alarm reloaded the counter with 0 (seen as `now()` jumping back and
  a "remaining" timeslice of 24-106 ms out of 10 ms). It was only cleared as
  a side effect of `stop()` writing the whole register.

After both: stock board, round-robin, `ticker` + `whileone`: 902 wakeups/s,
worst gap 10.7 ms (one timeslice), vs 13 syscalls in 10 s before.

**Stress apps** (`~/experiment/stress-apps`, outside the repo; image built by
`tools/apps-c3.py` with TBF padding between fixed-address apps):

| Flash | RAM | App | Does |
|---|---|---|---|
| `0x40000` | `0x3FCA0000` | `nimble_bleprph` | GATT server, pairing |
| `0x60000` | `0x3FCB0000` | `ticker` | 1 ms alarm loop, prints wakeups/s and worst gap |
| `0x68000` | `0x3FCB8000` | `noisy` | 32 B RNG + 100-char console line every 20 ms |
| `0x70000` | `0x3FCC0000` | `whileone` (libtock-c `tests/whileone`) | busy loop, never yields |

Under the priority scheduler the order is the priority order, so `whileone`
runs whenever the others wait: every BT interrupt then arrives in user mode,
and every sandbox call is followed by a process PMP rewrite.

**Pairing** (`tools/pair-c3.py`, 5 arms interleaved, 20 each, `btmon`):

| Kernel | Scheduler | Apps | Pairing |
|---|---|---|---|
| before the fixes | priority | ~~NimBLE only~~ stress set (see below) | 15/20 (all 5 failures: status 7, reason 520) |
| fixed | priority | ~~NimBLE only~~ stress set (see below) | **20/20** |
| fixed | priority | stress set | **20/20** |
| fixed | round-robin | stress set | **20/20** |
| before the fixes | round-robin | stress set | 0/20 (NimBLE never advertises: starved) |

With the earlier A/B (6/20 vs 20/20), the priority scheduler without the
fix failed 11 of 40 and with the fix 0 of 60.

*Correction (2026-10-08, later):* the "NimBLE only" arms never got their
app image: `pair-arms.sh` cut esptool off after the kernel (see "Test
tooling"), and every console log of those arms shows `ticker` running. So
all four arms of this table ran with the stress set, and the "NimBLE
only" rows are a second sample of the same configuration. The kernel
comparison (before vs after the fixes) stands.

**Transactions** (`tools/stress-batch.sh`; per arm, stress set installed):

| | fixed, round-robin | fixed, priority | before the fixes, priority |
|---|---|---|---|
| GATT 25 s (x2) | 126 / 126 round trips, median 195 / 195 ms, max 553 / 627 ms | 127 / 127, 195 / 195 ms, max 557 / 561 ms | 122 / 121, 195 / 195 ms, max 560 / 658 ms |
| GATT 120 s | 605, median 195 ms, max 606 ms | 613, median 195 ms, max 562 ms | 587, median 195 ms, max 564 ms |
| connect/disconnect cycles (`tools/conn-cycle.py 50`) | 50/50; connect median 884 ms; round trip 227 ms | 50/50; 896 ms; 227 ms | **32/50**: after cycle 31 the board stopped advertising (app logged the disconnect, never `advertising` again) |
| bonded reconnects (`tools/bond-cycle.py 20`) | 20/20 | 20/20 | **1/20**: pairing worked, then every reconnect timed out |

The old kernel's wedging after disconnects was not analyzed further (the
fixed kernel does not show it); it is consistent with lost interrupts
leaving the controller or the host waiting.

Per-call costs with the stress set (120 s GATT run, kernel in flash): `isr`
70/s avg ~117 us, `run` ~68/s avg ~145 us, `send` 10/s ~650 us, `recv`
40/s ~50 us, ~2.7% CPU in the sandbox: higher than with NimBLE alone
(76-80 / 90-97 us), as three more apps compete for the flash cache.
`ticker` (higher priority than `noisy` and `whileone`): priority 768
wakeups/s, worst gap 63 ms; round-robin 81/s (one per round, as `whileone`
uses its full 10 ms slice each round), worst gap 32 ms. The 63 ms gap is
not explained yet; a likely candidate is `ticker` blocking on the console
behind `noisy`'s output, but this was not checked.

**Not done / open from this round:**

- The kernel hot path in SRAM is drafted but untested (`esp32c3-ble-hotpath`
  `a2e5ba1a6`: 21.4 KiB of trap path, runtime, PMP, drivers and kernel loop
  linked to `0x40388000`, copied by `main`). Measure it before deciding.
- The straight-line PMP writes (`39c6c2b64` on `esp32c3-ble-hotpath`) are
  measured only by the boot microbenchmark (above); not merged into
  `esp32c3-ble`.
- The instrumented flash build failed pairing only 2/20 (one of them
  without a latency outlier but with ~10 instead of ~115 process-context
  BT interrupts in its window); not explained, and moot with the fix.
- ~~Flash at DIO 40 MHz; 80 MHz would halve the miss cost.~~ Done: QIO
  80 MHz, see "Flash at 80 MHz".

### EM window closed (2026-10-08)

Commits `ba833be89`..`a26df3149` on `esp32c3-ble`.

**PMP.** The RWBLE page is now a TOR chain: RW `0x6003_1000`, read-only
`[0x6003_1204, 0x6003_12CC)` (slots 0-47 and the bitmaps for 0-47), RW,
read-only `[0x6003_12E0, 0x6003_1304)` (slots 48-55 and their bitmap), RW
up to `0x6003_2000`. 12 of 14 entries. The per-packet registers in between
and after stay direct (no RWBLE access is emulated).

- **Overlapping entries do not work on the C3.** Read-only TOR entries at
  lower indices than the RW NAPOT page (CSRs read back as intended) gave
  0 faults over a bring-up that writes the window 82 times; the
  non-overlapping chain gave 82. So a lower-numbered read-only entry does
  not restrict user mode if an overlapping RW entry exists.

**Who writes the window.** Correction to §5's table: the ROM's
`esp32c3.rom.bt_funcs.ld` assigns `r_emi_alloc_`, `_free_`,
`_overwrite_em_mapping_by_offset`, `r_emi_get_*` and both `r_emi_*_show`
to ROM jump-table entries, and a linker-script assignment silently
overrides the archive's definitions. So `r_plf_funcs_ro` pointed to the
**ROM's** copies for the runtime remapping; `emi.o` supplied only
`r_emi_em_base_init`/`_deinit`, `r_emi_init`/`_deinit` and
`emi_reset_em_mapping_by_offset`. Also: a linker-script assignment does
not count while archives are searched, so leaving any `emi.o` symbol to
the script pulls `emi.o` in (and it then clashes with a replacement).

**Design.** `og_bt/emi.c` defines every global of `emi.o`: the writers
(alloc, free, overwrite, reset, base init/deinit and their wrappers) in C,
the four read-only ones as absolute symbols at their ROM jump-table
entries; the Makefile drops the ROM list's `r_emi_*` lines. Allocation,
logging and asserts stay in the sandbox (every writer calls the sandbox
heap, which a callback cannot do: callbacks run in the trap handler, with
no nested calls into the sandbox). Each mapping change is a static
callback (`omniglot-tock`: `StaticCallback`, `set_static_callbacks`;
springboard words in kernel memory, checked before the alloc chain, as in
omniglot-lfi):

- `em_map(slot, addr)`: slot ≤ 38, `[addr, addr + window)` in `ogram`
  (`0x3FCD0000..0x3FCDF000`), window and EM offset from
  `em_base_reg_lut` (checked against the ROM at boot; slots 1, 4, 7-16
  span 2 KiB, the others 1 KiB; note the buffers are smaller than their
  windows, e.g. 328 B in slot 0). Writes the register (canonical offset)
  and sets the enable bit.
- `em_unmap(slot, em_offset)`: slot ≤ 55; clears the enable bit and the
  address. `em_base_init` unmaps slots 7-16, 27-38 and 39 (offset
  `0xCC00`, not in the LUT).
- A refusal panics with the arguments (step 9 should turn it into an
  error); a store to the window that still faults is refused.

Checked with temporary probes: `em_map(7, 0x3FC90000)` → "refused
em_map(slot 7, 0x3fc90000)"; a direct store to slot 7 → access fault,
refused (`mtval=60031220`).

**Order of work, as asked:** the fault was shown first (sandbox stores to
the EM window faulting, counted by the existing access emulation), before
any callback handled them. That is how the overlapping-entry layout was
caught: its CSRs read back correctly but nothing faulted.

**Pieces** (all in `boards/esp32-c3-devkitM-1-bt-og` unless noted):

| Piece | What |
|---|---|
| `libraries/omniglot-tock/src/rv32i_c_rt.rs` (`35409d8e1`) | `StaticCallback<ID>` (a springboard word, `unimp`, plus a handler `fn`), `set_static_callbacks(&'static [..])`; `callback_handler` matches `mepc` against the static table before walking the alloc chain, and passes the same context/return/scopes as invocation-scoped callbacks. The table is stored as a raw slice pointer so `ID` need not be `'static` elsewhere |
| `src/og_bt_em.rs` | the EM registers as `tock-registers` (`EmRegisters`, `SLOT::{ADDR, EM_OFFSET}`), `slot_geometry` + `check_slot_geometry` (against `em_base_reg_lut` at boot), `map`/`unmap` checks, `EM_CALLBACKS`, `EM_STATS` |
| `src/og_bt.rs` | RWBLE TOR chain (`EM_BANKS`); `allow_access` refuses the banks; registers the callbacks and hands their `fn_ptr`s to `og_bt_start`; reports callback counts and cycles every 10 s |
| `og_bt/emi.c` | the `emi.o` replacement (C, in the sandbox) |
| `og_bt/og_bt.{c,h}` | `og_bt_start(em_map, em_unmap)`; stats `OG_BT_STAT_EM_CALLS/_CYCLES/_MAX` (6/7/8), from `upccr` around each callback in `emi.c` |
| `Makefile` | generates `esp32c3.rom.bt_funcs.ld` without its `r_emi_*` lines |
| `src/main.rs` | kernel stack 0x900 → 0x1000 (callbacks run in the trap handler, on the kernel stack) |

**Why callbacks and not trap-and-emulate.** The task started from
trap-and-emulate (plan step 7 as revised), and the first analysis
recommended it: each `emi.o` operation does only two stores (RMW of the
slot address, RMW of the bitmap), so paravirtualization saves little and
needed runtime work. Both were then built and measured. Callbacks won:

- Cost: one callback per operation instead of two emulated stores (each
  a fault, a decode and a validation): ~10,500 vs ~20,400 cycles per
  operation at DIO 40.
- Trusted interface: the kernel builds the register word itself from
  `(slot, addr)` and the ROM table, instead of validating raw stores. A
  store filter has to track per-slot state across stores (an enabled slot
  must always have a valid mapping), because `emi.o`'s sequences pass
  through intermediate states. The rules first proposed for the filter
  would have refused normal bring-up writes and panicked the kernel:
  `em_base_init` writes slot 39 (offset `0xCC00`, not in the LUT); it is
  what sets bits `[31:18]` in the first place, so "offset unchanged"
  fails; and init, free and reset write empty addresses for disabled
  slots.
- A writer that is missed still faults, and the fault is refused, so
  nothing silently bypasses the check either way.

**Why `emi.c` is C in the sandbox, not Rust in the kernel.** Every writer
calls the sandbox heap (`malloc`/`free` through the OSI table), its asserts
and its config getter; a callback runs in the trap handler and cannot call
back into the sandbox. Moving the whole of `emi` into the kernel would mean
a kernel-side allocator (arena) for EM buffers, which was rejected. Only
the two register operations cross the boundary. Overriding just the
writers and keeping `emi.o` for the rest is not possible either: any
`emi.o` symbol left undefined pulls in the whole object, which then
clashes; so the four read-only functions are defined as absolute symbols at
the ROM's jump-table entries.

**Rates and cost** (GATT load, 25 s; kernel in flash):

| | per operation | operations |
|---|---|---|
| callback, DIO 40 MHz | ~10,500 cycles avg (66 us), max 19,000 | ~10 maps + 10 unmaps/s; 41 at bring-up |
| callback, DIO 80 MHz | ~5,200 cycles | same |
| callback, QIO 80 MHz (current) | **~2,900 cycles (18 us)** | same |
| trap-and-emulate, DIO 40 MHz (temporary variant: the same `emi.c` writing directly, 2 emulated stores per operation) | ~20,400 cycles | ~40 stores/s |
| callback at bring-up (warm) | ~1,140 cycles | |

All dominated by flash-cache misses on the kernel's trap path: ~0.13% CPU
for the callbacks at DIO 40, ~0.04% at QIO 80. None refused in any run.
GATT 25 s: 126 round trips, median 195 ms, as before. The interleaved
stress set passed afterwards ("Stress set after the EM and flash
changes").

**Not done:** a refusal still panics (needs step 9); teardown (clear all
enable bitmaps before reusing sandbox RAM) is not implemented, as the
sandbox is never torn down yet; `overwrite_em_mapping_by_offset` is
implemented but no caller was seen.

### Flash at 80 MHz (2026-10-08)

The board now runs the flash (DIO) at 80 MHz instead of 40 MHz, if a check
at boot passes: blocks of flash read through SPI1 (`esp_rom_spiflash_read`)
at 80 MHz must match the same bytes read through the cache (SPI0, still at
40 MHz). Only then is SPI0, which the kernel executes from, switched. On
the C3 no extra dummy cycles are needed (the ROM's `spi_dummy_len_fix` only
zeroes them). The ROM takes the flash for 2 MiB, so the probes stay below.

GATT 25 s, steady state, 40 → 80 MHz: `run` 130 → 70 us, `isr` 85 → 57 us,
`send` 770 → 411 us, `recv` 51 → 35 us, sandbox CPU 2.53 → 1.48%, EM
callback avg ~9,800 → ~5,200 cycles. GATT 120 s: 613 round trips, median
195 ms; pairing 5/5.

**QIO at 80 MHz** (`0ad6ee4f3`): tried only if the flash's QE bit reads
as set (`esp_rom_spiflash_read_statushigh`, SPI1). Then the HD/WP pads are
handed to the flash controller (`esp_rom_spiflash_select_qio_pins`; in
direct boot they are GPIOs, so QIO reads came back wrong without it), the
same SPI1 check runs in QIO, and only if it passes does SPI0 switch.
Fallback QIO 80 → DIO 80 → DIO 40; the pads are put back if QIO fails.
The kernel never sets QE: it was set once with a 16-bit WRSR of `0x0200`
(`set_qe.py`-style, as the ROM's `Enable_QMode` does; esptool's
`write-flash-status --bytes 2` follows its 16-bit write with an 8-bit one,
which cleared it again on this XMC flash, ID `46 4016`).

Two pitfalls found on the way:

- The ROM's `esp_rom_spiflash_config_readmode(mode, a1)` has a second
  argument that ESP-IDF's prototype lacks: for DIO and slower modes, a
  non-zero `a1` clears QE (`Disable_QMode`, a non-volatile write). All
  four ESP32-C3 boards call it with one argument (garbage `a1`); on this
  board that cleared QE at every boot after it was set. The `-bt-og` board
  now passes 0; the others still have the bug.
- With `a1 = 0`, the board hung at boot: `config_readmode` rewrites SPI0's
  mode and the cache's read command without waiting for idle (the
  `Disable_QMode` path waited, hiding this), and a cache fetch from flash
  in between hangs the chip. The switch now runs from SRAM: a 12-instruction
  position-independent routine (`Wait_SPI_Idle`, then `config_readmode`),
  copied to the start of `ogiram` before the sandbox is loaded there.

| GATT 25 s, steady state | DIO 40 MHz | DIO 80 MHz | QIO 80 MHz |
|---|---|---|---|
| `og_bt_run` avg | 130 us | 70 us | 45 us |
| `og_bt_isr` avg | 85 us | 57 us | 46 us |
| `og_bt_hci_send` avg | 770 us | 411 us | 257 us |
| `og_bt_hci_recv` avg | 51 us | 35 us | 19 us |
| sandbox CPU | 2.53% | 1.48% | 1.00% |
| EM callback avg (cycles) | ~9,800 | ~5,200 | ~2,900 |

QIO 80: 5/5 boots in QIO, GATT 25 s 127 round trips and 120 s 614 (median
195 ms), pairing 5/5, QE still set after boot. The boot log prints the
mode and SPI0's ctrl/clock/user1/user2 registers.

*Superseded 2026-10-09:* the boards now call upstream's
`esp32_c3::flash::configure` (tock/tock#5260), a naked function in the
chip crate's `.iram` section, which the direct-boot entry stub copies to
SRAM before `_start`. It reads QE (`esp_rom_spiflash_read_statushigh`),
selects the QIO pins if set, then `Wait_SPI_Idle`,
`config_readmode(mode, 0)` and `config_clk(1, 0)`. Unlike the version
below, it has no read-back check on SPI1 and no DIO 40 fallback, and it
does not change SPI1's mode bits by hand. The pitfalls below (second
argument, switching from SRAM, QIO pins) all still apply, and are
handled there. Kept for reference:

**Implementation** (`boards/esp32-c3-flash`, `configure()`, shared by all
four ESP32-C3 boards and called early in `setup` right after the PLL
switch; ROM routines called at their fixed addresses):

1. DIO at 40 MHz on both controllers (`set_flash_readmode(DIO)`,
   `esp_rom_spiflash_config_clk(2, ..)`), then a sanity check of the
   method: `matches()` reads 6 blocks of 512 B (kernel and apps, offsets
   `0x0`..`0x50000`) through SPI1 and compares them with the cache mapping
   at `0x42000000`.
2. SPI1 at 80 MHz; `reliable()` = 8 passes of `matches()`. Fail → stay at
   DIO 40.
3. QE read through `esp_rom_spiflash_read_statushigh` (bit 9 of its
   result). Clear → SPI0 to 80 MHz, DIO.
4. Save the HD/WP pad registers (`0x6000_9034/38`), call
   `select_qio_pins(0, 0)`, set SPI1's CTRL mode bits to QIO
   (`(ctrl & !0x0190_6180) | 0x0100_2000`), `reliable()` again. Pass →
   `set_flash_readmode(QIO)` (both controllers and the cache's read
   command) and SPI0 to 80 MHz. Fail → CTRL and pads restored, SPI0 to
   80 MHz, DIO.

Boot messages: `flash: QIO at 80 MHz (...)`, `DIO at 80 MHz (flash QE bit
clear)`, `DIO at 80 MHz (QIO check failed)`, `DIO at 40 MHz`, `DIO at
40 MHz (check failed)`. With QIO 80 SPI0 reads ctrl `0x01202000`, clock
`0x80000000` (undivided), user1 `0x5c000005`, user2 `0x700000eb`.

Notes:

- Whether QIO works can't be known before switching, but whether it is
  safe to try can: QE must be set (otherwise HD is HOLD# and driving it
  would be wrong), and the check on SPI1 catches everything else without
  risking the kernel, which runs through SPI0.
- Provisioning QE is a one-time, non-volatile step per board, outside the
  kernel: `~/experiment/tools/set_qe.py [port]` (esptool API: attach the
  flash, refuse unless SR2 reads `0x00`, WREN, 16-bit WRSR `00 02`, wait
  for WIP). Status reads `0x200000` before, `0x200200` after. Without
  `attach_flash` the ROM loader reads `0xffffff`.
- `set_read_mode` copies its routine (12 instructions) into a buffer on
  the stack (SRAM1) and calls it through SRAM1's instruction-bus alias
  (`+ 0x700000`), after `fence.i`. (The first version used the start of
  `ogiram`, which needed a board region and had to run before the sandbox
  was loaded there.)
- The ROM takes the flash for 2 MiB (`rom_spiflash_legacy_data`) and
  refuses reads beyond; the probes stay below.
- The other three ESP32-C3 boards (`esp32-c3-devkitM-1`, `-bt`,
  `-omniglot`) called `config_readmode(mode)` with one argument, i.e. with
  whatever was in `a1`, and so cleared QE at every boot. Since `9806c892d`
  they use the shared crate too: all four boot in QIO at 80 MHz with QE
  set, and QE stays set across boots of every board (checked on hardware;
  `-bt` GATT 25 s 127 round trips, median 195 ms; `-omniglot` demo as
  before; the stock board's console shows the mode, then the NimBLE app
  faults as that kernel has no HCI driver, as before).

### Stress set after the EM and flash changes (2026-10-08)

Arms (all with the stress apps at 0x40000, sandbox flashed per arm,
interleaved):

- **new**: `esp32c3-ble` at `9806c892d` (EM callbacks, QIO 80 MHz);
- **base**: `7956c22fb` (before both: EM window open, DIO 40 MHz), with
  its `config_readmode` call replaced by `esp32_c3_flash::set_read_mode`
  so that it does not clear QE (every new-arm boot in the run came up in
  QIO, after base arms ran in between);
- each under round-robin and the priority scheduler (`prio.py` patch).

| | new, RR | base, RR | new, prio | base, prio |
|---|---|---|---|---|
| pairing (4 × 5, interleaved) | **20/20** | 20/20 | **20/20** | 20/20 |
| GATT 25 s (x2) | 126 / 126, median 195 ms | 125 / 126, 195 ms | 126 / 126, 195 ms | 127 / 126, 195 ms |
| GATT 120 s | 613, median 195 ms, max 563 ms | 606, 195 ms, 563 ms | 613, 195 ms, 563 ms | 612, 195 ms, 559 ms |
| connect/disconnect (50) | 50/50, connect median 885 ms | 50/50, 896 ms | 50/50, 889 ms | 50/50, 903 ms |
| bonded reconnects (20) | 20/20 | 20/20 | 20/20 | 20/20 |
| `og_bt_run` avg / max (120 s) | 52 / 312 us | 136 / 713 us | 61 / 303 us | 142 / 840 us |
| `og_bt_isr` avg / max | 56 / 171 us | 112 / 433 us | 66 / 177 us | 126 / 438 us |
| `og_bt_hci_send` avg / max | 254 / 695 us | 669 / 1909 us | 259 / 699 us | 662 / 1848 us |
| sandbox CPU | 1.17% | 2.60% | 1.32% | 2.75% |
| `ticker` wakeups/s, worst gap | 94/s, 35 ms | 81/s, 51 ms | 885/s, 24 ms | 760/s, 63 ms |

EM callbacks in the new arms: 0 refused; ~3,700 cycles avg, max ~7,900,
with the stress apps sharing the cache (~2,900 with NimBLE alone).

Reading: the BLE-level results are identical between arms (they are set
by the link and the peer, not by our CPU time); the cost of confining the
DMA is invisible, and QIO more than halves the sandbox's time per call and
its worst cases, which also shows up as more CPU for the other apps
(`ticker`). The worst sandbox call (~0.7 ms, `hci_send`) is the bound on
extra radio-interrupt latency in steady state.

### Fault containment (2026-10-08)

A failed call into the sandbox no longer panics the kernel. The controller
stops, and the kernel and the processes keep running.

- **Runtime** (`omniglot-tock`): `encode_return` records a `ForeignFault`
  (mcause, mtval, mepc, foreign sp, and the reason if the kernel ended the
  call itself). The call then returns `Err(OGError::InternalError)`, and
  `take_fault()` gives the details. `emulate_access` returns 0 for an
  access it refuses, and the asm then leaves through `_return_from_og`, as
  for any fault. A static callback returns `Result<(), &'static str>`, and
  an `Err` ends the call the same way. `init` returns an error instead of
  panicking.
- **Board**: every call goes through `OgBt::call`, which returns
  `Result`; there are no more `unwrap`s on sandbox results. On the first
  failure, `fail()` does the following:
  - stops the radio: maps the three BT sources to CPU line 0 in the
    interrupt matrix (disconnected; masking the line would not stick,
    `enable_all` re-enables every line), clears the EM enable bitmaps, and
    holds the modem blocks in reset (`MODEM_RESET_WHEN_PU`);
  - prints the fault, and any refused EM callback's arguments;
  - drains the sandbox log, bounded;
  - from the next deferred call, sends the host an HCI Hardware Error event
    (code `0x5B`). Later calls fail with `OFF`, and BT interrupts still
    pending are dropped.
- **Fault injection**: `og_bt_test(kind)` (`OG_BT_TEST_*` in `og_bt.h`). A
  kernel built with `OG_BT_FAULT_TEST=<kind>` runs it 20 s after bring-up.

Results (stress apps + NimBLE, under `gatt-c3.sh 40`; each fault fired
mid-connection):

| Kind | Reported |
|---|---|
| 1 load from kernel RAM | `access not allowed`, mcause 5, mtval `0x3fcc0000` |
| 2 store to EM slot 0 | `access not allowed`, mcause 7, mtval `0x60031204` |
| 3 store to SPI1 | `access not allowed`, mcause 7, mtval `0x60002000` |
| 4 store to own code | `access not allowed`, mcause 7, mtval in `ogflash` |
| 5 `em_map(0, 0x3fcc0000)` | `em_map refused`, mcause 1 at the springboard, args printed |
| 6 jump into kernel RAM | `fault`, mcause 1, mepc `0x3fcc0000` |
| 7 `ebreak` | `fault`, mcause 3 |

In all 7 runs the kernel kept running, and `ticker` and `noisy` kept
printing to the end of the capture. In 5 of 7 runs, NimBLE got the
Hardware Error event: `disconnect; reason=1627` (`BLE_HS_HW_ERR(0x5B)`),
then `host reset`. In the other 2 runs (1 and 6), its next ACL send failed
before the event arrived, and its debug assert ended the app (`rc == 0`
in `ble_att_tx_with_conn`). That matters for recovery: while the
controller restarts, sends should probably be accepted and dropped, so
that the host lives to see the event. Without a fault (same build):
GATT 126 round trips in 25 s, median 195 ms, 1.03% sandbox CPU.

### Rebase onto upstream (2026-10-08)

Base `ab29d5abf`: origin/master `238630bf9`, then `--no-ff` merges of
`dev/esp32-c3-scheduler-timer-fix` (`074586b23`, the same TIMG fixes as
`af269765d` and `e669c2bad`, which were dropped) and
`dev/esp32-c3-deferred-interrupt` (`75f3f2b10`). The latter keeps an
interrupt taken in the kernel or a process pending and returns with MPIE
(hence MIE) clear, services lines lowest first, sets MIE at the end, and
follows the TRM's interrupt controller procedure (MIE clear, `fence` after
each write, CLEAR set then cleared, 4-cycle settle before reading EIP). That
is the scheme of `b1e7ee7ba`; `785e8ba91` is moot (no `handle_interrupt`).

On top, `39297ac7b` keeps what only the sandbox needs: the runtime's trap for
foreign code (`_disable_interrupt_trap_rust_from_foreign`) masks the pending
lines and records them in `Intc::masked` (user mode takes machine
interrupts regardless of MIE, and foreign code resumes after the trap);
`next_pending` reports them, and the service loop calls `Intc::complete`
after each handler to unmask a line it masked. Unlike `b1e7ee7ba`, lines are
no longer masked around every handler: a BT handler that calls into the
sandbox while its line is still asserted takes the foreign trap once, which
masks it until `complete`. The BT boards' bring-up polls do the same.

Results (old = `esp32c3-ble-pre-rebase`, same sandbox image, stress apps,
interleaved; `prio` = priority scheduler patched in):

| | new, RR | old, RR | new, prio | old, prio |
|---|---|---|---|---|
| pairing (4 × 5) | 20/20 | 20/20 | 20/20 | 20/20 |
| GATT 25 s (x2) | 126 / 126 | 126 / 126 | 126 / 127 | 126 / 126 |
| GATT 120 s | 614, median 195 ms, max 559 ms | 613, 195, 563 | 613, 195, 563 | 613, 195, 563 |
| connect/disconnect (50) | 50/50, median 888 ms | 50/50, 873 ms | 50/50, 883 ms | 50/50, 889 ms |
| bonded reconnects (20) | 20/20 | 20/20 | 20/20 | 20/20 |
| `og_bt_isr` avg / max (120 s) | 59 / 161 us | 57 / 163 us | 69 / 158 us | 64 / 155 us |
| sandbox CPU | 1.09% | 1.13% | 1.26% | 1.26% |
| `ticker` wakeups/s, worst gap | 94/s, 35 ms | 93/s, 43 ms | 882/s, 27 ms | 897/s, 30 ms |

Also on the rebased tree: a 20 min GATT soak (new, prio, stress apps) with
6,150 round trips, median 195 ms, max 556 ms, no faults; fault injection
kinds 1, 5 and 6 all contained under GATT load (kernel and `ticker` kept
running; kind 5's NimBLE asserted on a send before the Hardware Error event,
as kinds 1 and 6 did in "Fault containment"); `-bt` board GATT 127 round
trips and pairing 5/5; stock board with `ticker` + `whileone` ~997
wakeups/s, worst gap ~1 ms. No console log of any run showed a panic or an
unhandled interrupt.

### Rebase onto tock/tock#5260 (2026-10-09)

*The board, flash layout and memory map described here were replaced on
2026-10-10; see "Cleanup and new layout (2026-10-10)".*

**Base** `ed7ddfad9` (branch `esp32c3-ble-xip`), built from origin/master
`238630bf9` with `--no-ff` merges of:

| Merge | Upstream head | What |
|---|---|---|
| `dev/esp32-c3-scheduler-timer-fix` | `074586b23` | TIMG1 map, TIMG auto-reload, `stop()` |
| `dev/esp32-c3-deferred-interrupt` | `949240314` | deferred interrupts kept pending with MIE clear (the branch was force-pushed since 2026-10-08; only a comment changed) |
| `dev/esp32-c3-flash-xip` (tock/tock#5260) | `98ea67bd2` | XIP from flash, `esp32_c3::flash` (QIO/DIO at 80 MHz), direct-boot entry stub and `.iram` section |

plus `ed7ddfad9`, which is not part of any PR yet: the stock board's
Makefile and README use `tockloader flash --esptool` and
`tockloader install` instead of the `--local-board` flash-file flow.
origin/master has since moved to `101a99398` (a flux update only); the
base stays at `238630bf9`, which is #5260's base.

**What changed on the branch:**

- Dropped `67281a9ed` (this branch's XIP commit for the stock board): #5260
  replaces it.
- The commit that switched the boards to `esp32-c3-flash` now leaves the
  stock board alone (resolved to #5260's version). Its message still
  mentions the stock board.
- New commit `f1e2f63e0`: `-bt`, `-bt-og` and `-omniglot` use the chip
  crate's flash setup like the stock board (direct-boot entry kept in
  `rom`, a 256-byte `iram` region for `.iram`), `boards/esp32-c3-flash`
  is deleted, and their Makefiles write kernel and sandbox with
  `tockloader flash --esptool` (`-bt` drops the flash-file flow).
- Nothing else changed: `git diff esp32c3-ble-pre-xip-rebase` before
  `f1e2f63e0` is #5260 plus the stock board's Makefile/README, minus
  `67281a9ed`.

**Memory map of `-bt-og`** (vs the stock board; as of `dev/ble`):

| Region | Stock (#5260) | `-bt-og` |
|---|---|---|
| `rom` (kernel) | `0x42000000`, 256 KiB | same |
| `prog` (apps) | `0x42040000`, 3.75 MiB | `0x42040000`, 2.75 MiB |
| `ogflash` (sandbox image) | - | `0x42300000`, 256 KiB (flash offset `0x300000`) |
| `ogiram` (sandbox `.iram1`, I-bus alias) | - | `0x40380000` = SRAM `0x3FC80000`, 32 KiB |
| `ram` (kernel stack/data, then apps) | `0x3FC80000..0x3FCDEF00` | `0x3FC88000..0x3FCD0000` |
| `ogram` (sandbox data, heap, stacks) | - | `0x3FCD0000..0x3FCDEF00` (59.75 KiB; ~53 KiB used) |
| `iram` (kernel code in SRAM) | `0x403DEF00` = SRAM `0x3FCDEF00`, 256 B | same |
| ROM data | `0x3FCDF000..0x3FCE0000` | same (sandbox RW) |

The three sandbox regions are carved out of the stock board's `prog` and
`ram`; the stock regions and `iram` keep their places. The first version
after the rebase (`f1e2f63e0` on `esp32c3-ble-pre-restructure`) moved
`iram` instead, to the top of `ram`, so that `ogram` and the ROM data
stayed one NAPOT entry. Keeping `iram` in place costs 2 PMP entries: the
sandbox's RAM, which used to be one NAPOT entry over `ogram` and the ROM
data (`0x3FCD0000`, 64 KiB), is now a TOR region `[0x3FCD0000,
0x3FCDEF00)` (2 entries) plus a NAPOT entry for the ROM data (`0x3FCDF000`,
4 KiB). The sandbox now uses all 14 user entries. The single NAPOT entry
would have given the sandbox write access to the kernel's `iram` through
the data bus: execute access was never granted (the sandbox's only
executable SRAM is `ogiram`), but writing the kernel's SRAM code is just
as bad once the kernel runs any of it after the sandbox started. The
Omniglot runtime's RAM region (which bounds pointer checks and the
foreign stack) and the EM DMA bound (`og_bt_em.rs`, now from the linker
symbols) are both `ogram` only; before, the runtime's region included the
ROM data and the DMA bound was hard-coded to `0x3FCDF000`.

Checked on hardware: a sandbox store to `0x3FCDEF00` (fault kind 8,
`OG_BT_TEST_STORE_KERNEL_IRAM`) faults (mcause 7) and is contained like
the others; bring-up 112 ms; GATT 25 s 126 round trips, median 195 ms;
pairing 5/5. An `em_map` onto a window ending past `ogram` was tried
with a temporary patch, but that run did not fire; the bound is only
checked by reading the code.

**libtock-c's `TOCK_TARGETS` and Tockloader are unaffected.** The
ESP32-C3 targets of tock/libtock-c#599 are flash `0x42040080`,
`0x42050080`, `0x42060080` with RAM `0x3FCA0000`, `0x3FCB0000`,
`0x3FCC0000`. All three flash addresses are in `-bt-og`'s `prog`, and
all three RAM addresses are in its `ram`, above the kernel's ~12 KiB.
Tockloader's `esp32-c3-devkitm-1` definition only fixes the kernel at
`0x42000000` and apps at `0x42040000`; it never writes past the last
app, so it reaches the sandbox at `0x42300000` only if the apps exceed
2.75 MiB. The costs:

- the third RAM slot gets 64 KiB (up to `0x3FCD0000`) instead of
  ~124 KiB on the stock board;
- an app linked for a fixed flash address above `0x42300000` would not
  load (none of libtock-c's targets is);
- nothing stops Tockloader from writing apps over the sandbox image if
  they exceed 2.75 MiB. A `max_app_size` / app-region end in Tockloader's
  board definition would fix that, but it is board-specific.

**libtock-c:** branch `dev/ble` is origin/master `ab071148` plus a
`--no-ff` merge of `dev/esp32-c3-xip` (#599, `8e52ec9b`), then the seven
NimBLE commits, then `tests/ticker` and `tests/noisy` (the stress apps,
formerly in `~/experiment/stress-apps`). The two local `Configuration.mk`
commits of the old branch (`b18575a9`, `2c77ae5f`) are dropped, as #599
produces the identical file. #599 sits on a newer master than the old
branch did, so `dev/ble` also has 8 more upstream commits (the
syscall-return test, `tock.c` helpers). Old state:
`nimble-softdevice-demo-pre-pr599`.

**Tooling:** Tockloader at #149's head (`70e4fc9`) is installed with
`pipx` (`pipx install ~/experiment/tockloader`, with esptool injected),
so `tockloader` is on `PATH`. `tockloader list/install/flash` find the
CP2102N on `/dev/ttyUSB0` without `--port` (the imix on `/dev/ttyUSB1` is
skipped). `nimble_bleprph` now builds from libtock-c on this host
(`examples/nimble_bleprph`; the submodules are checked out), so the
read-back TBF in `stress-apps/` is no longer needed. With #599's targets,
NimBLE (120 KiB of flash) occupies the first two flash slots, so only the
third is left for a second app. The stress set needs its own addresses
as before (see "Test tooling"), but can be installed with `tockloader
install` instead of `apps-c3.py`.

**Verified on hardware** (kernel at `f1e2f63e0`):

- stock board: `make flash` (Tockloader), `tockloader install` of
  `c_hello`: QIO at 80 MHz, "Hello World!";
- `-bt-og`: `make flash` (kernel + sandbox through Tockloader), NimBLE
  built from libtock-c and installed with Tockloader: sandbox bring-up
  113 ms, advertises, GATT 25 s 126 round trips, median 195 ms, max
  557 ms, no faults; `og_bt_isr` 39 us avg, `og_bt_run` 22 us avg while
  advertising;
- `-bt`: boots in QIO, NimBLE advertises;
- `-omniglot`: builds; not run (its demo image would overwrite the BLE
  sandbox at `0x300000`).

Not rerun: pairing, the stress set, fault injection. The flash and
layout changes don't touch the code they exercise, but the stress set
should run once before relying on this.

### Cleanup and new layout (2026-10-10)

**Structure.** The BLE support is part of the stock board
(`boards/esp32-c3-devkitM-1`), as additions; there is no separate board.
Without the controller's image in the kernel binary, the board runs as
before, without BLE (checked on hardware before the layout change, with
the image's flash region erased).

| What | Where |
|---|---|
| kernel driver | `src/esp_ble_controller_driver/`: `mod.rs` (`EspBleController`: loading, PMP layout, access emulation, calls, fault containment), `exchange_memory.rs` (EM callbacks), `instrumentation.rs` (debug) |
| HCI syscall driver | `src/hci_driver.rs` |
| the image | `esp_ble_controller_image/`: `os_adapter.c`, `emi.c`, `green_threads.S`, `function_table.c`, `sandbox.h` (entry points `sandbox_*`), `layout.ld` |
| bindings | `build.rs` (bindgen over `sandbox.h`) |

**Flash layout.** Apps start at `0x42100000` (tock#5260, libtock-c#599
and tockloader#149 were updated to match; libtock-c's three slots are now
256 KiB apart). The image is written into the kernel binary (1 MiB):

| Range | What | Used | Slack |
|---|---|---|---|
| `0x42000000..0x42070000` | boot header and kernel | 81 KB | ~367 KB (ASSERT in layout.ld) |
| `0x42070000..0x42080000` (`ble_image`) | descriptor, function table, load copies of .iram1 and .data (kernel reads) | 28 KB | 36 KB |
| `0x42080000..0x420C0000` (`ble_flash`) | image .text and .rodata: the sandbox's code window, one 256 KiB NAPOT PMP entry | 122 KB | 134 KB |
| `0x420C0000..0x42100000` | unused, kernel attributes at the end | | |
| `0x42100000..` | apps (3 MiB) | | |

RAM is as before: `ble_iram` `0x3FC80000..0x3FC88000` (sandbox .iram1),
kernel and apps `0x3FC88000..0x3FCD0000`, `ble_ram`
`0x3FCD0000..0x3FCDEF00` (below the kernel's `iram`), ROM data above.
`make` writes the image into the kernel binary at offset `0x70000` with
`dd` (objcopy cannot: LLVM's refuses to move a section of an executable,
and GNU's adds it outside any segment, which the LLVM `.bin` step then
drops); `BLE_IMAGE=0` builds and flashes the kernel alone.

**Loading without an Omniglot binary format.** The runtime
(`omniglot-tock`) no longer parses a binary or runs a foreign `init`:
`TockRv32iCRt::new` takes a `ForeignMemory` (code and RAM regions, stack
top, function table). The board reads the image's descriptor (magic
"BLEC", at `ble_image`), checks every range against its regions, zeroes
the sandbox's RAM and copies .data and .iram1 itself. Removed from the
runtime: `binary.rs` (header parser, TBF lookup), `init_riscv32.S` and the
foreign init path, `MpuProtection`, the benchmark helper, and the unused
`omniglot_c_rt` files. The chip-specific instruction-bus alias for
decoding emulated accesses moved from the runtime to the board
(`AccessEmulation::code_address`).

**Other changes in the cleanup:**

- Received packets reach the HCI client from the deferred call, not from
  within `send` (no callbacks in downcalls).
- The image's `fail()` logged through the ROM's `ets_printf`, which
  writes UART0, which the sandbox cannot reach: the message never
  appeared. It now goes to the log ring, which the kernel drains on a
  fault.
- `hci_driver.rs` lost its `static mut` buffer; `pmp_layout` returns a
  fixed array of 14 entries, so the entry count is checked at compile
  time; the EM DMA bound and the runtime's RAM region come from the
  linker symbols.
- Debug instrumentation is in one module, off with `BLE_DEBUG=0` (4.6 KB
  less kernel text); the image's debug parts (`sandbox_stat`,
  `sandbox_test`, counters, stack painting) are marked "Debug".
- `make clippy` passes for the whole tree, including `omniglot-tock`.

**Checked on hardware** (final tree, new layout): boot, bring-up 114 ms,
GATT 25 s 127 round trips (median 195 ms), pairing 3/3, NimBLE installed
with tockloader at `0x42100000`. Earlier in the cleanup (same driver, old
layout): debug-off boot, no-image boot, fault kind 8 (store to the
kernel's `iram`) contained. Not rerun: the stress set, fault injection
on the final tree.

**Pitfalls met:** `make clippy` and the editor's background checks fill
`target/debug` (13 GB): clear it with `cargo clean --profile dev`; a full
disk breaks builds in odd ways. The USB serial devices re-enumerated
once, swapping `/dev/ttyUSB0` and `1` (the tools now use the by-id path).

### Refactor into a chip crate (2026-10-10)

**Structure.** The kernel driver and the sandbox are one crate,
`chips/esp32-c3-ble-omniglot`, which the stock board depends on. Other
ESP32-C3 boards do not depend on it, so they do not pull in Omniglot.

| What | Where |
|---|---|
| kernel driver | `src/lib.rs` (`BleOmniglot`: loading, PMP layout, calls, fault containment) |
| access emulation | `src/emulation.rs` (`Emulation`: the MMIO allowlist, the interrupt matrix, and the EM store filter) |
| debug | `src/instrumentation.rs` |
| bindings | `src/bindings.rs`, checked in, regenerated by `tools/build/ble-omniglot-bindgen` (`make ble-omniglot-bindings`) |
| sandbox | `sandbox/`: `main.c` (entry points, function table), `osi.c` (what the controller calls), `controller.c` (bring-up and HCI), `threads.c` (green threads), `debug.c`, `libbtdm.h`, `glue.h`, `sandbox.h`, `layout.ld` |
| sandbox build | `sandbox.mk`, included by the board's Makefile |

The board reads the linker symbols and passes the sandbox's regions, its
PMP and its interrupt controller to `BleOmniglot::create`. The crate reads
no linker symbols. The board no longer needs a build script of its own or
libclang.

**Registers.** SYSTIMER, SYSCON and the interrupt matrix are described
with tock-registers in `chips/esp32-c3` (`systimer.rs`, `syscon.rs`, and
one map register per source in `intc.rs`). The ROM's EM slot table is a
typed `StaticRef`. The only raw pointer accesses left in the driver copy
memory.

**EM mapping registers.** `emi.c` and the two static callbacks are gone.
The sandbox links the controller's own `emi.o` and the ROM's `r_emi_*`
functions, as ESP-IDF does. Their stores to the read-only EM registers
fault, and `Emulation` performs a store only under these rules:

- A store to a slot register is allowed if the slot is disabled, or if
  the new value maps a safe window.
- A store to an enable bitmap is allowed if every slot it newly enables
  already maps a safe window. Disabling slots is always allowed.
- Only 4-byte stores are allowed.

A safe window belongs to a slot the controller uses (0-38). It is at the
slot's EM offset from the ROM's table, and lies entirely in the sandbox's
RAM. The filter keeps no state of its own. It reads the current registers.
This works because every writer leaves an enabled slot valid between its
stores. The ROM's alloc writes the address and then sets the enable bit.
Its free clears the enable bit and then the address. Its overwrite
replaces the address of an enabled slot with one store. `emi.o`'s
`em_base_init` writes each address before its enable bit, and
`emi_reset_em_mapping_by_offset` disables a slot before clearing it.
`StaticCallback` and `set_static_callbacks` were removed from
`omniglot-tock`.

**Sandbox code.** The C code is split by caller. Semaphores and queues
loop on `thread_wait` instead of passing a predicate to a wait function.
A wait outside a thread now fails on purpose (it was never seen). The
context switch is a naked C function, and a new thread starts in a C
function, so the sandbox has no assembly file. The log and the RX queue
share one byte ring type.

**Checked on hardware:** GATT 25 s, 127 round trips (median 195 ms) after
the EM change and again after the C split; 30/30 connect/disconnect
cycles after the EM change, with no refused store. The EM page shows about
40 emulated stores per second under GATT load. Not rerun: pairing, the
bonded reconnects, fault injection (`BLE_FAULT_TEST=2` should now be
refused by the filter).

**Pitfalls met:** the CP2102N is now `/dev/ttyUSB1` and the imix is
`/dev/ttyUSB0`, so `tockloader` needs `--port /dev/ttyUSB1`. The tools use
the by-id path. Building the bindings generator puts about 370 MB in
`tools/target`.

### If the kernel in flash is ever too slow

Not needed so far (the kernel runs from flash with BLE timing unchanged),
but this is the order of fixes with the least impact on the shared Tock
code:

1. **Cheaper misses** (no code moves): flash speed (done: QIO 80), then
   the straight-line PMP writes (`39c6c2b64` on `esp32c3-ble-hotpath`):
   the PMP code was the worst single offender (~150 us cold at DIO 40),
   from a 64-arm `match` whose jump table sits in flash rodata.
   Self-contained in `SimplePMP`.
2. **Move hot code with the board's linker script, not with
   annotations.** With one section per function, each non-inlined function
   keeps a `.text.<mangled>` section whose name contains its crate and
   module path, so `layout.ld` can pull modules into SRAM by pattern:
   `*(.text.*omniglot_tock*)`, `*(.text.*5riscv3pmp*)`,
   `*(.text.*8esp32_c34intc*)`, `*(.text.*6kernel6kernel*)`, … The draft
   `a2e5ba1a6` (`esp32c3-ble-hotpath`, never run) does this: 21.4 KiB of
   trap path, runtime, PMP/CSR, `og_bt`/HCI drivers, interrupt code,
   kernel loop, `memcpy`/`memset`, linked to run at `0x40388000`, loaded
   from flash, copied by `main`. Pitfalls:
   - Tock's generic `.ramfunc` doesn't work on the C3: it links at the
     data-bus address, and the C3 executes SRAM only through the
     instruction-bus alias, `0x700000` higher. A separate output section
     (run address on the I-bus alias, load address in flash) is needed.
   - Out-of-line callees stay in flash unless matched (`core::fmt`,
     `compiler_builtins`, panic paths, CSR helpers); inlined code moves
     with its caller.
   - Rodata (jump tables, vtables, constants) is fetched through the same
     cache: move the matching `.rodata.*` too.
   - Patterns depend on mangling and on where generics are defined; a
     miss is silent (slow, not broken).
   - `#[unsafe(link_section)]` only for the odd board- or chip-local
     function a pattern can't select; not in `kernel/` or `arch/`.
3. **Hot vs cold:** a static check after linking: walk the final ELF's call
   graph (direct `jal`, resolvable `jalr`, rodata references) from the
   SRAM roots and list everything reachable still at a flash address
   (`0x42…`). Not written yet. Dynamically, the `mpccr` stamps of
   `c86003ed7` split a sandbox call into entry/PMP/handler/exit (port to
   `defer_interrupt` first); PC sampling is blind exactly where it
   matters (trap handler, callbacks and sandbox calls run with interrupts
   off). Roots: trap entry/exit, `defer_interrupt` and the service loop,
   Omniglot invoke/return/callback and access emulation, PMP writes, the
   `og_bt` driver, and the process switch.

### Phase 2: tighten (only after phase 1 works)

6. ~~Fault-driven MMIO allowlist~~ **Done** differently: trap-and-emulate
   with per-page counting gave the profile in one run; see "Phase 2
   progress". The bring-up shares the runtime layout (emulated).
7. **Done** with static callbacks after all; see "EM window closed". The
   direction first taken (2026-10-08) was: **trap-and-emulate
   the EM mapping register writes** with the existing emulation path
   (`set_access_emulation` / `allow_access`), validating slot and target
   range, instead of paravirtualizing `emi.o` with static callbacks. That
   needs no runtime callback support and no changes to the sandbox image;
   the cost (~2.4 us per emulated access) only applies to the mapping
   writes (per ACL/adv TX buffer, plus bursts at init and HCI reset), as
   long as the PMP keeps the rest of the RWBLE page directly accessible.
   Original plan, kept for reference: static callbacks in the runtime;
   paravirtualized `emi.o` (§5); EM window closed; `interrupt_alloc` and
   the Wi-Fi MAC coex registers as callbacks.
8. DMA-safe layout (§6). EM banks: **done** (TOR chain; overlapping
   entries do not work on the C3, see "EM window closed"). Remaining: the
   rest of §6's layout, if the MMIO allowlist should become PMP entries
   instead of emulation.
9. ~~Fault containment~~ **Done**: a sandbox fault returns an error and
   disables the radio, instead of panicking the kernel ("Fault
   containment").
10. **Resilience and stress testing.** Make the sandboxed stack survive
    misbehaviour and load, then try to break it.
    - Recovery: on a sandbox fault, assert, emulation refusal, or a call
      that runs too long (watchdog via the cycle counter or a TIMG alarm
      checked on return), stop the radio (clear the EM enable bitmaps, mask
      the BT interrupt lines, power down the modem domain), report an
      error to the HCI client, and re-initialise: reset the sandbox RAM,
      rerun `omniglot_init` and the bring-up, and let the NimBLE app
      re-sync (it already handles HCI reset). Needs the runtime to return
      an error instead of panicking in `encode_return`/`emulate_access`.
    - Bounds checks on everything the kernel takes from the sandbox:
      status words, packet lengths from `og_bt_hci_recv`, log bytes, and
      the IRAM descriptor (done); RX/log ring overflow counters exported.
    - ~~Explain the scheduler effect on pairing~~ **done** (lost-interrupt
      race, fixed). Transition overhead: flash at QIO 80 cut it ~3x
      (done); kernel hot path in SRAM drafted and PMP fast path measured,
      both only needed if timing gets tight ("If the kernel in flash is
      ever too slow").
    - Partly done (2026-10-08): stress apps next to NimBLE (busy loop, 1 ms
      ticker, console/RNG load), pairing loops, 120 s GATT runs,
      connect/disconnect and bonded-reconnect cycles, under both
      schedulers. Not yet: hours-long runs, throughput, scanning, HCI
      reset, fault injection.
    - Stress tests, each with the controller in the sandbox vs the
      coroutine kernel, recording failures, latency and sandbox CPU:
      - long GATT load (hours, not 25 s) and repeated connect/disconnect
        cycles (hundreds);
      - pairing loops (fresh bond each time) and bonded reconnects;
      - maximum throughput: notifications at the shortest connection
        interval, MTU 256, data length extension;
      - concurrent load: a CPU-bound second process, heavy console output,
        and alarm-heavy apps, to stress interrupt latency;
      - scanning with many advertisers nearby (RX ring and HCI event
        bursts), and extended advertising;
      - controller-side errors: HCI reset mid-connection, host restart,
        Pi adapter power-cycled during a connection;
      - fault injection: corrupt sandbox state on purpose (bad pointer
        into an OSI call, write outside the allowlist, infinite loop) and
        check that the kernel survives and recovers (needs step 9);
      - long idle (advertising for hours) to catch timer wraps
        (`get_time_us`, low-power clock conversions).
    - A test script on the Pi that runs the set unattended and logs
      results, plus a kernel build flag to dump the sandbox stats
      (`og_bt_stat`, emulation counts) on request from the process
      console.

## Notes for whoever implements this

- **Hardware:**
  - The only access to the board is the CP2102N UART (flashing and
    console); there's no JTAG. Its `/dev/ttyUSBn` number changes when the
    USB devices re-enumerate (an imix is attached too), so the tools use
    `/dev/serial/by-id/usb-Silicon_Labs_CP2102N_...`.
  - `tools/console-c3.py <secs>` resets the board and captures the console.
    `esptool` is in `~/.local/bin`, `tockloader` (pipx, #149) too.
  - `make -C boards/esp32-c3-devkitM-1 flash` builds the controller's
    image and writes the kernel binary with it (1 MiB at `0x42000000`)
    with `tockloader flash --esptool`; `BLE_IMAGE=0` leaves the image out.
    Apps go on with `tockloader install <tab>` (or `--erase` to replace
    all), from `0x42100000`. Neither touches the other.
  - `BLE_DEBUG=0` turns the driver's debug instrumentation off;
    `BLE_FAULT_TEST=<kind>` builds a kernel that faults the sandbox on
    purpose 20 s after the bring-up.
- **Peer for on-air tests:** the Raspberry Pi 5's adapter (BlueZ, bleak
  installed). `tools/gatt-load.py <secs>` connects to "Tock-NimBLE" and
  times write+read round trips.
  - Baseline with the coroutine kernel: 127 round trips in 25 s, median
    195 ms.
- **Debugging a sandbox fault:**
  - The kernel prints `og_bt: sandbox stopped in <call>: <reason>, mcause
    .. mtval .. mepc .. sp ..` (reason: `fault`, `access not allowed` and
    the other emulation refusals, or `em_map/em_unmap refused`, followed by
    the refused arguments), then the sandbox's remaining log. Symbolize mepc against the
    sandbox ELF (or the ROM ELF at
    `~/experiment/esp/esp-rom-elfs/esp32c3_rev101_rom.elf`, which has full
    symbols).
  - The ROM's BT asserts print to UART0 directly. That keeps working in
    phase 1, where all MMIO and ROM RAM are reachable. `r_ke_state_*` asserts
    end in `ebreak`, which shows up as a sandbox fault with mcause 3.
- **Known ESP32-C3 quirks:**
  - The core has no `mie`/`mip` CSRs (illegal instruction, even in M-mode).
  - The ROM assumes 20 ticks/µs until `ets_update_cpu_frequency(160)`.
  - The modem clock mask must be `0x00FB9FCF`; ESP32C3_BLE.md has the
    other pitfalls.
  - The trap vector table needs U-mode X: the chip crate reserves 2 PMP
    entries for that.
  - PMP priority between overlapping entries is not honoured: a
    lower-numbered read-only entry does not restrict U-mode under an
    overlapping RW one. Use non-overlapping (TOR-chained) layouts.
  - ROM `esp_rom_spiflash_config_readmode(mode, a1)`: non-zero `a1`
    clears the flash's QE bit for DIO and slower modes; and changing the
    read mode while running from flash can hang the chip (run it from
    SRAM after `Wait_SPI_Idle`). See "Flash at 80 MHz".
  - In direct boot the flash's HD/WP pads are GPIOs; QIO needs
    `esp_rom_spiflash_select_qio_pins` first.
  - The ROM assumes a 2 MiB flash for its SPI1 reads.
  - The board's flash (XMC, ID `46 4016`) has QE set now (non-volatile).
- **Omniglot runtime facts:**
  - Each call reprograms the PMP and runs foreign code with the kernel's
    `gp`/`tp`.
  - Interrupts during foreign code are masked and recorded via
    `_disable_interrupt_trap_rust_from_app`, then foreign code resumes.
  - `ecall` from foreign code means "return", so don't use `ecall` in the
    glue.
  - Callbacks run inside the trap handler (on the kernel stack, so keep it
    large: 0x1000 on `-bt-og`) and cannot call into foreign code. Besides
    the invocation-scoped ones, `StaticCallback`s registered with
    `set_static_callbacks` stay valid across invocations (the EM
    callbacks).
  - A linker-script symbol assignment (e.g. the ROM's
    `esp32c3.rom.bt_funcs.ld`) silently overrides archive definitions, but
    does not count while archives are searched.
  - The board's `make` builds the kernel, the controller's image (`make
    ble-image` alone), and the kernel binary with the image in it.
  - The bindgen fork needs libclang (`libclang-dev` is installed). The
    allowlist must name every exported function.
- **Tools:**
  - `tools/mmio-scan-c3.sh` re-runs the static MMIO scan.
  - The function-level BT analysis used `riscv64-unknown-elf-objdump -dr` on
    `libbtdm_app.a` (relocations show the OSI/plf table offsets; the table
    layouts are in §5 and in `bt.rs` `OsiFuncs`).
- **Repo rules** (`AGENTS.md`):
  - commit trailers name the model;
  - no AI-written PR/issue prose;
  - `### Safety` comments on `unsafe`;
  - `make -C boards/<board>` to build.
  - Work on branch `dev/ble` (in every repo).

### Test tooling (2026-10-08)

All in `~/experiment/tools` (not a git repository) unless noted:

- `pair-c3.py <tries> [logdir]`: Just Works pairing from a fresh boot,
  bond removed first; per-try console log with host timestamps and the
  `bluetoothctl` transcript. Success = "Pairing successful" and the app's
  `encryption change; status=0`.
- `pair-arms.sh <rounds> <per-round> <outdir> label=kernel.bin,apps.bin ...`:
  interleaves pairing blocks across arms, flashing kernel (0x0) and app
  image (0x40000) per block, with `sudo btmon -w` per block. Decode with
  `btmon -r <file>`; a failure shows the Pi's "Pairing Public Key" then
  "Connection Timeout (0x08)".
- `stress-batch.sh <outdir> label=kernel.bin,apps.bin ...`: per arm, two
  25 s and one 120 s GATT run (`gatt-c3.sh`), `conn-cycle.py 50`,
  `bond-cycle.py 20`.
- Both take an optional third image per arm, `label=kernel.bin,apps.bin,sandbox.bin`,
  written at 0x300000 (the `-bt-og` sandbox differs between arms when
  `og_bt/` changed). Each block's esptool output is kept
  (`<arm>-r<n>.flash.log`, `<arm>.flash.log`) and every image's hash
  must verify.
- **Caveat for runs before 2026-10-08 15:40:** both scripts piped esptool
  into `grep -q`, which exits at the first "Hash of data verified"; esptool
  died before writing the next image. Kernels were always written (first
  image), apps images were not: the "NimBLE only" pairing arms in "Fixes
  and stress tests" ran with the stress set (corrected there). Found
  when a per-arm sandbox image was not written either.
- `conn-cycle.py <n> [rts]`: connect / write+read / disconnect cycles.
- `set_qe.py [port]`: sets the flash's QE bit once (non-volatile; see
  "Flash at 80 MHz"). Run by hand, not from scripts.
- `bond-cycle.py <n> [log]`: pair once, then reconnects that must
  re-encrypt with the bond (checked on the console).
- `apps-c3.py <out.bin> <tbf>[@offset] ...`: app region image for flash
  0x40000, with TBF padding headers between fixed-address apps and a zero
  header at the end. *Since 2026-10-09:* `tockloader install` does the
  padding itself, so this is only needed to make the per-arm images that
  `pair-arms.sh` and `stress-batch.sh` write with esptool.
- Stress apps: `libtock-c/examples/tests/{ticker,noisy,whileone}` and
  `examples/nimble_bleprph` (all on libtock-c `dev/ble`). Since
  2026-10-10, #599's three slots are 256 KiB apart, so NimBLE, `ticker`
  and `noisy` build with the default targets and take one slot each;
  `whileone` needs a fourth address, e.g. `PATH=$HOME/.cargo/bin:$PATH
  RUSTUP_TOOLCHAIN=nightly-2026-07-21-aarch64-unknown-linux-gnu make
  TOCK_TARGETS="rv32imc|rv32imc.0x421C0080.0x3FCC8000|0x421C0080|0x3FCC8000"`
  (not yet tried). Install them with `tockloader install`, or make an
  image with `apps-c3.py`. `pair-arms.sh`, `stress-batch.sh` and
  `apps-c3.py` were moved to the apps at flash `0x100000` and lost the
  separate sandbox image argument; none was rerun since. (`elf2tab` 0.13
  was installed with `cargo install`.)
- Latency/transition instrumentation lives on `esp32c3-ble-iram-kernel`
  (`c86003ed7`): `IRQ_TRACE` in the chip's trap handler, histogram and
  outliers with `mepc` in `og_bt.rs`, `og_bt_isr` split, boot
  microbenchmark. It predates the interrupt fix and the rebase (it stamps in
  `handle_interrupt`, which no longer exists), so port the stamp to the
  trap handlers in `chips/esp32-c3/src/chip.rs` before reusing it.

## Housekeeping found along the way

- `AGENTS.md` forbids `unsafe` in new code under `libraries/`;
  `libraries/omniglot-tock` is mostly `unsafe` and asm. Move it, e.g. next to
  `arch/` or into a board-support directory.
- ~~`make clippy` fails in `libraries/omniglot-tock`~~ Fixed 2026-10-10;
  the whole tree passes.
- The board runs the `esp32-c3-devkitM-1` kernel with the controller's
  image from `dev/ble`, and `nimble_bleprph` (built from libtock-c
  `dev/ble`) alone at `0x42100000`. Its flash has QE set.
- Branches: see "Branch layout".
- Bonds are kept in RAM by the NimBLE app: after a board reset, remove the
  bond on the Pi (`bluetoothctl remove`) or reconnects fail (reason 517).


## Branch layout

Every repo works on branch `dev/ble`.

**tock** `dev/ble`: origin/master `238630bf9`, `--no-ff` merges of
`dev/esp32-c3-scheduler-timer-fix`, `dev/esp32-c3-deferred-interrupt` and
`dev/esp32-c3-flash-xip` (tock#5260 at `5537fd086`, with apps at
`0x42100000`, merged as `46b99feb0`). Then, compacted on 2026-10-10:

| Commit | What |
|---|---|
| `5f61b2dbb` | the stock board's Tockloader Makefile/README |
| `5ba21f6c3` | `riscv/pmp`: track the applied user configuration in the PMP implementations |
| `ac59e9a3b` | `riscv/pmp`: let `SimplePMP` users write raw user-mode entries |
| `823413aa7` | `chips/esp32-c3`: mask lines for interrupts taken in foreign code; SYSTIMER, SYSCON and per-source interrupt map registers |
| `7e09898bd` | `kernel::hil::bluetooth_hci`, the `bluetooth_hci` capsule and component, `StreamingProcessSlice::remaining_capacity` |
| `35c4b8744` | nRF52840 SoftDevice Controller board (`nrf52840dk_sdc`), as an HCI transport |
| `8a7200c86` | `libraries/omniglot-tock`: the vendored runtime |
| `f39380057` | `chips/esp32-c3-ble-omniglot`: the sandbox (C glue, layout, `sandbox.mk`) |
| `3323f060b` | `chips/esp32-c3-ble-omniglot`: the kernel driver, emulation and bindings; `tools/build/ble-omniglot-bindgen` |
| `407e6f7db` | `chips/esp32-c3-ble-omniglot`: debug instrumentation and fault injection (kernel and sandbox) |
| `61730a532` | `esp32-c3-devkitM-1`: the board integration |
| (this commit) | docs: this plan and `ESP32C3_BLE.md` |

Archive branches: `dev/ble-pre-compact` (the history before the
compaction of 2026-10-10, with the step-by-step refactor commits),
`dev/ble-pre-cleanup` (2026-10-09: the restructure with a separate
`-bt-og` board, then the cleanup as one WIP commit),
`esp32c3-ble-pre-restructure`, `esp32c3-ble-pre-xip-rebase`,
`esp32c3-ble-pre-rebase`, `esp32c3-ble-iram-kernel`, `esp32c3-ble-hotpath`
(PMP fast path `39c6c2b64`, untested hot-path SRAM layout `a2e5ba1a6`),
`softdevice-controller-demo`; `pr-5260` and `dev/esp32-c3-flash-xip`
(local copies of #5260; the stash `pre-PR-5260 local changes` belongs to
the latter). The `-bt` board is archived as
`~/experiment/esp32-c3-devkitM-1-bt.zip`, with `ESP32C3_BLE.md`.

**libtock-c** `dev/ble`: master `ab071148`, `--no-ff` merge of #599
(`6d574fb1`), NimBLE (7 commits), `tests/ticker` and `tests/noisy`.
Archive: `nimble-softdevice-demo-pre-pr599`.

**tock-firmware** `dev/ble`: the blobs crate for the image
(`esp32c3-bt/resources`, used by the board's Makefile). Its commits, and
`main`'s `f3cec39` (nrf-sdc), exist only in this clone.

**tockloader** `dev/ble`: #149's head (`1bdec0e`), installed with pipx.

**`~/experiment`:** `tock`, `libtock-c`, `tockloader`, `tock-firmware`
(the repos above); `esp/` (`esp-rom-elfs` for symbolizing, plus
re-cloneable `esp-hal`, `esp-idf`, `esp32c3-bt-lib`, `crates/`);
`omniglot-tock` and `nrfxlib` (upstream clones); `tools/` (test scripts,
not under git); `sessions/` (Claude session transcripts);
`esp32-c3-devkitM-1-bt.zip`.
