# ESP32-C3 BLE bring-up (working notes)

Goal: replicate the `nrf52840dk_sdc` experiment on the ESP32-C3-DevKitM-1 —
link Espressif's binary BLE controller into the kernel, expose it to userspace
as an H4 HCI transport on driver `0x30001`, and run the NimBLE host in a
libtock-c process (`nimble-softdevice-demo` branch, `examples/nimble_bleprph`).

This is an experiment: binary blobs linked into the kernel are fine here.
Keep it minimal — get the stack working, don't build general infrastructure.

## Repositories

| Repo | Branch | Base |
|---|---|---|
| `tock` | `esp32c3-ble` (on top of `softdevice-controller-demo`) | `origin/master` ff8a680c1 |
| `libtock-c` | `nimble-softdevice-demo` | `origin/master` 3765d5cb |
| `tock-firmware` | `main` (+ new `esp32c3-bt` crate) | |
| `mynewt-nimble`, `mbedtls` | 79355f9, 3.6.4 | submodules of libtock-c under `nimble/` |

Pre-rebase heads are tagged `backup/*-pre-rebase` in both repos.

Reference material (not committed) lives in `~/experiment/esp/`:
`esp-hal` (esp-radio's Rust BLE port is the main reference),
`esp-idf` sparse checkout (`components/bt/controller/esp32c3/bt.c`, ROM
linker scripts), `crates/esp-wifi-sys-esp32c3` (the blobs).

## Hardware

- ESP32-C3 (QFN32) rev v1.1 (ECO7), 40 MHz crystal, 4 MB embedded flash,
  MAC `d8:85:ac:20:b1:7c`.
- Only access is the CP2102N UART (`/dev/ttyUSB0`): esptool for flashing,
  115200 console. No JTAG.
- Attached to a Raspberry Pi 5, whose built-in adapter (BlueZ) is the peer
  for on-air tests; `bluetoothctl` needs an agent (`agent NoInputNoOutput`)
  to pair.

## Background findings

### The controller lives (mostly) in mask ROM

The C3's 384 KiB on-die ROM at `0x4000_0000` holds, besides the first-stage
bootloader and parts of libc, Espressif's BLE link layer (`r_ke_*`, `r_lld_*`,
`r_llc_*`, `r_hci_*`, …) as ordinary RV32IMC code. It is not a coprocessor and
has no special privilege: it executes on the single core, in whatever mode
calls it — for us, machine mode, inside the kernel, with the same trust as a
linked blob. It is there to save flash and RAM.

`libbtdm_app.a` is glue and patches on top of it: the controller task, the
VHCI interface, the OS-adapter hookup, and replacement functions that the ROM
reaches through function-pointer tables in RAM (`r_osi_funcs_p`,
`r_plf_funcs_p`, `r_modules_funcs_p`, …). ROM entry points are bound by
address through Espressif's linker scripts.

Trial link (`-gc-sections`, entry points used by esp-radio, esp-hal's ROM
linker-script set: `api, eco3, rom, libgcc, version, bt_funcs, eco3_bt_funcs,
additional` + `libc-suboptimal_for_misaligned_mem`):

- `libbtdm_app` + `libbtbb` + `libphy` + `libcoexist`: ~76 KiB text,
  ~8 KiB rodata, <2 KiB data/bss.
- Only unresolved symbol: `phy_printf`.

### ROM-owned RAM

Read from the chip's `ets_rom_layout_p` (`0x3ff1c130`):

| Range | Owner |
|---|---|
| `0x3FCDC710–0x3FCDE710` | ROM bootloader stack (free after boot) |
| `0x3FCDF05C–0x3FCE0000` | "rtos reserved": ROM data/bss for btdm, phy, coexist, pp, … |
| `0x3FCDF098–0x3FCDF11C` | btdm data/bss |
| `0x3FCDF35C–0x3FCDF64C` | phy ROM data |
| `0x3FCDF96C–0x3FCDFFD0` | btdm interface data/bss |

Nothing Tock-owned may live above `0x3FCDF000`; the board's RAM ends there.

### Controller interface

- HCI is VHCI: `API_vhci_host_send_packet()` (gated by
  `API_vhci_host_check_send_available()` / the `notify_host_send_available`
  callback), and the controller pushes host-bound packets through
  `notify_host_recv(ptr, len) -> int`.
- An OS adapter table (`btdm_osi_funcs_register`, version `0x0001000B`, ~50
  entries): interrupt alloc/enable, critical sections, semaphores, queues,
  mutexes, task create/delete, malloc/free, MAC from efuse, rand, sleep
  hooks, coex hooks, `get_time_us`, `ets_delay_us`, `btdm_rom_table_ready`.
- `btdm_controller_init` creates one task, `btdm_controller_task`. After
  setup it is a loop: `queue_recv(btdm_queue, &msg, forever)` →
  `btdm_rw_run(msg)`.
- Interrupt sources: `BT_BB` (5), `RWBT` (7), `RWBLE` (8), routed through the
  interrupt matrix (`interrupt_handler_set(5|8, …)` in esp-radio).
- Also needed: BT/modem clocks, PHY init (`esp-phy`: `register_chipv7_phy`
  with init data), low-power clock selection (`btdm_lpclk_select_src/set_div`),
  `coex_pti_v2()`.

## Design decisions

- **Controller task: a crude coroutine.** One extra stack in the kernel. When
  the task blocks on its queue/semaphore with nothing available, switch back to
  the kernel main loop; resume it when an ISR or an HCI send posts to it. Only
  what this one task needs, no general threading.
- **Deferred interrupts.** The BT sources go through Tock's normal path (the
  trap handler masks and records them, the main loop services them), like
  every other C3 peripheral. Revisit only if it demonstrably breaks timing.
- **HCI delivery straight into userspace** via `StreamingProcessSlice`
  (double-buffered, atomic swap), reusing the nRF `HciDriver` interface so
  the libtock-c NimBLE port runs unchanged. No kernel RX ring: a packet that
  does not fit in the process' slice is dropped, which is the app's fault.
- **Execute in place from flash** (ROM direct boot, flash mapped 1:1 at
  `0x42000000`), like `esp32-c3-devkitM-1`. The archives' `.iram1` code and
  the OS-adapter functions ESP-IDF marks `IRAM_ATTR` run from SRAM.

## Plan / progress

- [x] Study nRF experiment, esp-radio port, blobs; trial link; read ROM layout
- [x] Rebase `tock` and `libtock-c` onto master; fix the `mynewt-nimble` symlink
- [x] **1. Baseline**: stock kernel + `c_hello` flash and run
- [x] **2. Firmware crate** `tock-firmware/esp32c3-bt` (branch `esp32c3-bt`)
- [x] **3. Board crate** `boards/esp32-c3-devkitM-1-bt`: controller init and
      enable; `HCI_Reset` round-trips from inside the kernel
- [x] **4. HCI driver** on `0x30001` over VHCI
- [x] **5. NimBLE app**: `nimble_bleprph` syncs (public address
      `d8:85:ac:20:b1:7e`, base MAC + 2) and advertises as "Tock-NimBLE"; the
      controller services ~30 radio interrupts/s, consistent with the 30-60 ms
      advertising interval. Not yet confirmed on air.
- [x] **6. On air** (host: the Raspberry Pi 5's built-in adapter, BlueZ +
      `bleak`): advertising seen, connect, MTU 256, GATT read/write/subscribe,
      disconnect and re-advertise; 60 s connection with 153 write+read round
      trips, no errors, median 190 ms (4 connection events at 48.75 ms).
- [x] Execute in place from flash; NimBLE built with all GAP roles; scanning
      and DTM enabled in the controller; process console back.
- [x] NimBLE security manager (mbedtls): LE Secure Connections Just Works
      pairing and bonded reconnect verified with the Pi. Costs ~49 KB of app
      code (~33 KB mbedtls, ~15 KB security manager) and ~0.5 KB RAM.
- [ ] Cleanup: `### Safety` comments on `unsafe`, `make clippy`, drop the
      `c_variadic` feature or get it accepted (see below)

## Build and run

```
make -C boards/esp32-c3-devkitM-1-bt
make -C ../libtock-c/nimble           # app builds do not rebuild the library
make -C ../libtock-c/examples/nimble_bleprph
cd boards/esp32-c3-devkitM-1-bt
make init                             # once
tockloader install --local-board ../../../libtock-c/examples/nimble_bleprph/build/nimble_bleprph.tab
make flash
```

Apps use libtock-c's standard ESP32-C3 targets. Console:
`/dev/ttyUSB0` (check `/dev/serial/by-id/`), 115200. Set `TRACE` in
`hci_driver.rs` to log all HCI traffic.

## How it works

- `bt.rs`: OS adapter table (`OsiFuncs`, layout from ESP-IDF master
  `bt.c`), controller config (`esp_bt_controller_config_t` v0x02509280),
  bring-up, BT interrupt dispatch, VHCI callbacks.
  - Waits on a semaphore/queue from the task block the coroutine; posts
    from anywhere schedule a deferred call that resumes it. A wait from
    kernel context (only `btdm_controller_init`, which waits for the task's
    init) runs the task and the BT interrupts in place. A timed wait on the
    task re-arms the deferred call each pass (polling); none observed yet.
  - The controller registers one interrupt, source 8 (RWBLE), mapped 1:1
    to CPU line 8 and serviced through Tock's deferred path.
- `task.rs`: the coroutine (callee-saved register switch, painted 6 KiB
  stack in .bss above the kernel stack, as the trap handler treats
  `sp <= _sstack` as overflow).
- `heap.rs`: first-fit arena for the controller's `malloc`.
- `printf.rs`: the archives' `ets_printf`/`phy_printf` → `debug!`. ROM code
  still prints straight to UART0 (e.g. its "BLE assert" messages).
- `layout.ld`: flash layout as in `esp32-c3-devkitM-1`; `.iram1` runs from
  SRAM through its instruction-bus alias (`0x40380000`, 32 KiB), `.dram1` is
  initialised data in RAM. `bt::load_sections` copies both from flash first
  thing in `setup()`, before any interrupt can reach `bt::service_interrupt`
  (itself in `.iram1`).

Bring-up order (ESP-IDF): power up modem domain + clocks + modem reset →
`btdm_controller_rom_data_init` → `btdm_osi_funcs_register` → LP clock
(XTAL /40) → `btdm_controller_init` → PHY (`register_chipv7_phy`, full
calibration) → `btdm_controller_enable(BLE)` → `coex_pti_v2` →
`API_vhci_host_register_callback`.

Controller features: everything esp-radio enables (legacy + extended
advertising and scanning, DTM, encryption, connections).

## Memory (measured)

| Region | Range (physical) | Used |
|---|---|---|
| rom (flash) | `0x42000008-0x42040000` (256 KiB) | ~206 KiB: kernel, blobs, and the load images of `.iram1`/`.dram1` |
| iram (SRAM) | `0x40380000-0x40388000` (32 KiB) | `.iram1` ~26.6 KiB |
| prog (flash) | `0x42040000-0x42400000` (3.75 MiB) | |
| ram (SRAM) | `0x3FC88000-0x3FCDF000` (348 KiB) | kernel ~49 KiB (controller heap 32 KiB, peak 24.1 KiB after enable; task stack 6 KiB); app memory from `0x3FC94928`, first app linked for `0x3FCA0000` |


## Findings / pitfalls

- **Modem clocks**: `SYSTEM_WIFI_CLK_EN` must get the full `0x00FB9FCF`
  mask; the BT baseband (bit 11) and LC (bits 16, 17) clocks are off out of
  reset. Without them: `BLE assert emi.c 164, param 0 0x1000` (exchange
  memory mapping).
- **ROM CPU frequency**: call `ets_update_cpu_frequency(160)`. The ROM
  assumes 40 MHz, so every `ets_delay_us` (PHY calibration, controller) was
  4x short, and the link layer stalled after its first couple of events.
- **`connect_en`** is ESP-IDF's `BT_CTRL_BLE_MASTER`, "BLE connection
  feature", not just the central role. Without it, LE Set Advertising
  Parameters for connectable advertising fails with 0x12.
- The C3 core has no `mie` CSR (illegal instruction); interrupts are masked
  in the interrupt matrix.
- The controller may read a sent HCI packet until it calls
  `notify_host_send_available`, so the TX buffer is static and one packet is
  in flight at a time (BUSY otherwise).
- Tock's build script requires `INCLUDE`s in `layout.ld` to be local files;
  the ROM scripts are passed with `-T` from the board's `build.rs`, from the
  firmware crate's `links` metadata (`DEP_ESP32C3_BT_LD`).
- `ets_printf` in the vendored `esp32c3.rom.ld` is a `PROVIDE` so the board
  can override it.
- **NimBLE port, receive path**: `tock_npl_pump()` must drain HCI after its
  `yield()`, not only before. Otherwise a packet that arrives during the yield
  queues work that waits for the next 1 s timeout: every ATT response took
  ~1 s.
- `make clippy` fails in this board's `build.rs` (host build has no
  `DEP_ESP32C3_BT_LD`).

## Deviations from Tock rules (AGENTS.md)

- `#![feature(c_variadic)]` in the board crate, for formatting the archives'
  `printf` arguments. Tock does not allow new unstable features. The
  alternative is a non-variadic `ets_printf`/`phy_printf` that only logs the
  format string.
- `unsafe` blocks lack `### Safety` comments so far.
- Linking a binary blob (fine for this experiment, as with the nRF board).

## Open questions

- Which libbtdm_app version matches this ROM (ECO7)? Using the
  esp-wifi-sys-esp32c3 0.3.0 blobs (= esp32c3-bt-lib 0a08c4b) + esp-hal's
  ROM script set (ECO3, no ECO7 scripts). Works so far.
- Heap/stack under connections; whether timed task waits occur.
- Latency of deferred interrupt servicing under load (e.g. while a process
  is busy) and whether the controller tolerates it.

## Log

- 2026-10-06: Initial study and plan. Rebased both repos (clean).
- 2026-10-06: Steps 1-5. Controller up after fixing modem clocks; HCI_Reset
  round-trips; NimBLE syncs; advertising runs after fixing the ROM's CPU
  frequency and enabling connections.
- 2026-10-06: Execute in place from flash; all controller features and NimBLE
  roles; on-air verified with the Pi's adapter; fixed the NimBLE port's 1 s
  receive latency.
- 2026-10-06: NimBLE security manager with mbedtls; pairing and bonding
  verified on air. mynewt-nimble and mbedtls are libtock-c submodules.
