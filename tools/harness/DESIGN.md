# Tock CI Harness — Design Decisions

Internal design record for `tock-harness` (`tools/harness`). Documents what
has been settled, not the reasoning behind it. This is a decisions record,
not a tutorial or a pitch.

## 1. Scope

- Replaces the QEMU (`qemu-virt-ci-runner`, `qemu-runner`) and LiteX
  (`litex-ci-runner`) CI runners with a single harness supporting both
  virtual and physical boards.
- The external `tock-hardware-ci` Python repository and `treadmill-ci.yml`
  are disregarded. This harness is a from-scratch replacement, not built to
  interoperate with that system.
- The harness implements three operations: `plan`, `prebuild`, `run`. It
  does not implement the GitHub check/PR integration or the `ci-bridge`
  service described in `Treadmill_CI.md`; those are the responsibility of
  external tooling that consumes the harness's output.
- A fourth, `treadmill`, runs a prebuilt plan, or a subset of it, on a
  Treadmill job, emulating what the `ci-bridge` does for a single job: it
  drives the user's `tml` CLI to create a job whose host can run the
  selected tests, upload the plan and its prebuilt artifacts, run `run` in
  it, and download `results.json`. It never plans or builds, and talks to
  Treadmill only through `tml`. (Later, it may split a plan into the set
  of jobs that covers it.)
- MVP board set: `qemu_rv64_virt` (virtual), the nRF52840DK and the
  NUCLEO-F429ZI (physical, attached via USB/SWD to the host running the
  harness).
- MVP requirement set: `Uart` and `Gpio` only. Host-level requirements not
  tied to a specific DUT (e.g. an 802.15.4 sniffer attached to a Treadmill
  host) are out of scope; `Requirement` only models DUT-scoped resources.

## 2. Three-Phase Architecture

| Phase      | Input                                   | Output                                   |
|------------|------------------------------------------|-------------------------------------------|
| `plan`     | the compiled-in test registry (`tests.rs`) | `plan.json` (a `Plan`)                   |
| `prebuild` | a `Plan`, a libtock-c checkout             | built kernels/apps, `manifest.json`, optional archive |
| `run`      | a `Plan`, a `Manifest`, optionally a host-spec | `results.json` + human-readable stdout log |

- `plan` requires no hardware and no privileged access. It is a pure
  function of the compiled-in `TESTS` registry.
- `prebuild` cross-compiles kernels and userspace apps. It does not touch
  any board.
- `run` is the only phase that touches hardware (real or simulated).
- `plan`, `prebuild`, and `run` are subcommands of the same `tock-harness`
  binary, built from the same commit. `run` does not need to deserialize
  test logic (closures, `Requirement` lists) from the `Plan` — it looks
  those up directly from the linked-in `TESTS` registry by test ID.

### 2.1 No job/grouping abstraction

- `Plan` has no `Job` type. It is a flat list of `PlannedTest` entries, one
  per `(test, board)` pair.
- Grouping `PlannedTest`s into schedulable Treadmill jobs (by identical
  requirement signature, so a job can be placed on any host whose
  capabilities match) is done by tooling outside the harness, not by
  `plan`.
- On a given host, `run` attempts every `PlannedTest` it can satisfy
  (matching board kind and, for physical boards, matching against the
  local host-spec) and reports a per-test outcome. External tooling
  determines whether a Treadmill job's assigned test set is fully covered
  and cancels redundant jobs accordingly. The harness itself has no
  concept of "this job is done."

## 3. Plan / Artifact Data Model

```rust
type Label = String; // e.g. "libtock-c-app-c_hello-3f2a9c1e", see below

#[serde(tag = "type", rename_all = "kebab-case")]
enum BuildSpec {
    TockKernel { board: BoardKind },                          // "tock-kernel"
    LibtockCApp { name: String, tock_targets: Vec<String> },  // "libtock-c-app"
}

struct PlannedTest {
    id: String,           // "{test_id}@{board}", e.g. "hello_world@nrf52840dk"
    board: BoardKind,
    kernel: Label,
    apps: Vec<Label>,
}

struct UnsupportedTest {
    id: String,           // as PlannedTest.id
    board: BoardKind,
    reason: String,
}

struct Plan {
    artifacts: BTreeMap<Label, BuildSpec>,
    tests: Vec<PlannedTest>,
    unsupported: Vec<UnsupportedTest>,
}
```

- A `(test, board)` pair listed in the test's `unsupported` (a board of its
  `boards` that has what it `requires`, but on which it cannot pass, e.g.
  for a feature the board's kernel lacks or a known bug; the reason is free
  text) is not planned: `plan` records it in `Plan.unsupported` instead,
  which selection filters like `tests`. A selector matching only unsupported
  tests fails with their reasons. `run` reports each as `NotApplicable`.
- `Plan` does **not** serialize `Requirement`s. They live only in the
  compiled `TestCase` registry, matched at `run` time by `PlannedTest.id`.
- A test naming multiple candidate boards (`TestCase.boards`) expands into
  one independent `PlannedTest` per board during `plan::compute()`. Each
  such entry gets its own `kernel` label (kernels are never shared across
  board kinds).
- Each artifact is one build result, described by its `type` and that
  type's configuration. An app is built for its board's `tock_targets`, so
  boards sharing targets (e.g. `cortex-m4`) share the app's TAB.
- **Label derivation**: `label_of(spec)` = a readable name
  (`tock-kernel-<board>`, `libtock-c-app-<name>`, sanitized to
  `[A-Za-z0-9._-]` so it is a file name) + `-` + the first 8 hex digits of
  the SHA-256 of `serde_json::to_vec(spec)`. Deterministic across
  machines/toolchains (canonical JSON field order from the struct
  definition, not a process-local hash like `DefaultHasher`); the hash
  keeps specs sharing a readable name apart. `plan` asserts that no label
  maps to two different specs.
- **Deduplication is automatic and structural**: two `PlannedTest`s that
  need the identical `BuildSpec` (e.g. the same kernel config, or the same
  app) collapse to the identical `Label` in `Plan.artifacts` with no
  coordination required from test authors. Labels are never hand-chosen.
- `prebuild` produces a `Manifest = BTreeMap<Label, PathBuf>`, written as
  `manifest.json` inside the output directory, mapping each label to its
  built file (`<label>.bin` for kernels, `<label>.tab` for apps). Paths are
  relative to the manifest's own directory, so the output directory can be
  archived and unpacked elsewhere.
- `prebuild` also builds the harness itself as a static binary for each
  Treadmill host architecture (`tock-harness-x86_64`,
  `tock-harness-aarch64`, targets `*-unknown-linux-musl`), by invoking
  `cargo build --target` on its own package. These link with the
  toolchain's `rust-lld` (`tools/harness/.cargo/config.toml`), so no
  system cross toolchain is needed. Cargo artifact dependencies
  (`-Z bindeps`) were rejected: a package cannot depend on itself, nor on
  one package for two targets.
- `prebuild` optionally tars the output directory into a single archive
  file (`--archive <path>`) for transfer to a different machine. On the
  same machine, `run` can reference the manifest/output directory directly
  with no archive step.

## 4. Test Model

```rust
struct TestCase {
    id: &'static str,
    boards: &'static [BoardKind],
    requires: &'static [Requirement],
    apps: &'static [&'static str],
    unsupported: &'static [(BoardKind, &'static str)],
    body: TestBody,
}

enum TestBody {
    UartSequence { needles: &'static [&'static str], timeout: Duration },
    Run { run: fn(&mut TestCtx) -> Result<(), String>, time_limit: Duration },
}

enum Requirement {
    Uart,
    Gpio { name: &'static str, mode: GpioMode },
}
```

- GPIO tests observe DUT outputs by sampling inputs and comparing edge
  times, not levels, so they do not depend on active-high/low wiring.
  Active-low buttons are pressed by driving the line low and released by
  switching it back to an input, leaving the DUT's pull-up to raise it:
  the host never drives a line high into a DUT that may use a lower I/O
  voltage.
- `TestCase`s live in a `const TESTS: &[TestCase]` registry
  (`tests.rs`), analogous in style to the existing
  `qemu-virt-ci-runner`'s `const TESTS: &[TestCase]`.
- `TestBody::UartSequence` is the "generator" form for the simple case
  (wait for literal substrings on the console, in order, each within
  `timeout`). `Uart::wait_for` consumes console output up to and including
  the match and returns it, so successive waits never re-match old output.
  `TestBody::Run` takes a plain `fn` pointer; a non-capturing closure
  coerces to `fn` and satisfies this, so tests may be written as closures
  in practice.
- Every body has a time limit (`TestBody::time_limit`), the longest it may
  take with all of its waits timing out: a `UartSequence`'s is its
  `timeout` per needle; a `Run` declares its `time_limit`, derived from the
  waits in its body (`timeouts(n, extra_ms)`: `n` `TIMEOUT`s plus other
  waits). `run` fails a test whose body passes but takes longer than its
  limit, as `treadmill` leases jobs by the limits.
- `TestCtx` wraps `&mut dyn Board` plus the test's `requires` slice.
  `TestCtx::uart()` and `TestCtx::gpio(name)` each assert, at call time,
  that the resource they expose was declared in `requires`. Calling an
  undeclared resource accessor panics.
  - This is a **runtime** check, not a compile-time/type-level one. A
    macro/typestate-based static enforcement approach was considered and
    rejected in favor of this in the interest of keeping test code free of
    macros and easy to read.
  - The intended mitigation for catching this class of bug before hardware
    is involved: a CI job that executes every `TestCase.body` against a
    no-op mock `Board` implementation, so an undeclared-resource panic
    surfaces on ordinary CI hardware, with no board or QEMU required.
- `Requirement` matching is split into two distinct, separately-implemented
  concerns:
  1. **Self-consistency** (test vs. its own declaration): enforced by the
     `TestCtx` runtime assertion described above.
  2. **Requirement vs. host/DUT capability** (test vs. a specific host):
     a data-matching problem against the host-spec, used to decide whether
     a `PlannedTest` is runnable on a given host/DUT. **Not implemented in
     the current sketch** — see §8.

## 5. Host Spec

- Canonical location on a Treadmill host: `/run/tml/host-spec.json`.
- `run` resolves the host-spec in this order:
  1. `--host-spec <path>` if given.
  2. Otherwise `/run/tml/host-spec.json`, if it exists.
  3. Otherwise `None` (no host-spec).
  - `--no-host-spec` forces `None` even if the canonical file exists.
- With no host-spec, only backends that do not require one (currently:
  `QemuVirt`) are usable. GPIO and any other host-spec-dependent
  capability are never available without a host-spec file — this applies
  equally to local/interactive use (a hand-authored override file is
  required to exercise GPIO locally; there is no auto-discovery fallback
  for it).
- Schema (fields settled so far):

```jsonc
{
  "duts": [
    {
      "board": "nrf52840dk",
      "debug": {
        "protocol": "swd",
        "probe": {
          "vendor": "Segger",
          "model": "J-Link",
          "serial": "000683188086"
        }
      },
      "console": {
        "kind": "uart",
        "device": "/dev/serial/by-id/usb-SEGGER_J-Link_000683188086-if00",
        "baud": 115200
      },
      "gpio": {
        "P0.12": { "type": "sysfs-gpio", "gpioN": 17, "modes": ["digital_in", "digital_out"] }
      }
    }
  ]
}
```

- `gpio` is a new addition designed as part of this work; it is keyed by
  schematic/silkscreen pin name (e.g. `"P0.12"`), not by the harness's
  logical resource name (e.g. `"led0"`).
- Each `gpio` entry is tagged by `type`, which selects the host-side
  driver; the remaining fields are defined by that driver's module
  (`sysfs-gpio`: `gpioN`, the line number under `/sys/class/gpio`, and
  `modes`, the directions the wiring supports). An entry of an unknown
  `type` still parses, but provides no GPIO.
- `console.device` is a stable path
  (`/dev/serial/by-id/usb-SEGGER_..._<probe-serial>-if00`), sufficient for
  the harness to open the UART directly with no separate discovery step.
- The mapping from a harness-internal logical resource name (`"led0"`,
  `"button0"`) to a host-spec schematic pin name (`"P0.13"`) is owned by
  the harness's per-board backend code, not by the host-spec. These names
  are Tock/harness concepts, not board-vendor or Treadmill concepts.

## 6. Board Backends

`BoardKind`: `QemuVirt`, `Nrf52840Dk`, `NucleoF429zi`. Boards are named
by `BoardKind::name()` everywhere outside the Rust code (plans, test ids,
`--board`, job labels): the Tock board directory name, which is also the
host-spec DUT `board` (`qemu_rv64_virt`, `nrf52840dk`, `nucleo_f429zi`,
...). (The trait/
composition design generalizes to more without changes to `Board`,
`Uart`, or `Gpio`.)

### 6.1 `QemuVirt`

- Always constructible; does not consult the host-spec.
- Flashing: `tockloader flash --flash-file <tmp> --board qemu_rv64_virt
  --address 0x80000000 <kernel.bin>`, then `tockloader install
  --flash-file <tmp> --board qemu_rv64_virt <app.tab>` per app — mirrors
  the existing runners' use of a flash-file rather than a real device.
- Execution: spawns `qemu-system-riscv64 -machine virt -bios <flash-file>`
  with piped, non-blocking stdout as the UART transport (no QMP, no TCP
  serial socket — simplified relative to `qemu-virt-ci-runner` since the
  MVP test needs no key injection or screenshot comparison).
- `Board::gpio()` always returns `None`. No virtual GPIO backend exists.
- `Board::reset()` kills and waits on the child process (not a
  process-group kill, unlike the existing `qemu-virt-ci-runner`).

### 6.2 `Nrf52840Dk`

- Constructed only when a host-spec is present and contains a `DutSpec`
  with `board == "nrf52840dk"`.
- Flashing: `tockloader flash --board nrf52dk --openocd
  [--openocd-serial-number <probe.serial>] --address 0x00000 <kernel.bin>`,
  then a single `tockloader install --erase --board nrf52dk --openocd
  [--openocd-serial-number <probe.serial>] <app.tab>...`. The console is
  then opened (input flushed) and the board reset via `openocd ... -c
  "init; reset run; exit"`, so boot output is never missed.
- **Decision: keep using `tockloader` (subprocess) rather than calling
  `probe-rs` as a library.** Reasons on record: tockloader already
  performs image assembly; tockloader itself uses `probe-rs` as one of its
  own backends; not all currently-targeted boards support `probe-rs`, so
  depending on it directly would narrow hardware coverage. This means the
  harness binary is not fully self-contained/independent of the board —
  accepted trade-off.
- UART: opens `dut.console.device` at `dut.console.baud` via the generic
  `SerialUart` driver (built on the `serialport` crate).
- GPIO: `gpio(name)` looks up `name` in a harness-owned `PIN_MAP` constant
  (`led0`/`led1` → `P0.13`/`P0.14`, `button0`/`button1` → `P0.11`/`P0.12`,
  `gpio0` → `P1.01`), then looks up that schematic name in `dut.gpio`, then
  opens the driver for that entry via `boards::open_gpio`. Logical names are
  indices into the corresponding Tock driver (LED, button, userspace GPIO).
  Opened pins are released by `Board::reset()`.
- APPROTECT: before flashing, the CTRL-AP `APPROTECTSTATUS` register is
  read; if debug access is locked, the chip is mass-erased and unlocked
  via OpenOCD's `nrf52_recover`, and the test fails if it is still locked
  afterwards. The Tock kernel keeps APPROTECT disabled once flashed.
- Reset/recovery: a hard reset via the debug probe is attempted around
  flashing. Recovering a fully wedged board is explicitly out of scope for
  the harness — handled out-of-band via USB power-cycling by something
  other than the harness.

### 6.3 `NucleoF429zi`

- Constructed only when a host-spec is present and contains a `DutSpec`
  with `board == "nucleo_f429zi"`. Debug probe is the on-board ST-LINK/V2-1;
  `probe.serial` is its USB serial string.
- Flashing (temporary workaround): the image is assembled off-target with
  `tockloader flash --flash-file <tmp> --board nucleof4 --address 0x08000000
  <kernel.bin>` and `tockloader install --flash-file <tmp> --board nucleof4
  <app.tab>...`. The kernel `rom` + app `prog` window (`0x08000000`, 512 KiB,
  0xFF-padded) is sliced out of the flash file and written with a single
  `openocd ... -c "program <image.bin> verify 0x08000000"`. The console is
  then opened and the board reset via `openocd ... -c "init; reset run"`.
  - `tockloader install --openocd` is not used because it silently loses
    apps on STM32F4: it read-modify-writes in 2 KiB `page_size` blocks, but
    OpenOCD's `program` erases whole flash sectors (up to 128 KiB), so the
    trailing padding write erases the app just written. To be replaced
    with regular tockloader flashing once that is fixed upstream.
- All direct OpenOCD invocations use `reset_config srst_only srst_nogate
  connect_assert_srst`, so attaching does not depend on the currently
  flashed firmware: a plain attach to firmware sleeping without
  debug-in-sleep (`DBGMCU_CR.DBG_SLEEP`) fails examination. OpenOCD's
  `stm32f4x.cfg` sets `DBGMCU_CR` on examine, which persists until
  power-on reset, so this only shows up after a power cycle or with
  foreign firmware.
- UART: `USART3` (PD8/PD9) is routed to the ST-LINK virtual COM port, which
  appears as `/dev/serial/by-id/usb-STMicroelectronics_STM32_STLink_<probe-serial>-if02`.
- GPIO: `PIN_MAP` maps `led0`/`led1` → `PB0`/`PB7` (LD1/LD2), `button0` →
  `PC13` (B1) and `gpio0` → `PG9` (Arduino D0), then opens the host-spec
  entry via `boards::open_gpio` like the nRF52840DK backend. B1 is
  active-high, so the active-low `press`/`release` test helpers do not apply
  to it; the GPIO tests currently target only the nRF52840DK.

### 6.3a `Esp32C3DevkitM1`

- Board name `esp32-c3-devkitm-1`: lowercase like host-spec board names,
  unlike its Tock board directory (`boards/esp32-c3-devkitM-1`).
- The kernel and apps run from SRAM, which the ROM loads from an ESP image
  at flash offset 0; there is no second-stage bootloader. Flashing assembles
  the kernel `rom` + app `prog` window (`0x40380000`-`0x403E0000`) with
  `tockloader flash`/`install --flash-file --board esp32-c3-devkitm-1` (the
  board's `flash_address` in tockloader), appends 256 bytes of `0xff` so the
  kernel finds no stale app in SRAM (which survives resets) behind the new
  ones, and writes it as a single-segment ESP image (`esp_image`, equal to
  `esptool elf2image --dont-append-digest` output) with `esptool write-flash
  --after no-reset`.
- Flashing and the console share the USB-UART bridge (CP2102N), the
  host-spec console. The ROM's UART download mode is always available
  (unless eFuses disable it), so flashing cannot brick the board. The board
  is reset by the bridge's RTS (EN) with DTR (GPIO9, boot mode) released,
  once the console is open, so no boot output is lost; `restart` does the
  same.
- Apps use libtock-c's `ESP32_C3_TOCK_TARGETS` (fixed addresses in the
  kernel's `prog` and app memory regions). No GPIO.

### 6.4 Shared physical-board drivers

- `SysfsGpio` (`boards/sysfs_gpio.rs`): generic Linux `sysfs`-based GPIO
  driver, constructed from a `sysfs-gpio` host-spec entry (`SysfsGpioSpec`,
  defined in the same module). Exports `/sys/class/gpio/gpio<N>`, sets
  `direction`, reads/writes `value`, and rejects modes not listed in the
  entry's `modes`. On drop, the line is returned to input and unexported,
  so no test leaves a line driven. Not specific to the nRF52840DK.
- `SerialUart` (`boards/serial_uart.rs`): generic UART-over-serial-port
  driver using the `serialport` crate, opened against a device path and
  baud rate. Not specific to the nRF52840DK.

### 6.5 Reflash policy

- Apps are re-installed per test (`tockloader install`/erase cycle); this
  is treated as effectively a reflash per test, though `tockloader` does
  not necessarily rewrite the entire flash image if unchanged.
- Multiple identical DUTs of the same `BoardKind` on one host: `run` picks
  one DUT per test in round-robin order (`next_dut[kind] % duts.len()`) to
  spread flash wear, rather than running the test against every matching
  DUT. This state is an in-memory counter local to one `run` invocation —
  it does not persist across separate invocations or across hosts.

## 7. CLI Surface

```
tock-harness plan
    [--test <test|alltests>@<board|allboards|anyboard>]...
    [--out <path=plan.json>]

tock-harness prebuild
    --plan <path>
    --libtock-c <path>
    [--out-dir <path=artifacts>]
    [--archive <path>]

tock-harness run
    --plan <path>
    --manifest <path>
    [--host-spec <path> | --no-host-spec]
    [--test <test|alltests>@<board|allboards|anyboard>]...
    [--out <path=results.json>]
    [--logs <dir=logs>]

tock-harness treadmill
    --plan <path>
    --manifest <path>
    [--test <test|alltests>@<board|allboards|anyboard>]...
    [--job <uuid> | --new-job]
    [--keep]
    [--lease <duration>]
    [--out <path=results.json>]
    [--logs <dir=logs>]
```

- `plan` includes the planned tests any `--test` selects; selectors are
  cumulative, and a bare `plan` plans every test on every board. A selector
  is `<test>@<board>`, where the test may be `alltests` and the board
  `allboards`, or `anyboard` for one board the test runs on: one that the
  other selected tests already use if possible (fewer kernels and hosts),
  otherwise the first of the test's boards. `anyboard` adds nothing for a
  test another selector already includes. Unknown tests or boards are
  argument errors; a selector matching no planned test is an error.
- `treadmill` selects a subset of the given (prebuilt) plan with the same
  selectors (none: the whole plan), which must be tests of one physical
  board, and prints which tests it runs and which of the plan it skips. It
  uploads the manifest's directory (as written by `prebuild`, including the
  `tock-harness-<arch>` binaries) and the selected part of the plan, so
  the remote `run` needs no `--test`. It
  derives the job's host predicate from the tests' `requires`: for each
  test, `host.duts.exists(d, d.board == "<board>" && ...)`, joined with
  `&&` (deduplicated), so the host has, for every test, some DUT that can
  run it. `Requirement::cel` renders a requirement as a condition on `d`:
  `Uart` as `has(d.console)`, `Gpio` as the pin (via the backend's
  `PIN_MAP`, `BoardKind::pin`) being present in `d.gpio`, supporting the
  mode, and on a `linux-gpiochip` controller (what `boards::open_gpio`
  drives). A GPIO the backend doesn't map renders as `false`.
- `treadmill` reuses jobs: rather than booting a job per run, it claims an
  idle job of an earlier run whose host can run the selected tests, and
  creates one only if there is none (or with `--new-job`). Jobs it creates
  are reclaimable (`tml job create --reclaimable`: past its lease, a job
  keeps running until Treadmill needs its host for another job) and carry
  the annotation `tock-harness.pool=<image set>`. A run holds its job with
  the annotation `tock-harness.claim=<user>@<host>, pid <pid>` and a lease
  from now, and releases it when it ends, also on failure, by ending the
  lease now and removing the claim.
  - The lease (unless `--lease` gives one) covers setting up, then each
    test's time limit plus a constant 30 s: the time limits already assume
    every wait times out, so the constant covers flashing and resetting the
    board (10-15 s on the nRF52840DK hosts) and slack. Setting up takes 2
    minutes in a reused job (upload, tool check), and 8 in a new one, whose
    lease runs from its dispatch (boot, tool installation).
  - Jobs are labeled `tock-harness - <description>`, and `tock-harness (run
    <n>) - <description>` when reused for their `n`th run, where the
    description names the tests (as many as fit into Treadmill's 256
    characters; labels may not contain `:`). The annotation
    `tock-harness.runs` counts a job's runs; claims increment it.
  - Candidates are the caller's own jobs (`tml job list --mine`; never a
    group's or a shared one, whose state is unknown) in the pool for this
    image set, `ready`, reclaimable, and unclaimed. A claim that outlived
    its run (e.g. a crash) keeps its job from being reused; its lease has
    lapsed, so Treadmill reclaims the host when a job needs it.
  - A candidate's host must have, for each test, a DUT of the board that
    satisfies all of its requirements: the host predicate of a new job,
    checked against the host's spec (`tml host show`) with
    `Requirement::satisfied_by`, which mirrors `Requirement::cel`.
  - Claiming is a conditional update (`tml job update --if-revision`) of
    the claim, the lease (`--lease-until now+<lease>`) and the label: of
    two runs claiming the same job, one wins and the other moves on to the
    next candidate. Nothing else decides who holds a job, so the claim's
    value only informs people.
  - Before running, the remote script stops any harness still running in
    the job (`pkill -f '^\./tock-harness-'`), e.g. of a run that lost its
    connection.
  - `--job` runs on an existing job as it is, without claiming, changing or
    releasing it. `--keep` keeps the claim and lease after the run, and
    prints how to release the job.
  - On failure, `treadmill` prints how to claim the job again for
    debugging, and how to resume it if Treadmill has reclaimed its host
    by then (a terminated job can be resumed on its host for 24 hours).
  - Exit code: that of the remote `run` (0/1), or 2 if Treadmill/SSH
    failed.
- Temporarily, until a Tock image overlay provides them, `treadmill`
  installs `tockloader` (at a pinned revision) and `esptool` (v5) in the job
  (with `pipx`) before running. The base images provide `openocd` and
  `pipx`. The remote output is mirrored to the job's serial console on a
  best-effort basis: a console failing writes does not fail the run.

- `run --test` takes the same selectors, applied to the given plan; if
  omitted, `run` attempts every `PlannedTest` in the plan.
- `run --logs` writes each test's console transcript, everything received
  since the board was flashed, to `<dir>/<planned test id>.log`.
  `treadmill` downloads the remote `run`'s logs into its own `--logs`.
- Where a board's console is (the host spec's, or e.g. the kernel's USB
  CDC-ACM device on a Pico) is a fixed property of its kernel's
  configuration, not an option: a backend may support several (`Rp2xxx`
  takes a `Console`), and each board kind picks one. A kernel configured
  otherwise is a board kind of its own.
- Arguments are parsed with `clap` (derive). `tock-harness help` and
  `<subcommand> --help` document every option.

## 8. Reporting

- `run` writes a JSON array to `results.json` (or `--out`):

```jsonc
[
  { "id": "hello_world@qemu_rv64_virt", "outcome": { "status": "Passed" } },
  { "id": "hello_world@nrf52840dk", "outcome": { "status": "NotApplicable", "reason": "no matching DUT on this host" } }
]
```

  `outcome.status` is one of `Passed`, `Failed` (with `reason`),
  `NotApplicable` (with `reason`).
- All output is logged (`log` + `env_logger`) to stderr: info messages
  plain, others behind their level (`warning: ...`). `-v` enables debug,
  `-vv` trace; `RUST_LOG` also works. `run` logs each test (`=== id ===`,
  flashing, `PASSED` / `FAILED: <reason>` / `SKIPPED: <reason>`) and its
  steps: every UART `write`/`wait_for` (provided methods of `Uart`, over the
  backends' `send`/`receive_until`), button presses, edge waits and
  recordings; GPIO mode changes and writes, and UART output, at debug.
- Tools the backends run (tockloader, openocd) go through `boards::run`,
  which logs their output at debug; a failing tool's error carries the
  last lines of its output instead.
- `treadmill` describes the host it needs in words (per distinct
  requirement set, e.g. `console, button0 at P0.11 (digital_out)`) and logs
  the CEL predicate only at debug. It forwards everything `tml` prints, and
  the remote `run`'s output as `job | ...` lines; with `-v`, the remote
  `run` is verbose too. On the job, the remote output is also `tee`d to the
  serial console Treadmill records (the last non-VT entry of
  `/sys/class/tty/console/active`, e.g. `ttyS0`, `ttyAMA10`), if the job's
  user can write it.
- `run` starts with an empty line, so that it starts on a line of its own
  on a console left mid-line, and a banner: the harness version, the time
  (UTC), the Treadmill job (from `/run/tml/job-id`) and how `treadmill`
  came by it (`--job-origin fresh|reused|given`), the host (its spec's name
  and architecture), who launched the run (`--launched-by`), and the plan's
  summary. `treadmill` passes both options, which are hidden from `--help`.
  A prebuilt directory is therefore tied to the harness that built it:
  rerun `prebuild` after updating the harness.
- `run` ends with a table (`comfy-table`, 80 columns, long cells
  wrapped) of each test's status and the first line of its reason; under
  `treadmill` it arrives with the forwarded job output (and on the serial
  console).
- `treadmill` terminates the job it holds when interrupted (Ctrl-C,
  SIGTERM, SIGHUP; `ctrlc`), after setting its exit status to `failure` /
  `interrupted`, and exits with 130: the remote `run` may still hold its
  boards. It logs why it skips an idle job (its host's DUTs cannot run the
  tests; another run claimed it first).
- Process exit code is non-zero if any test's outcome is `Failed`.
  `NotApplicable` does not affect the exit code.
- Inside a Treadmill job (detected by `/run/tml/job-id`), `run` records
  each run in `~/.tock-harness-runs.jsonl` (outside the directory each
  `treadmill` run replaces), and sets the job's exit status from all of
  them via `tml job set-exit-status`: `failure` if any run failed, else
  `success`, with a summary of the latest run and, once there are several,
  of all (e.g. `run 5: 2 passed, 0 failed, 0 skipped; 5 runs on this job: 4
  passed, 1 failed (run 3)`). `treadmill` numbers its runs (`--run`). A
  failure to report is a warning and does not change the exit code.

## 9. Repository Layout and Dependencies

- New crate at `tools/harness`, package name `tock-harness`.
- Added to `tools/Cargo.toml`'s `[workspace] members` (the `tools/`
  directory is excluded from the root Tock workspace and forms its own
  independent Cargo workspace, per existing convention — same as
  `qemu-virt-ci-runner`, `litex-ci-runner`, etc.).
- Dependencies: `serde` (derive), `serde_json`, `sha2`, `tempfile`, `nix`
  (feature `fs`, used only for `fcntl`/`O_NONBLOCK` on the QEMU backend's
  piped stdout), `serialport` (new to this repository — not used by any
  existing tool; required for real termios-configured UART access on the
  physical backend), `gpiocdev`, `clap` (derive).
- The harness links no system libraries: `serialport` is built without
  its default `libudev` feature (devices are opened by path, never
  enumerated), so it builds as a fully static musl binary.
- File layout:

```
tools/harness/
  Cargo.toml
  DESIGN.md
  src/
    main.rs        # CLI dispatch (plan/prebuild/run)
    board.rs        # Board/Uart/Gpio traits, BoardKind, GpioMode
    requirement.rs  # Requirement enum
    testcase.rs     # TestCase, TestBody, TestCtx
    tests.rs        # const TESTS registry
    plan.rs         # Plan, PlannedTest, BuildSpec, Label, label_of, compute
    build.rs        # prebuild(), Manifest, per-board build glue
    hostspec.rs      # HostSpec, DutSpec, DebugSpec, ProbeSpec, ConsoleSpec, GpioPinSpec
    run.rs          # run(), TestOutcome, TestReport, make_board
    boards/
      mod.rs        # open_gpio: host-spec GPIO entry -> driver
      qemu_virt.rs
      nrf52840dk.rs
      nucleo_f429zi.rs
      sysfs_gpio.rs
      serial_uart.rs
```

## 10. Explicitly Deferred / Not Implemented

These are known gaps in the current state of the harness, not omissions
from this document:

- **Requirement-vs-host-capability preflight matching** (§4, item 2) is
  unimplemented. A `PlannedTest` whose `Requirement` a board cannot
  satisfy is currently only discovered when the test body calls the
  corresponding `TestCtx` accessor, which panics rather than producing a
  clean `NotApplicable` outcome.
- **No panic isolation** around test body execution in `run`. A panic
  (e.g. from the case above, or from any `unwrap`/`expect` inside a test
  or backend) aborts the entire `run` invocation rather than being
  recorded as a single `Failed` test.
- `build.rs`'s target-triple/platform-name table for kernels is hardcoded
  per `BoardKind` rather than derived from each board's own Makefile.
- The `QemuVirt` backend's `tockloader --flash-file` invocation has not
  been verified against the installed `tockloader` version.
- Host-level requirements not tied to a specific DUT (e.g. a sniffer
  device attached to the host rather than the board under test) are not
  modeled. `Requirement` covers DUT-scoped resources only.
- A test requiring more than one DUT simultaneously is not modeled. A
  `PlannedTest` binds to exactly one DUT.
- Virtual GPIO (a QEMU-backed `Gpio` implementation) does not exist.
  `QemuVirt::gpio()` always returns `None`.
- Treadmill job creation/scheduling, the GitHub check integration, and
  the `ci-bridge` service are out of scope for this harness entirely (see
  §1).

## 11. Prior Art Reviewed

Retained for context; none of this is depended on by the design above.

- `tools/ci/qemu-virt-ci-runner`: `const TESTS: &[TestCase]` with a
  `TestStep` enum (`WaitSerialInOrder`/`AnyOrder`, `Sleep`, `SendKey`,
  `SendSerial`), QMP for key injection and screendump-hash comparison, raw
  TCP serial socket, tockloader-based image assembly at run time.
- `tools/ci/qemu-runner`: older/simpler variant for `hifive1` and
  `opentitan/earlgrey-cw310`, using `rexpect` PTY + `exp_string`/
  `exp_regex`, no `TestCase` abstraction.
- `tools/ci/litex-ci-runner`: inline closures called sequentially (no
  registry, no `--test` selection), `rexpect` PTY for console I/O, a
  ZeroMQ-based `SimCtrl`/`GpioCtrl` client for simulated GPIO (sim-only,
  no physical-board equivalent).
- `tools/ci/board-runner`: per-board bespoke Rust files
  (`earlgrey_cw310.rs`, `esp32_c3.rs`, `artemis_nano.rs`), no shared
  trait or abstraction across boards.
- `tock-hardware-ci` (external Python repo) / `doc/TockHardwareCI.md`:
  class-hierarchy design (`BoardHarness`, `TestHarness`, `OneshotTest`
  subclasses). Disregarded per §1.
