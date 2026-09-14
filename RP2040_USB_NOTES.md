# RP2040 (Raspberry Pi Pico) USB CDC-ACM console failures in the CI harness

Notes from debugging the Pico's USB CDC-ACM console on the `pton-rpi2004`
Treadmill host (Raspberry Pi 5; Pico on USB port `3-2`, Raspberry Pi Debug
Probe on `3-1`), 2026-09-29. Kernel logs below are the host's `dmesg`; UART0
is the Pico's panic console, read through the Debug Probe.

Kernels (SHA-256 prefixes of `raspberry_pi_pico.bin`):

- **pre-PR**: `63a9b8a15a9dcfaa`, this branch before the merge of
  tock/tock#5175, with the Pico's console on USB CDC-ACM.
- **with-PR**: `3c0139c3983ca029`, this branch at `34c9d2374` (tock/tock#5175
  merged, console on USB CDC-ACM).

## 1. RP2040 USB: enumeration fails, and takes the host controller down

**Fixed by tock/tock#5175** (merged here as `aea0ec1f4`).

With the pre-PR kernel, the Pico's USB device fails to enumerate, and the
host's xHCI controller dies, taking the Debug Probe (on the same controller)
with it. The host then needs a USB bus reset (unbinding and rebinding
`xhci-hcd.1`) while the Pico is held in reset, or a power cycle with the Pico
in BOOTSEL mode, since it fails again on every boot.

Host boot, with a pre-PR kernel in the Pico's flash:

```
[    2.863102] usb 3-2: new full-speed USB device number 3 using xhci-hcd
[    2.987130] usb 3-2: device descriptor read/64, error -71
[    8.287122] xhci-hcd xhci-hcd.1: Timeout while waiting for setup device command
[   13.663143] xhci-hcd xhci-hcd.1: Timeout while waiting for setup device command
[   13.871105] usb 3-2: device not accepting address 3, error -62
[   29.554663] xhci-hcd xhci-hcd.1: Abort failed to stop command ring: -110
[   29.581562] xhci-hcd xhci-hcd.1: xHCI host controller not responding, assume dead
[   29.589080] xhci-hcd xhci-hcd.1: HC died; cleaning up
[   29.589079] xhci-hcd xhci-hcd.1: Unsuccessful disable slot 2 command, status 25
[   29.601495] usb usb3-port2: couldn't allocate usb_device
[   29.606832] usb 3-1: USB disconnect, device number 2
```

Reproducer: `tools/harness/scripts/rp2040-usb-flashloop.sh`, programming an
image of the kernel and `c_hello` over SWD, resetting the chip, and checking
enumeration. pre-PR, two attempts (recovering the host in between), verbatim:

```
iter 1: FAIL (bound after: never); UART0: no panic
    [ 3551.322068] xhci-hcd xhci-hcd.1: Timeout while waiting for setup device command
    [ 3556.698141] xhci-hcd xhci-hcd.1: Timeout while waiting for setup device command
    [ 3556.906016] usb 3-2: device not accepting address 54, error -62
HOST CONTROLLER DIED
RESULT mode=program image=img-prepr.bin: 0 ok, 1 failed of 1; kernel panics on UART0: 0
```

```
iter 1: FAIL (bound after: never); UART0: no panic
    [ 3598.170092] xhci-hcd xhci-hcd.1: Timeout while waiting for setup device command
    [ 3603.545993] xhci-hcd xhci-hcd.1: Timeout while waiting for setup device command
    [ 3603.749962] usb 3-2: device not accepting address 4, error -62
HOST CONTROLLER DIED
RESULT mode=program image=img-prepr.bin: 0 ok, 1 failed of 1; kernel panics on UART0: 0
```

with-PR, the same image and script:

```
RESULT mode=program image=img-baseline.bin: 30 ok, 0 failed of 30
```

tock/tock#5175 changes the RP2040 USB driver to leave EP0 IN unarmed at bus
reset (it used to answer the host's first IN token with whatever was in the
buffer, and kept ownership of the buffer while filling it), and to complete
zero-length control transfers such as SET_ADDRESS through the status stage.
Both match the failures above: the first descriptor read fails (`-71`), and
the device never answers at its new address (`-62`).

## 2. Not a USB bug: IPC `rot13` panics the kernel on the Pico

With the with-PR kernel, `ipc_rot13` still failed with "no TockOS USB CDC
device appeared within 10s", and the host logged enumeration errors, e.g.:

```
[  566.998932] usb 3-2: device descriptor read/64, error -110
[  569.314931] usb 3-2: device descriptor read/64, error -71
```

The cause is a kernel panic right after boot, which UART0 shows (the USB
console has not enumerated by then):

```
panicked at kernel/src/process_standard.rs:760:17:
Process org.tockos.examples.rot13 had a fault
```

With the reproducer and an image of the kernel and the rot13 client and
service, this panic occurred on 8 of 8 boots with the with-PR kernel, and on
9 of 9 checked boots with this branch's Cortex-M0+ `SVC_SWITCH_TO_APP` commits
(`9a9106e2c`, `b3c2e9376`) reverted, so it is not caused by them.

The service faults on its first read of the buffer the client shares with it
(`r2`, in the client's RAM at `0x20005000`-`0x20007000`; `r1` is its length;
excerpt of the panic's process dump, and the service's disassembly):

```
𝐀𝐩𝐩: org.tockos.examples.rot13   -   [Faulted]
  R0 : 0x00000000    R6 : 0x00000000
  R1 : 0x00000040    R7 : 0x00000000
  R2 : 0x20005940    R8 : 0x10044060
  PC : 0x100440CA
```

```
80000066 <rot13_callback>:
80000066:	push	{r4, r5, r6, r7, lr}
80000068:	movs	r5, #0
8000006a:	ldrsb	r5, [r2, r5]
```

The kernel maps a shared IPC buffer into the recipient's MPU configuration
as one region (`kernel/src/ipc.rs`, `schedule_upcall()`). The Cortex-M0+ MPU
(`cortexm::mpu::MPU<8, 256>`) has 256-byte minimum regions, and correctly
refuses a region for the client's 64-byte buffer. IPC ignores that and passes
the buffer to the service anyway, which then faults. As kernel IPC is about to
be removed, this is not treated as a kernel bug.

**Fixed in libtock-c** (`ci-harness` branch, `6af7b075`): `IPC_MIN_SHARE_SIZE`
(256 bytes on ARMv6-M, 64 elsewhere) sizes and aligns the examples' shared
buffers. With it, and the with-PR kernel:

```
RESULT mode=program image=img-baseline-rot13fix.bin: 10 ok, 0 failed of 10; kernel panics on UART0: 0
```

## 3. Open: rare truncated configuration descriptor

With the with-PR kernel, the device twice returned only the 9-byte header of
its 67-byte configuration descriptor, so `cdc_acm` never bound to it. Once in
`malloc_test01`, and once in `process_console_restart` (app `tests/whileone`):

```
[  578.731881] usb 3-2: config index 0 descriptor too short (expected 67, got 9)
[  578.731886] usb 3-2: config 1 has 0 interfaces, different from the descriptor's value: 2
```

```
[ 4098.698361] usb 3-2: config index 0 descriptor too short (expected 67, got 9)
[ 4098.698367] usb 3-2: config 1 has 0 interfaces, different from the descriptor's value: 2
[ 4098.698723] usb 3-2: string descriptor 0 read error: -71
[ 4098.698977] usb 3-2: can't set config #1, error -71
```

That is 2 failures in 37 tests of three full harness runs of the Pico's
tests (not counting the first run's panicking `ipc_rot13`). It has **not been reproduced** outside them:

```
RESULT mode=program image=img-baseline.bin: 30 ok, 0 failed of 30
RESULT mode=program image=img-baseline-malloc01.bin: 20 ok, 0 failed of 20; kernel panics on UART0: 0
RESULT mode=program image=img-baseline-whileone.bin: 30 ok, 0 failed of 30; kernel panics on UART0: 0
RESULT mode=program image=img-baseline-whileone.bin: 30 ok, 0 failed of 30; kernel panics on UART0: 0
RESULT mode=program image=img-baseline-brkbusy.bin: 20 ok, 0 failed of 20; kernel panics on UART0: 0
RESULT mode=program image=img-baseline-rot13fix.bin: 10 ok, 0 failed of 10; kernel panics on UART0: 0
```

(The second `whileone` run with `CONSOLE=1`; `brkbusy` is an app growing and
shrinking its heap in a loop, keeping the kernel busy zeroing memory while the
host enumerates it.) Nor did the harness's `usb_enumeration_stress` test fail
(50 restarts, in three runs), nor 5000 back-to-back descriptor reads, or 300
cycles of control writes (opening the tty, setting its baud rate, closing it)
each followed by descriptor reads, through usbfs on the running device.

A candidate cause, **unconfirmed**: after the last packet of a control read,
`handle_ep0datadone()` re-arms EP0 IN (`AVAILABLE0::SET`) with the packet it
just sent. If the host's IN token for the next control read arrives before the
firmware handles that read's SETUP, the stale 9-byte packet of the preceding
`GET_DESCRIPTOR(config, 9)` answers `GET_DESCRIPTOR(config, 67)`. In the same
driver, a SETUP received while EP0 is not idle is answered with
`sie_ctrl.write(SIE_CTRL::EP0_INT_STALL::SET)`, which also clears
`PULLUP_EN` (detaching the device); `transmit_in_ep0()` does the same for
`CtrlInResult::Delay` and `CtrlInResult::Error`. A patch addressing these
passed 20 of 20 iterations of the reproducer with `c_hello`, but as the
failure does not reproduce outside of harness runs, there is no evidence that
it fixes it, and it is not part of this branch.

## Harness changes

- `usb_enumeration_stress` (boards with a USB CDC-ACM console): restarts the
  board 50 times, checking each time that the console prints the app's output
  and answers a process console command.
- `Board::restart()`: reset the board without reflashing it, continuing its
  console transcript.
- The Pico backend waits for the previous kernel's CDC-ACM device to go away
  before looking for the new one (both have the same name), and retries
  opening it while udev sets it up.
