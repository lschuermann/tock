#!/bin/bash
# Licensed under the Apache License, Version 2.0 or the MIT License.
# SPDX-License-Identifier: Apache-2.0 OR MIT
# Copyright Tock Contributors 2026.
#
# Reproducer for RP2040 USB enumeration failures, see RP2040_USB_NOTES.md.
# Runs on the pton-rpi2004 Treadmill host: its Raspberry Pi Pico is on USB
# port 3-2, and its Debug Probe has the serial number below.
#
# Usage: [CONSOLE=1] rp2040-usb-flashloop.sh <image> <iterations> <program|reset>
#
# <image> is a 512 KiB image for 0x10000000: the kernel, then the apps' TBFs
# from 0x10040000, padded with 0xff. Each iteration either programs it over
# SWD (halting the running kernel, like the harness's flash) and resets the
# chip through its watchdog (like the harness), or only resets the chip. It
# then counts the iteration as ok if the kernel's CDC-ACM device enumerated
# within 10 s without USB errors in the kernel log, and logs UART0 (the panic
# console) for kernel panics. CONSOLE=1 also opens the console after each
# enumeration and runs a process console command, like a test does.
IMG=$1; N=$2; MODE=$3
OCD="openocd -f interface/cmsis-dap.cfg -c \"adapter serial E6616408437E5133\" -f target/rp2040.cfg -c \"adapter speed 5000\""
RESET='-c init -c "write_memory 0x4005801c 32 0" -c "write_memory 0x40010008 32 0x1fffc" -c "catch {write_memory 0x40058000 32 0x80000000}" -c exit'
ok=0; fail=0
for i in $(seq 1 $N); do
  sudo dmesg -c > /dev/null
  if [ $MODE = program ]; then eval $OCD -c "\"program $IMG verify 0x10000000\"" -c exit > /tmp/ocd.log 2>&1 || { echo "iter $i: openocd program failed"; tail -3 /tmp/ocd.log; break; }; fi
  # Log UART0 (the panic console, also with a USB CDC-ACM console) during boot.
  U=/dev/serial/by-id/usb-Raspberry_Pi_Debug_Probe__CMSIS-DAP__E6616408437E5133-if01
  stty -F $U 115200 raw -echo; (timeout 6 cat $U > /tmp/uart0-$i.log &)
  eval $OCD $RESET > /tmp/ocd.log 2>&1
  t0=$(date +%s.%N); bound=""
  for w in $(seq 1 100); do
    if sudo dmesg | grep -q "cdc_acm 3-2:1.0: ttyACM"; then bound=$(awk "BEGIN{printf \"%.1fs\", $(date +%s.%N)-$t0}"); break; fi
    sleep 0.1
  done
  # Use the console like a test does: open it (DTR on), run a process console
  # command, and close it again, leaving CDC-ACM connected until the next
  # iteration's flash.
  if [ -n "$bound" ] && [ "$CONSOLE" = 1 ]; then
    T=$(ls /dev/serial/by-id/*TockOS* 2>/dev/null | head -1)
    python3 - "$T" <<'PY' || echo "iter $i: console check failed"
import os, sys, time, termios
fd = os.open(sys.argv[1], os.O_RDWR | os.O_NOCTTY)
a = termios.tcgetattr(fd); a[3] &= ~(termios.ECHO | termios.ICANON); a[0] = a[1] = 0; termios.tcsetattr(fd, termios.TCSANOW, a)
os.set_blocking(fd, False); buf = b""
for c in b"list\r\n": os.write(fd, bytes([c])); time.sleep(0.01)
t = time.time()
while time.time() - t < 3 and b"tock$" not in buf.split(b"list", 1)[-1]:
    try: buf += os.read(fd, 4096)
    except BlockingIOError: time.sleep(0.02)
os.close(fd)
sys.exit(0 if b"PID" in buf or b"whileone" in buf or b"c_hello" in buf else 1)
PY
  fi
  errs=$(sudo dmesg | grep -E "usb 3-2: .*(error|too short|not accepting|0 interfaces)|xhci-hcd.1" )
  sleep 6 & wait
  panic=$(grep -a -m1 -E "panicked at|had a fault" /tmp/uart0-$i.log)
  [ -n "$panic" ] && panics=$((panics+1))
  if [ -n "$bound" ] && [ -z "$errs" ]; then ok=$((ok+1)); [ -n "$panic" ] && echo "iter $i: enumerated OK, but UART0: $(tr -d '\r' < /tmp/uart0-$i.log | grep -a -A1 'panicked at' | tr '\n' ' ')"; else fail=$((fail+1)); echo "iter $i: FAIL (bound after: ${bound:-never}); UART0: ${panic:-no panic}"; echo "$errs" | sed 's/^/    /'; fi
  if sudo dmesg | grep -q "HC died"; then echo "HOST CONTROLLER DIED"; break; fi
done
echo "RESULT mode=$MODE image=$(basename $IMG): $ok ok, $fail failed of $i; kernel panics on UART0: ${panics:-0}"
