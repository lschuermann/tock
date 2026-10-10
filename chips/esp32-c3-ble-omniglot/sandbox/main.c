// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

// The entry points the kernel calls (see sandbox.h), and the Omniglot
// function table.
//
// Every entry point runs on the Omniglot stack and returns once no green
// thread can run. Most return the status word, which tells the kernel what
// to do next.

#include "glue.h"
#include "sandbox.h"

static uint32_t status(void) {
  uint32_t s = 0;
  if (controller_started) {
    s |= SANDBOX_STARTED;
  }
  if (controller_failed) {
    s |= SANDBOX_FAILED;
  }
  if (rx_ring.len > 0) {
    s |= SANDBOX_RX;
  }
  if (log_ring.len > 0) {
    s |= SANDBOX_LOG;
  }
  if (controller_tx_ready()) {
    s |= SANDBOX_TX_READY;
  }
  if (threads_runnable()) {
    s |= SANDBOX_RUNNABLE;
  }
  if (threads_waiting_with_deadline()) {
    s |= SANDBOX_DEADLINE;
  }
  return s;
}

uint32_t sandbox_start(void) {
  controller_start();
  threads_run();
  return status();
}

uint32_t sandbox_run(void) {
  threads_run();
  return status();
}

uint32_t sandbox_isr(uint32_t line) {
  osi_isr(line);
  return status();
}

int32_t sandbox_hci_send(const uint8_t* packet, uint32_t len) {
  if (!controller_started) {
    return SANDBOX_ERR_OFF;
  }
  if (len < 1 || len > PACKET_MAX) {
    return SANDBOX_ERR_SIZE;
  }
  if (!controller_tx_ready()) {
    return SANDBOX_ERR_BUSY;
  }
  controller_send(packet, len);
  threads_run();
  return (int32_t)status();
}

uint32_t sandbox_hci_recv(uint8_t* buf, uint32_t cap) {
  while (rx_ring.len >= 2) {
    uint32_t len = ring_get(&rx_ring);
    len |= (uint32_t)ring_get(&rx_ring) << 8;
    if (len > cap) {
      for (uint32_t i = 0; i < len; i++) {
        ring_get(&rx_ring);
      }
      rx_ring.drops++;
      continue;
    }
    for (uint32_t i = 0; i < len; i++) {
      buf[i] = ring_get(&rx_ring);
    }
    return len;
  }
  return 0;
}

uint32_t sandbox_log(uint8_t* buf, uint32_t cap) {
  uint32_t n = 0;
  while (n < cap && log_ring.len > 0) {
    buf[n++] = ring_get(&log_ring);
  }
  return n;
}

// The function table. The indices must match the `fntab_id`s in
// `sandbox.omniglot.toml`.
typedef void (*fnptr)(void);

fnptr const __attribute__((section(".omniglot_fntab"),
                           used)) omniglot_fntab[] = {
    /* 0 */ (fnptr)sandbox_start,
    /* 1 */ (fnptr)sandbox_run,
    /* 2 */ (fnptr)sandbox_isr,
    /* 3 */ (fnptr)sandbox_hci_send,
    /* 4 */ (fnptr)sandbox_hci_recv,
    /* 5 */ (fnptr)sandbox_log,
    /* 6 */ (fnptr)sandbox_stat,
    /* 7 */ (fnptr)sandbox_test,
};
