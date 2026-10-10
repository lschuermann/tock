// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

// Interface of the sandboxed BLE controller to the kernel.
//
// Every call runs to completion. Inside the sandbox, the controller task and
// the bring-up run as green threads. A call runs them until they all wait.
// The status it returns tells the kernel what to do next.

#include <stdint.h>

// Status bits, returned by most calls.

// Bring-up has completed; HCI packets can be sent.
#define SANDBOX_STARTED (1u << 0)
// Bring-up failed (see the log).
#define SANDBOX_FAILED (1u << 1)
// Packets for the host are queued: drain them with `sandbox_hci_recv`.
#define SANDBOX_RX (1u << 2)
// Log output is queued: drain it with `sandbox_log`.
#define SANDBOX_LOG (1u << 3)
// A green thread can run: call `sandbox_run`.
#define SANDBOX_RUNNABLE (1u << 4)
// A green thread waits with a timeout: call `sandbox_run` again later.
#define SANDBOX_DEADLINE (1u << 5)
// `sandbox_hci_send` accepts a packet.
#define SANDBOX_TX_READY (1u << 6)

// Errors from `sandbox_hci_send`.
#define SANDBOX_ERR_OFF (-1)
#define SANDBOX_ERR_BUSY (-2)
#define SANDBOX_ERR_SIZE (-3)

// Start bringing up the controller, and run until it blocks.
uint32_t sandbox_start(void);

// Run the green threads until they all block.
uint32_t sandbox_run(void);

// Run the controller's handler for interrupt `line` (an interrupt source,
// mapped 1:1 to a CPU line).
uint32_t sandbox_isr(uint32_t line);

// Send one H4 packet (type byte first) to the controller. Returns the status
// or a negative `SANDBOX_ERR_*`.
int32_t sandbox_hci_send(const uint8_t* packet, uint32_t len);

// Copy the oldest queued packet for the host into `buf`, and return its
// length, or 0 if none is queued. Packets longer than `cap` are dropped.
uint32_t sandbox_hci_recv(uint8_t* buf, uint32_t cap);

// Copy up to `cap` bytes of queued log output into `buf`, and return how many.
uint32_t sandbox_log(uint8_t* buf, uint32_t cap);
