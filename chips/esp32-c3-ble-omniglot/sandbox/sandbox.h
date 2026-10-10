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

// Debug: values for `sandbox_stat`.
#define SANDBOX_STAT_HEAP_USED 0
#define SANDBOX_STAT_HEAP_PEAK 1
#define SANDBOX_STAT_TASK_STACK 2
#define SANDBOX_STAT_CONTROLLER_STACK 3
#define SANDBOX_STAT_RX_DROPS 4
#define SANDBOX_STAT_LOG_DROPS 5

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

// Debug: return one of the `SANDBOX_STAT_*` values.
uint32_t sandbox_stat(uint32_t which);

// Debug: faults for `sandbox_test`, each of which the kernel must contain.
// Load from kernel RAM.
#define SANDBOX_TEST_LOAD_KERNEL 1
// Store to an EM mapping register, which only the kernel may write.
#define SANDBOX_TEST_STORE_EM 2
// Store to the flash controller (SPI1).
#define SANDBOX_TEST_STORE_SPI1 3
// Store to the sandbox's own code.
#define SANDBOX_TEST_STORE_CODE 4
// Jump into kernel RAM.
#define SANDBOX_TEST_JUMP 6
// `ebreak`, as the ROM's asserts do.
#define SANDBOX_TEST_EBREAK 7
// Store to the kernel's `.iram` code, through the data-bus alias.
#define SANDBOX_TEST_STORE_KERNEL_IRAM 8

// Debug: fault on purpose, in one of the `SANDBOX_TEST_*` ways; returns 0 for
// any other `kind`.
uint32_t sandbox_test(uint32_t kind);
