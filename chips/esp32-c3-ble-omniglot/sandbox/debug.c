// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

// Debug entry points: statistics and fault injection, for the kernel
// driver's `instrumentation`. The controller does not need them.

#include "glue.h"
#include "sandbox.h"

uint32_t sandbox_stat(uint32_t which) {
  switch (which) {
    case SANDBOX_STAT_HEAP_USED:
      return heap_used;
    case SANDBOX_STAT_HEAP_PEAK:
      return heap_peak;
    case SANDBOX_STAT_TASK_STACK:
      return thread_stack_used(&osi_task_thread);
    case SANDBOX_STAT_CONTROLLER_STACK:
      return thread_stack_used(&controller_thread);
    case SANDBOX_STAT_RX_DROPS:
      return rx_ring.drops;
    case SANDBOX_STAT_LOG_DROPS:
      return log_ring.drops;
    default:
      return 0;
  }
}

// Kernel RAM, below `ble_ram`.
#define TEST_KERNEL_RAM 0x3fcc0000u
// The kernel's `.iram` code (layout.ld), through the data-bus alias.
#define TEST_KERNEL_IRAM 0x3fcdef00u
// SPI1's command register.
#define TEST_SPI1_CMD 0x60002000u
// The first EM slot register.
#define TEST_EM_SLOT0 0x60031204u

uint32_t sandbox_test(uint32_t kind) {
  switch (kind) {
    case SANDBOX_TEST_LOAD_KERNEL:
      return REG(TEST_KERNEL_RAM);
    case SANDBOX_TEST_STORE_EM:
      REG(TEST_EM_SLOT0) = TEST_KERNEL_RAM;
      return 0;
    case SANDBOX_TEST_STORE_SPI1:
      REG(TEST_SPI1_CMD) = 0;
      return 0;
    case SANDBOX_TEST_STORE_CODE:
      *(volatile uint32_t*)(uintptr_t)sandbox_test = 0;
      return 0;
    case SANDBOX_TEST_JUMP:
      ((void (*)(void))TEST_KERNEL_RAM)();
      return 0;
    case SANDBOX_TEST_EBREAK:
      __asm__ volatile("ebreak");
      return 0;
    case SANDBOX_TEST_STORE_KERNEL_IRAM:
      REG(TEST_KERNEL_IRAM) = 0;
      return 0;
    default:
      return 0;
  }
}
