// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

// Declarations shared by the files of the glue.

#pragma once

#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#define REG(addr) (*(volatile uint32_t*)(addr))

// From the mask ROM (see layout.ld).
void* memcpy(void* dst, const void* src, size_t n);

// Interrupt sources of the BT blocks. Tock maps each source to the CPU line
// of the same number.
#define SRC_BT_BB 5
#define SRC_RWBT 7
#define SRC_RWBLE 8

// Largest H4 packet. It is the type byte, an ACL header and a 255-byte
// payload.
#define PACKET_MAX 259

// ---------------------------------------------------------------------------
// Byte rings

struct ring {
  uint8_t* buf;
  uint32_t size;
  uint32_t head;
  uint32_t len;
};

#define RING(storage) {(storage), sizeof(storage), 0, 0}

static inline uint32_t ring_space(const struct ring* r) {
  return r->size - r->len;
}

// The caller checks `ring_space` first.
static inline void ring_put(struct ring* r, uint8_t b) {
  r->buf[(r->head + r->len) % r->size] = b;
  r->len++;
}

// The caller checks `len` first.
static inline uint8_t ring_get(struct ring* r) {
  uint8_t b = r->buf[r->head];
  r->head = (r->head + 1) % r->size;
  r->len--;
  return b;
}

// ---------------------------------------------------------------------------
// osi.c

struct osi_funcs;
struct thread;

// The OS functions the controller calls.
extern const struct osi_funcs osi_funcs;

// The controller's task, once the controller has created it.
extern struct thread osi_task_thread;

// Log output for the kernel to drain.
extern struct ring log_ring;

void log_printf(const char* fmt, ...);

// Stop on purpose. The fault ends the current call into the sandbox, and the
// kernel prints the log.
__attribute__((noreturn)) void fail(const char* what);

// Microseconds since boot.
int64_t now_us(void);

// The base MAC address burned into eFuse.
void base_mac(uint8_t mac[6]);

// Run the handler the controller registered for interrupt `source`.
void osi_isr(uint32_t source);

// ---------------------------------------------------------------------------
// threads.c

// A green thread with its own stack. Threads switch only when they wait, so
// nothing preempts a thread.
struct thread {
  uint32_t sp;
  enum { THREAD_NONE, THREAD_RUNNABLE, THREAD_WAITING, THREAD_DONE } state;
  // Absolute time (us) a waiting thread waits until, or -1.
  int64_t deadline;
  void (*entry)(void*);
  void* arg;
};

// Start a thread that runs `entry(arg)` on `stack`. `stack_size` is in bytes
// and a multiple of 16.
void thread_create(struct thread* t, uint32_t* stack, uint32_t stack_size,
                   void (*entry)(void*), void* arg);

// Whether a thread runs, as opposed to an entry point on the Omniglot stack.
bool thread_running(void);

// Let the current thread wait until `threads_wake_all` or until `deadline`
// (absolute, in us, or -1).
void thread_wait(int64_t deadline);

// Let the other threads run before the current one continues.
void thread_yield(void);

// Something a thread may wait for was posted. Every waiting thread checks
// again.
void threads_wake_all(void);

// Run the threads until none can run. Only an entry point calls this.
void threads_run(void);

// Whether a thread can run.
bool threads_runnable(void);

// Whether a thread waits with a deadline.
bool threads_waiting_with_deadline(void);

// ---------------------------------------------------------------------------
// controller.c

// The bring-up has completed, or has failed.
extern bool controller_started, controller_failed;

// Packets for the host, each prefixed with its length (u16, little endian).
extern struct ring rx_ring;

// Start the bring-up thread.
void controller_start(void);

// Whether the controller accepts a packet now.
bool controller_tx_ready(void);

// Hand a packet of `len` bytes to the controller. The caller checks
// `controller_tx_ready` first.
void controller_send(const uint8_t* packet, uint32_t len);

// The thread that runs the bring-up and then sends packets.
extern struct thread controller_thread;
