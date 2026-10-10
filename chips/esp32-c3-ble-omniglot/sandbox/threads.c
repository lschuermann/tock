// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

// Green threads for the controller.
//
// The controller expects an RTOS that blocks its task on a semaphore. Here
// every thread runs on its own stack in sandbox RAM. A thread that waits
// switches back to the scheduler, which runs on the Omniglot stack inside an
// entry point. Once no thread can run, the scheduler returns, and so does
// the entry point. Every call from the kernel therefore runs to completion.

#include "glue.h"

#define THREADS_MAX 2

static struct thread* threads[THREADS_MAX];
// The running thread, or NULL while the scheduler runs.
static struct thread* current;
// The scheduler's stack pointer while a thread runs.
static uint32_t scheduler_sp;

// Save the callee-saved registers and `ra` on the current stack, store the
// stack pointer to `*save_sp`, then restore the registers from the stack at
// `load_sp` and return there. Threads switch only at calls, so the
// caller-saved registers need not survive.
//
// The function is naked so that the compiler adds no prologue. Only the
// assembly uses the parameters.
__attribute__((naked)) static void thread_switch(
    __attribute__((unused)) uint32_t* save_sp,
    __attribute__((unused)) uint32_t load_sp) {
  __asm__(
      "addi sp, sp, -64\n"
      "sw   ra,   0(sp)\n"
      "sw   s0,   4(sp)\n"
      "sw   s1,   8(sp)\n"
      "sw   s2,  12(sp)\n"
      "sw   s3,  16(sp)\n"
      "sw   s4,  20(sp)\n"
      "sw   s5,  24(sp)\n"
      "sw   s6,  28(sp)\n"
      "sw   s7,  32(sp)\n"
      "sw   s8,  36(sp)\n"
      "sw   s9,  40(sp)\n"
      "sw   s10, 44(sp)\n"
      "sw   s11, 48(sp)\n"
      "sw   sp,   0(a0)\n"
      "mv   sp, a1\n"
      "lw   ra,   0(sp)\n"
      "lw   s0,   4(sp)\n"
      "lw   s1,   8(sp)\n"
      "lw   s2,  12(sp)\n"
      "lw   s3,  16(sp)\n"
      "lw   s4,  20(sp)\n"
      "lw   s5,  24(sp)\n"
      "lw   s6,  28(sp)\n"
      "lw   s7,  32(sp)\n"
      "lw   s8,  36(sp)\n"
      "lw   s9,  40(sp)\n"
      "lw   s10, 44(sp)\n"
      "lw   s11, 48(sp)\n"
      "addi sp, sp, 64\n"
      "ret\n");
}

// Switch from the current thread back to the scheduler.
static void to_scheduler(void) {
  thread_switch(&current->sp, scheduler_sp);
}

// The first code a new thread runs. `thread_switch` returns here.
static void thread_entry(void) {
  current->entry(current->arg);
  log_printf("ble: a thread returned\n");
  current->state = THREAD_DONE;
  to_scheduler();
  fail("a finished thread ran");
}

void thread_create(struct thread* t, uint32_t* stack, uint32_t stack_size,
                   void (*entry)(void*), void* arg) {
  uint32_t words = stack_size / 4;
  // The frame `thread_switch` restores. Its `ra` is `thread_entry`.
  uint32_t* frame = stack + words - 16;
  frame[0] = (uint32_t)thread_entry;
  *t = (struct thread){
      .sp = (uint32_t)frame,
      .state = THREAD_RUNNABLE,
      .deadline = -1,
      .entry = entry,
      .arg = arg,
  };
  for (int i = 0; i < THREADS_MAX; i++) {
    if (threads[i] == NULL || threads[i] == t) {
      threads[i] = t;
      return;
    }
  }
  fail("too many threads");
}

bool thread_running(void) {
  return current != NULL;
}

void thread_wait(int64_t deadline) {
  if (current == NULL) {
    fail("wait outside a thread");
  }
  current->state = THREAD_WAITING;
  current->deadline = deadline;
  to_scheduler();
  current->deadline = -1;
}

void thread_yield(void) {
  if (current != NULL) {
    current->state = THREAD_RUNNABLE;
    to_scheduler();
  }
}

void threads_wake_all(void) {
  for (int i = 0; i < THREADS_MAX; i++) {
    if (threads[i] != NULL && threads[i]->state == THREAD_WAITING) {
      threads[i]->state = THREAD_RUNNABLE;
    }
  }
}

void threads_run(void) {
  int64_t now = -1;
  for (int i = 0; i < THREADS_MAX; i++) {
    struct thread* t = threads[i];
    if (t != NULL && t->state == THREAD_WAITING && t->deadline >= 0) {
      if (now < 0) {
        now = now_us();
      }
      if (now >= t->deadline) {
        t->state = THREAD_RUNNABLE;
      }
    }
  }
  bool progress = true;
  while (progress) {
    progress = false;
    for (int i = 0; i < THREADS_MAX; i++) {
      struct thread* t = threads[i];
      if (t != NULL && t->state == THREAD_RUNNABLE) {
        current = t;
        thread_switch(&scheduler_sp, t->sp);
        current = NULL;
        progress = true;
      }
    }
  }
}

bool threads_runnable(void) {
  for (int i = 0; i < THREADS_MAX; i++) {
    if (threads[i] != NULL && threads[i]->state == THREAD_RUNNABLE) {
      return true;
    }
  }
  return false;
}

bool threads_waiting_with_deadline(void) {
  for (int i = 0; i < THREADS_MAX; i++) {
    if (threads[i] != NULL && threads[i]->state == THREAD_WAITING &&
        threads[i]->deadline >= 0) {
      return true;
    }
  }
  return false;
}
