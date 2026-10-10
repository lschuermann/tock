// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

// The OS functions the controller calls, and the symbols the archives need
// from ESP-IDF.
//
// The controller (in mask ROM, patched by `libbtdm_app.a`) expects an RTOS.
// It reaches the RTOS through the table `osi_funcs`. Its task runs as a
// green thread (see threads.c). A semaphore or queue that a thread cannot
// take makes the thread wait until something is posted.
//
// Interrupt handlers run when the kernel calls `sandbox_isr`. The kernel
// defers interrupts taken while the sandbox runs, so foreign code never
// preempts foreign code. Critical sections are therefore no-ops, and a mutex
// is never contended.

#include "glue.h"
#include "libbtdm.h"

#define INTMATRIX_BASE 0x600c2000

#define SYSTIMER_UNIT0_OP 0x60023004
#define SYSTIMER_UNIT0_VALUE_HI 0x60023040
#define SYSTIMER_UNIT0_VALUE_LO 0x60023044

#define EFUSE_MAC_0 0x60008844
#define EFUSE_MAC_1 0x60008848

#define RNG_DATA 0x600260b0

// ESP-IDF gives the controller task 3.5-4 KiB.
#define TASK_STACK_SIZE 4096

static uint32_t task_stack[TASK_STACK_SIZE / 4] __attribute__((aligned(16)));
struct thread osi_task_thread;

// Whether an interrupt handler runs.
static bool in_isr;

// Microseconds since boot, from SYSTIMER unit 0 (16 MHz). The kernel only
// latches unit 0 while the sandbox is not running.
int64_t now_us(void) {
  REG(SYSTIMER_UNIT0_OP) = 1u << 30;
  while ((REG(SYSTIMER_UNIT0_OP) & (1u << 29)) == 0) {
  }
  uint64_t hi = REG(SYSTIMER_UNIT0_VALUE_HI);
  uint64_t lo = REG(SYSTIMER_UNIT0_VALUE_LO);
  return (int64_t)(((hi << 32) | lo) / 16);
}

void base_mac(uint8_t mac[6]) {
  uint32_t lo = REG(EFUSE_MAC_0), hi = REG(EFUSE_MAC_1);
  mac[0] = (uint8_t)(hi >> 8);
  mac[1] = (uint8_t)hi;
  mac[2] = (uint8_t)(lo >> 24);
  mac[3] = (uint8_t)(lo >> 16);
  mac[4] = (uint8_t)(lo >> 8);
  mac[5] = (uint8_t)lo;
}

// ---------------------------------------------------------------------------
// Log

static uint8_t log_buf[2048];
struct ring log_ring = RING(log_buf);

static void log_putc(char c) {
  if (ring_space(&log_ring) == 0) {
    return;
  }
  ring_put(&log_ring, (uint8_t)c);
}

static void log_vprintf(const char* fmt, va_list ap) {
  ets_vprintf(log_putc, fmt, ap);
}

void log_printf(const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  log_vprintf(fmt, ap);
  va_end(ap);
}

// The archives' printfs go into the log.
int ets_printf(const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  log_vprintf(fmt, ap);
  va_end(ap);
  return 0;
}

int phy_printf(const char* fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  log_vprintf(fmt, ap);
  va_end(ap);
  return 0;
}

void fail(const char* what) {
  log_printf("ble: %s\n", what);
  for (;;) {
    __asm__ volatile("unimp");
  }
}

// ---------------------------------------------------------------------------
// Heap. It is a first fit over a fixed arena. Each block starts with a header
// that holds its size including the header, and whether it is in use. Blocks
// are contiguous and 8-byte aligned.

#define HEAP_SIZE (32 * 1024)
#define HDR 8

static uint8_t arena[HEAP_SIZE] __attribute__((aligned(8)));
static bool heap_init;
static uint32_t heap_used;

static void set_hdr(uint8_t* p, uint32_t size, bool used) {
  ((uint32_t*)p)[0] = size;
  ((uint32_t*)p)[1] = used;
}

static void* heap_malloc(size_t size) {
  if (!heap_init) {
    set_hdr(arena, HEAP_SIZE, false);
    heap_init = true;
  }
  uint32_t need = (uint32_t)((size ? size : 1) + 7) / 8 * 8 + HDR;
  uint8_t* end = arena + HEAP_SIZE;
  for (uint8_t* p = arena; p < end;) {
    uint32_t bsize = ((uint32_t*)p)[0];
    if (!((uint32_t*)p)[1]) {
      // Merge any free blocks that follow.
      for (uint8_t* next = p + bsize; next < end && !((uint32_t*)next)[1];
           next = p + bsize) {
        bsize += ((uint32_t*)next)[0];
      }
      set_hdr(p, bsize, false);
      if (bsize >= need) {
        if (bsize - need >= HDR + 8) {
          set_hdr(p + need, bsize - need, false);
          bsize = need;
        }
        set_hdr(p, bsize, true);
        heap_used += bsize;
        return p + HDR;
      }
    }
    p += bsize;
  }
  log_printf("ble: malloc(%u) failed, %u B in use\n", (unsigned)size,
             (unsigned)heap_used);
  return NULL;
}

static void heap_free(void* ptr) {
  if (ptr == NULL) {
    return;
  }
  uint8_t* p = (uint8_t*)ptr - HDR;
  if (!((uint32_t*)p)[1]) {
    fail("double free");
  }
  ((uint32_t*)p)[1] = 0;
  heap_used -= ((uint32_t*)p)[0];
}

// ---------------------------------------------------------------------------
// Waiting

#define FOREVER 0xffffffffu

// The absolute time (us) a wait of `timeout_ms` ends, or -1 for no end.
static int64_t deadline_after(uint32_t timeout_ms) {
  if (timeout_ms == FOREVER) {
    return -1;
  }
  return now_us() + (int64_t)timeout_ms * 1000;
}

// Wait until something is posted or `deadline` passes. Returns false if the
// caller cannot wait any longer, because the deadline has passed or because
// an interrupt handler runs.
static bool wait_for_post(int64_t deadline) {
  if (in_isr || (deadline >= 0 && now_us() >= deadline)) {
    return false;
  }
  thread_wait(deadline);
  return true;
}

// ---------------------------------------------------------------------------
// OS functions

// Handlers the controller registered, by interrupt source.
static isr_fn isr_handler[32];
static void* isr_arg[32];

static int32_t interrupt_alloc(int32_t cpu, int32_t source, isr_fn handler,
                               void* arg, void** handle) {
  (void)cpu;
  log_printf("ble: interrupt_alloc source %d\n", source);
  if (source < 0 || source >= 32) {
    return -1;
  }
  isr_handler[source] = handler;
  isr_arg[source] = arg;
  // Map the source to the CPU line of the same number, as Tock does.
  REG(INTMATRIX_BASE + 4 * source) = source;
  // esp-radio routes RWBT to the BT_BB handler; the controller does not
  // register it separately.
  if (source == SRC_BT_BB) {
    REG(INTMATRIX_BASE + 4 * SRC_RWBT) = SRC_RWBT;
  }
  if (handle != NULL) {
    *handle = (void*)(source + 1);
  }
  return 0;
}

static int32_t interrupt_free(void* handle) {
  (void)handle;
  return 0;
}

// Tock keeps every CPU line enabled, masking a line only while its
// interrupt waits to be serviced.
static int32_t interrupt_enable(void* handle) {
  (void)handle;
  return 0;
}

static int32_t interrupt_disable(void* handle) {
  (void)handle;
  return 0;
}

// Nothing preempts foreign code, so there are no critical sections.
static void global_intr_disable(void) {}
static void global_intr_restore(void) {}

static void task_yield(void) {
  thread_yield();
}

static void task_yield_from_isr(void) {}

struct sem {
  uint32_t count;
  uint32_t max;
};

static void* semphr_create(uint32_t max, uint32_t init) {
  struct sem* s = heap_malloc(sizeof(struct sem));
  if (s != NULL) {
    s->count = init;
    s->max = max;
  }
  return s;
}

static void semphr_delete(void* s) {
  heap_free(s);
}

static bool sem_try_take(struct sem* s) {
  if (s->count > 0) {
    s->count--;
    return true;
  }
  return false;
}

static bool sem_give(struct sem* s) {
  if (s->count < s->max) {
    s->count++;
    threads_wake_all();
    return true;
  }
  return false;
}

static int32_t semphr_take_from_isr(void* s, void* hptw) {
  (void)hptw;
  return sem_try_take(s);
}

static int32_t semphr_give_from_isr(void* s, void* hptw) {
  (void)hptw;
  return sem_give(s);
}

static int32_t semphr_take(void* s, uint32_t ms) {
  int64_t deadline = deadline_after(ms);
  do {
    if (sem_try_take(s)) {
      return true;
    }
  } while (wait_for_post(deadline));
  return false;
}

static int32_t semphr_give(void* s) {
  return sem_give(s);
}

// Cooperative scheduling: a mutex is never contended.
static void* mutex_create(void) {
  return heap_malloc(4);
}

static void mutex_delete(void* m) {
  heap_free(m);
}

static int32_t mutex_lock(void* m) {
  (void)m;
  return 1;
}

static int32_t mutex_unlock(void* m) {
  (void)m;
  return 1;
}

struct queue {
  uint32_t item;
  uint32_t len;
  uint32_t head;
  uint32_t count;
  uint8_t* buf;
};

static void* queue_create(uint32_t len, uint32_t item) {
  struct queue* q = heap_malloc(sizeof(struct queue));
  uint8_t* buf = heap_malloc(len * item);
  if (q == NULL || buf == NULL) {
    heap_free(q);
    heap_free(buf);
    return NULL;
  }
  *q = (struct queue){.item = item, .len = len, .buf = buf};
  return q;
}

static void queue_delete(void* q_) {
  struct queue* q = q_;
  heap_free(q->buf);
  heap_free(q);
}

static bool queue_try_send(struct queue* q, void* item) {
  if (q->count == q->len) {
    return false;
  }
  uint32_t slot = (q->head + q->count) % q->len;
  memcpy(q->buf + slot * q->item, item, q->item);
  q->count++;
  threads_wake_all();
  return true;
}

static bool queue_try_recv(struct queue* q, void* item) {
  if (q->count == 0) {
    return false;
  }
  memcpy(item, q->buf + q->head * q->item, q->item);
  q->head = (q->head + 1) % q->len;
  q->count--;
  return true;
}

static int32_t queue_send(void* q, void* item, uint32_t ms) {
  int64_t deadline = deadline_after(ms);
  do {
    if (queue_try_send(q, item)) {
      return true;
    }
  } while (wait_for_post(deadline));
  return false;
}

static int32_t queue_send_from_isr(void* q, void* item, void* hptw) {
  (void)hptw;
  return queue_try_send(q, item);
}

static int32_t queue_recv(void* q, void* item, uint32_t ms) {
  int64_t deadline = deadline_after(ms);
  do {
    if (queue_try_recv(q, item)) {
      return true;
    }
  } while (wait_for_post(deadline));
  return false;
}

static int32_t queue_recv_from_isr(void* q, void* item, void* hptw) {
  (void)hptw;
  return queue_try_recv(q, item);
}

static int32_t task_create(void* func, const char* name, uint32_t stack_depth,
                           void* param, uint32_t prio, void* handle,
                           uint32_t core) {
  (void)prio;
  (void)core;
  log_printf("ble: task_create %s, %u B stack\n", name,
             (unsigned)stack_depth);
  if (osi_task_thread.state != THREAD_NONE ||
      stack_depth > sizeof(task_stack)) {
    log_printf("ble: task_create failed\n");
    return 0;
  }
  thread_create(&osi_task_thread, task_stack, sizeof(task_stack),
                (void (*)(void*))func, param);
  if (handle != NULL) {
    *(uint32_t*)handle = 1;
  }
  return 1;
}

static void task_delete(void* handle) {
  (void)handle;
  fail("task_delete");
}

static bool is_in_isr(void) {
  return in_isr;
}

static void* malloc_fn(size_t size) {
  return heap_malloc(size);
}

static void free_fn(void* p) {
  heap_free(p);
}

// The BT MAC is the base MAC plus 2 (ESP-IDF's `ESP_MAC_BT`).
static int32_t read_efuse_mac(uint8_t* mac) {
  base_mac(mac);
  mac[5] += 2;
  return 0;
}

static void srand_fn(uint32_t seed) {
  (void)seed;
}

static int32_t rand_fn(void) {
  return (int32_t)REG(RNG_DATA);
}

// The low-power clock is the main crystal divided down to 1 MHz, so a cycle
// is one microsecond (in 19-bit fixed point), as in ESP-IDF.
#define LPCYCLE_US_FRAC 19
#define LPCYCLE_US (1ull << LPCYCLE_US_FRAC)

static uint32_t btdm_lpcycles_2_hus(uint32_t cycles, uint32_t* error_corr) {
  uint64_t local = error_corr == NULL ? 0 : *error_corr;
  uint64_t res = LPCYCLE_US * cycles * 2;
  local += res;
  res = local >> LPCYCLE_US_FRAC;
  local -= res << LPCYCLE_US_FRAC;
  if (error_corr != NULL) {
    *error_corr = (uint32_t)local;
  }
  return (uint32_t)res;
}

static uint32_t btdm_hus_2_lpcycles(uint32_t hus) {
  return (uint32_t)((((uint64_t)hus << LPCYCLE_US_FRAC) / LPCYCLE_US) >> 1);
}

// Modem sleep is disabled.
static bool btdm_sleep_check_duration(int32_t* slots) {
  (void)slots;
  return false;
}

static void btdm_sleep_enter_phase1(uint32_t lpcycles) {
  (void)lpcycles;
}

static void nop(void) {}

// No Wi-Fi, so no coexistence: the values ESP-IDF uses without it.
static void coex_wifi_sleep_set(bool sleep) {
  (void)sleep;
}

static int32_t coex_schm_register_btdm_callback(void* cb) {
  (void)cb;
  return 0;
}

static void coex_schm_status_bit(uint32_t type, uint32_t status) {
  (void)type;
  (void)status;
}

static uint32_t coex_schm_interval_get(void) {
  return 0;
}

static uint8_t coex_schm_curr_period_get(void) {
  return 1;
}

static void* coex_schm_curr_phase_get(void) {
  return NULL;
}

static bool coex_bt_wakeup_request(void) {
  return false;
}

static void ets_backup_dma_copy(uint32_t reg, uint32_t mem, uint32_t num,
                                bool to_mem) {
  (void)reg;
  (void)mem;
  (void)num;
  (void)to_mem;
}

static void delay_us(uint32_t us) {
  ets_delay_us(us);
}

// Install the patched functions for the controller's feature set, which
// must match the config flags below.
static void btdm_rom_table_ready(void) {
  ble_base_funcs_reset();
  ble_42_adv_funcs_reset();
  ble_ext_adv_funcs_reset();
  ble_dtm_funcs_reset();
  ble_scan_funcs_reset();
  ble_ext_scan_funcs_reset();
  ble_enc_funcs_reset();
  ble_init_funcs_reset();
  ble_con_funcs_reset();
}

static int64_t get_time_us(void) {
  return now_us();
}

static void assert_fn(void) {
  fail("controller assert");
}

const struct osi_funcs osi_funcs = {
    .magic = 0xfadebead,
    .version = 0x0001000b,
    .interrupt_alloc = interrupt_alloc,
    .interrupt_free = interrupt_free,
    .global_intr_disable = global_intr_disable,
    .global_intr_restore = global_intr_restore,
    .task_yield = task_yield,
    .task_yield_from_isr = task_yield_from_isr,
    .semphr_create = semphr_create,
    .semphr_delete = semphr_delete,
    .semphr_take_from_isr = semphr_take_from_isr,
    .semphr_give_from_isr = semphr_give_from_isr,
    .semphr_take = semphr_take,
    .semphr_give = semphr_give,
    .mutex_create = mutex_create,
    .mutex_delete = mutex_delete,
    .mutex_lock = mutex_lock,
    .mutex_unlock = mutex_unlock,
    .queue_create = queue_create,
    .queue_delete = queue_delete,
    .queue_send = queue_send,
    .queue_send_from_isr = queue_send_from_isr,
    .queue_recv = queue_recv,
    .queue_recv_from_isr = queue_recv_from_isr,
    .task_create = task_create,
    .task_delete = task_delete,
    .is_in_isr = is_in_isr,
    .malloc = malloc_fn,
    .malloc_internal = malloc_fn,
    .free = free_fn,
    .read_efuse_mac = read_efuse_mac,
    .srand = srand_fn,
    .rand = rand_fn,
    .btdm_lpcycles_2_hus = btdm_lpcycles_2_hus,
    .btdm_hus_2_lpcycles = btdm_hus_2_lpcycles,
    .btdm_sleep_check_duration = btdm_sleep_check_duration,
    .btdm_sleep_enter_phase1 = btdm_sleep_enter_phase1,
    .btdm_sleep_enter_phase2 = nop,
    .btdm_sleep_exit_phase3 = nop,
    .coex_wifi_sleep_set = coex_wifi_sleep_set,
    .coex_schm_register_btdm_callback = coex_schm_register_btdm_callback,
    .coex_schm_status_bit_set = coex_schm_status_bit,
    .coex_schm_status_bit_clear = coex_schm_status_bit,
    .coex_schm_interval_get = coex_schm_interval_get,
    .coex_schm_curr_period_get = coex_schm_curr_period_get,
    .coex_schm_curr_phase_get = coex_schm_curr_phase_get,
    .interrupt_enable = interrupt_enable,
    .interrupt_disable = interrupt_disable,
    .esp_hw_power_down = nop,
    .esp_hw_power_up = nop,
    .ets_backup_dma_copy = ets_backup_dma_copy,
    .malloc_retention = malloc_fn,
    .ets_delay_us = delay_us,
    .btdm_rom_table_ready = btdm_rom_table_ready,
    .coex_bt_wakeup_request = coex_bt_wakeup_request,
    .coex_bt_wakeup_request_end = nop,
    .get_time_us = get_time_us,
    .assert = assert_fn,
};

void osi_isr(uint32_t source) {
  // RWBT shares the BT_BB handler.
  if (source == SRC_RWBT) {
    source = SRC_BT_BB;
  }
  if (source < 32 && isr_handler[source] != NULL) {
    in_isr = true;
    isr_handler[source](isr_arg[source]);
    in_isr = false;
  }
}
