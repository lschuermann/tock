// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

// This header declares the interface of Espressif's BLE controller for the
// ESP32-C3. It covers the types and functions of `libbtdm_app.a` and of the
// mask ROM that the glue uses. The layouts follow ESP-IDF's
// `components/bt/controller/esp32c3/bt.c` and `esp_bt.h`.

#pragma once

#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

// An interrupt handler the controller registers, with its argument.
typedef void (*isr_fn)(void*);

// The OS functions the controller calls. This is ESP-IDF's `osi_funcs_t` for
// the ESP32-C3, version 0x0001000b. Entries that the controller does not
// call on this chip are placeholders.
struct osi_funcs {
  uint32_t magic;
  uint32_t version;
  int32_t (*interrupt_alloc)(int32_t, int32_t, isr_fn, void*, void**);
  int32_t (*interrupt_free)(void*);
  uint32_t interrupt_handler_set_rsv;
  void (*global_intr_disable)(void);
  void (*global_intr_restore)(void);
  void (*task_yield)(void);
  void (*task_yield_from_isr)(void);
  void* (*semphr_create)(uint32_t, uint32_t);
  void (*semphr_delete)(void*);
  int32_t (*semphr_take_from_isr)(void*, void*);
  int32_t (*semphr_give_from_isr)(void*, void*);
  int32_t (*semphr_take)(void*, uint32_t);
  int32_t (*semphr_give)(void*);
  void* (*mutex_create)(void);
  void (*mutex_delete)(void*);
  int32_t (*mutex_lock)(void*);
  int32_t (*mutex_unlock)(void*);
  void* (*queue_create)(uint32_t, uint32_t);
  void (*queue_delete)(void*);
  int32_t (*queue_send)(void*, void*, uint32_t);
  int32_t (*queue_send_from_isr)(void*, void*, void*);
  int32_t (*queue_recv)(void*, void*, uint32_t);
  int32_t (*queue_recv_from_isr)(void*, void*, void*);
  int32_t (*task_create)(void*, const char*, uint32_t, void*, uint32_t, void*,
                         uint32_t);
  void (*task_delete)(void*);
  bool (*is_in_isr)(void);
  uint32_t cause_sw_intr_to_core;
  void* (*malloc)(size_t);
  void* (*malloc_internal)(size_t);
  void (*free)(void*);
  int32_t (*read_efuse_mac)(uint8_t*);
  void (*srand)(uint32_t);
  int32_t (*rand)(void);
  uint32_t (*btdm_lpcycles_2_hus)(uint32_t, uint32_t*);
  uint32_t (*btdm_hus_2_lpcycles)(uint32_t);
  bool (*btdm_sleep_check_duration)(int32_t*);
  void (*btdm_sleep_enter_phase1)(uint32_t);
  void (*btdm_sleep_enter_phase2)(void);
  uint32_t btdm_sleep_exit_phase1;
  uint32_t btdm_sleep_exit_phase2;
  void (*btdm_sleep_exit_phase3)(void);
  void (*coex_wifi_sleep_set)(bool);
  uint32_t coex_core_ble_conn_dyn_prio_get;
  int32_t (*coex_schm_register_btdm_callback)(void*);
  void (*coex_schm_status_bit_set)(uint32_t, uint32_t);
  void (*coex_schm_status_bit_clear)(uint32_t, uint32_t);
  uint32_t (*coex_schm_interval_get)(void);
  uint8_t (*coex_schm_curr_period_get)(void);
  void* (*coex_schm_curr_phase_get)(void);
  int32_t (*interrupt_enable)(void*);
  int32_t (*interrupt_disable)(void*);
  void (*esp_hw_power_down)(void);
  void (*esp_hw_power_up)(void);
  void (*ets_backup_dma_copy)(uint32_t, uint32_t, uint32_t, bool);
  void* (*malloc_retention)(size_t);
  void (*ets_delay_us)(uint32_t);
  void (*btdm_rom_table_ready)(void);
  bool (*coex_bt_wakeup_request)(void);
  void (*coex_bt_wakeup_request_end)(void);
  int64_t (*get_time_us)(void);
  void (*assert)(void);
};

// `esp_bt_controller_config_t`, ESP_BT_CTRL_CONFIG_VERSION 0x02509280.
struct controller_config {
  uint32_t magic;
  uint32_t version;
  uint16_t controller_task_stack_size;
  uint8_t controller_task_prio;
  uint8_t controller_task_run_cpu;
  uint8_t bluetooth_mode;
  uint8_t ble_max_act;
  uint8_t sleep_mode;
  uint8_t sleep_clock;
  uint8_t ble_st_acl_tx_buf_nb;
  uint8_t ble_hw_cca_check;
  uint16_t ble_adv_dup_filt_max;
  bool coex_param_en;
  uint8_t ce_len_type;
  bool coex_use_hooks;
  uint8_t hci_tl_type;
  void* hci_tl_funcs;
  uint8_t txant_dft;
  uint8_t rxant_dft;
  uint8_t txpwr_dft;
  uint32_t cfg_mask;
  uint8_t scan_duplicate_mode;
  uint8_t scan_duplicate_type;
  uint16_t normal_adv_size;
  uint16_t mesh_adv_size;
  uint8_t coex_phy_coded_tx_rx_time_limit;
  uint32_t hw_target_code;
  uint8_t slave_ce_len_min;
  uint8_t hw_recorrect_en;
  uint8_t cca_thresh;
  uint16_t scan_backoff_upperlimitmax;
  uint16_t dup_list_refresh_period;
  bool ble_50_feat_supp;
  uint8_t ble_cca_mode;
  uint8_t ble_data_lenth_zero_aux;
  uint8_t ble_chan_ass_en;
  uint8_t ble_ping_en;
  uint8_t ble_llcp_disc_flag;
  bool run_in_flash;
  bool dtm_en;
  bool enc_en;
  bool qa_test;
  bool connect_en;
  bool scan_en;
  bool ble_aa_check;
  bool adv_en;
};

// How the controller hands packets to the host (VHCI).
struct vhci_callbacks {
  void (*notify_host_send_available)(void);
  int (*notify_host_recv)(uint8_t*, uint16_t);
};

#define ESP_BT_MODE_BLE 1
#define PHY_RF_CAL_FULL 2

// The controller (mask ROM and `libbtdm_app.a`).
int btdm_controller_rom_data_init(void);
int btdm_osi_funcs_register(const struct osi_funcs* funcs);
const char* btdm_controller_get_compile_version(void);
bool btdm_lpclk_select_src(uint32_t sel);
bool btdm_lpclk_set_div(uint32_t div);
int btdm_controller_init(struct controller_config* cfg);
int btdm_controller_enable(uint32_t mode);
void coex_pti_v2(void);

bool API_vhci_host_check_send_available(void);
void API_vhci_host_send_packet(const uint8_t* data, uint16_t len);
int API_vhci_host_register_callback(const struct vhci_callbacks* cb);

// The PHY (`libphy.a`).
int register_chipv7_phy(const uint8_t* init, uint8_t* cal, uint32_t mode);
void phy_bbpll_en_usb(bool en);

// Mask ROM.
void ets_delay_us(uint32_t us);
void ets_update_cpu_frequency(uint32_t ticks_per_us);
int ets_vprintf(void (*putc)(char), const char* fmt, va_list ap);

// Install the patched ROM functions for one feature of the controller.
void ble_base_funcs_reset(void);
void ble_42_adv_funcs_reset(void);
void ble_ext_adv_funcs_reset(void);
void ble_dtm_funcs_reset(void);
void ble_scan_funcs_reset(void);
void ble_ext_scan_funcs_reset(void);
void ble_enc_funcs_reset(void);
void ble_init_funcs_reset(void);
void ble_con_funcs_reset(void);
