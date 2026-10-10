// Licensed under the Apache License, Version 2.0 or the MIT License.
// SPDX-License-Identifier: Apache-2.0 OR MIT
// Copyright Tock Contributors 2026.

// Bring-up of the controller, and the HCI packets to and from it.
//
// One green thread brings the controller up and then hands it the packets
// the kernel sends. Sending can wait for room in the controller's queue,
// which is why it runs on a thread. The controller hands packets for the
// host to `notify_host_recv`, which queues them in `rx_ring`.
//
// The order of the bring-up and the values in the configuration follow
// ESP-IDF's `components/bt/controller/esp32c3/bt.c` and esp-hal's
// `esp-radio`.

#include "glue.h"
#include "libbtdm.h"

#define RTC_CNTL_DIG_PWC 0x60008088
#define RTC_CNTL_DIG_ISO 0x6000808c
#define WIFI_FORCE_PD (1u << 17)
#define BT_FORCE_PD (1u << 11)
#define WIFI_FORCE_ISO (1u << 28)
#define BT_FORCE_ISO (1u << 22)

#define SYSCON_WIFI_CLK_EN 0x60026014
#define SYSCON_WIFI_RST_EN 0x60026018
// Clocks shared by Wi-Fi, BT and the PHY.
#define WIFI_BT_COMMON_CLK 0x0078078fu
// All modem clocks (`SYSTEM_WIFI_CLK_EN`), including the BT baseband (bit 11)
// and link controller (bits 16, 17), which are off out of reset.
#define WIFI_CLK_ALL 0x00fb9fcfu
// SDIO and the unused bit 12, which ESP-IDF leaves off.
#define WIFI_CLK_UNUSED ((1u << 5) | (1u << 12))
// Modem blocks reset when the modem power domain comes up: Wi-Fi BB, RF front
// end, Wi-Fi MAC, BT BB, BT MAC, RW BT MAC, RW BT MAC regs, BT BB regs.
#define MODEM_RESET_WHEN_PU                                              \
  ((1u << 0) | (1u << 1) | (1u << 2) | (1u << 3) | (1u << 4) | (1u << 9) | \
   (1u << 11) | (1u << 13))

// ---------------------------------------------------------------------------
// HCI

// The controller reads a sent packet until it says it is done with it.
static uint8_t tx_buf[PACKET_MAX];
static uint16_t tx_len;
// A packet waits in `tx_buf` for the controller thread to send it.
static bool tx_pending;
// The controller has not yet released the last packet.
static bool tx_in_flight;

static uint8_t rx_buf[2048];
struct ring rx_ring = RING(rx_buf);

static void notify_host_send_available(void) {
  tx_in_flight = false;
}

static int notify_host_recv(uint8_t* data, uint16_t len) {
  if (len > PACKET_MAX || ring_space(&rx_ring) < 2u + len) {
    rx_ring.drops++;
    return 0;
  }
  ring_put(&rx_ring, (uint8_t)len);
  ring_put(&rx_ring, (uint8_t)(len >> 8));
  for (uint16_t i = 0; i < len; i++) {
    ring_put(&rx_ring, data[i]);
  }
  return 0;
}

static const struct vhci_callbacks vhci_callbacks = {
    .notify_host_send_available = notify_host_send_available,
    .notify_host_recv = notify_host_recv,
};

bool controller_tx_ready(void) {
  return controller_started && !tx_pending && !tx_in_flight &&
         API_vhci_host_check_send_available();
}

void controller_send(const uint8_t* packet, uint32_t len) {
  memcpy(tx_buf, packet, len);
  tx_len = (uint16_t)len;
  tx_pending = true;
  tx_in_flight = true;
  threads_wake_all();
}

// ---------------------------------------------------------------------------
// Bring-up

// ESP-IDF's default PHY init data for the ESP32-C3, with a 20 dBm cap.
static const uint8_t phy_init_data[128] = {
    [2] = 0x50,  [3] = 0x50,  [4] = 0x50,  [5] = 0x4c,  [6] = 0x4c,
    [7] = 0x48,  [8] = 0x4c,  [9] = 0x48,  [10] = 0x48, [11] = 0x44,
    [12] = 0x4a, [13] = 0x46, [14] = 0x46, [15] = 0x42,
    [19 ... 69] = 0xff,
    [111] = 0x74,
};

// `esp_phy_calibration_data_t`: version, MAC, opaque data.
static uint8_t phy_cal_data[4 + 6 + 1894];

static void power_up_modem(void) {
  // Undo any forced power-down and isolation of the Wi-Fi/BT domains.
  REG(RTC_CNTL_DIG_ISO) &= ~(WIFI_FORCE_ISO | BT_FORCE_ISO);
  REG(RTC_CNTL_DIG_PWC) &= ~(WIFI_FORCE_PD | BT_FORCE_PD);
  ets_delay_us(10);

  // Reset the modem blocks with their clocks running.
  uint32_t clk = REG(SYSCON_WIFI_CLK_EN);
  REG(SYSCON_WIFI_CLK_EN) = (clk & ~WIFI_CLK_UNUSED) | WIFI_CLK_ALL;
  uint32_t rst = REG(SYSCON_WIFI_RST_EN);
  REG(SYSCON_WIFI_RST_EN) = rst | MODEM_RESET_WHEN_PU;
  REG(SYSCON_WIFI_RST_EN) = rst & ~MODEM_RESET_WHEN_PU;
}

static void enable_phy(void) {
  REG(SYSCON_WIFI_CLK_EN) |= WIFI_BT_COMMON_CLK;
  phy_bbpll_en_usb(true);
  base_mac(&phy_cal_data[4]);
  int ret = register_chipv7_phy(phy_init_data, phy_cal_data, PHY_RF_CAL_FULL);
  log_printf("ble: PHY calibrated (%d)\n", ret);
}

static struct controller_config config = {
    .magic = 0x5a5aa5a5,
    .version = 0x02509280,
    .controller_task_stack_size = 4096,
    .controller_task_prio = 23,
    .controller_task_run_cpu = 0,
    .bluetooth_mode = ESP_BT_MODE_BLE,
    .ble_max_act = 3,
    .ble_adv_dup_filt_max = 30,
    .hci_tl_type = 1,  // VHCI
    .txpwr_dft = 11,   // ESP_PWR_LVL_P9
    .cfg_mask = 1,     // CFG_MASK_BIT_SCAN_DUPLICATE_OPTION
    .normal_adv_size = 100,
    .hw_target_code = 0x01010000,
    .slave_ce_len_min = 5,
    .cca_thresh = 75,
    .ble_50_feat_supp = true,
    .dtm_en = true,
    .enc_en = true,
    .connect_en = true,  // connections, in either role
    .scan_en = true,
    .ble_aa_check = true,
    .adv_en = true,
};

bool controller_started, controller_failed;

static bool bring_up(void) {
  // The ROM's delays count CPU cycles at the rate it was told; it boots
  // assuming the 40 MHz crystal, but the kernel runs the CPU at 160 MHz.
  ets_update_cpu_frequency(160);

  power_up_modem();
  btdm_controller_rom_data_init();

  if (btdm_osi_funcs_register(&osi_funcs) != 0) {
    log_printf("ble: btdm_osi_funcs_register failed\n");
    return false;
  }
  log_printf("ble: controller %s\n", btdm_controller_get_compile_version());

  // Low-power clock: main crystal, divided to 1 MHz.
  if (!(btdm_lpclk_select_src(0) && btdm_lpclk_set_div(40))) {
    log_printf("ble: failed to select the low-power clock\n");
    return false;
  }

  int ret = btdm_controller_init(&config);
  if (ret != 0) {
    log_printf("ble: btdm_controller_init failed: %d\n", ret);
    return false;
  }

  enable_phy();

  if (btdm_controller_enable(ESP_BT_MODE_BLE) != 0) {
    log_printf("ble: btdm_controller_enable failed\n");
    return false;
  }
  coex_pti_v2();

  API_vhci_host_register_callback(&vhci_callbacks);

  log_printf("ble: controller enabled\n");
  return true;
}

// ---------------------------------------------------------------------------
// The controller thread

#define CONTROLLER_STACK_SIZE 4096

static uint32_t controller_stack[CONTROLLER_STACK_SIZE / 4]
    __attribute__((aligned(16)));
struct thread controller_thread;

// The bring-up, then sending the packets the kernel hands over.
static void controller_main(void* arg) {
  (void)arg;
  if (!bring_up()) {
    controller_failed = true;
    return;
  }
  controller_started = true;
  for (;;) {
    while (!tx_pending) {
      thread_wait(-1);
    }
    tx_pending = false;
    API_vhci_host_send_packet(tx_buf, tx_len);
  }
}

void controller_start(void) {
  if (controller_thread.state == THREAD_NONE) {
    thread_create(&controller_thread, controller_stack,
                  sizeof(controller_stack), controller_main, NULL);
  }
}
