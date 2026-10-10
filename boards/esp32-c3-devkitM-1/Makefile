# Licensed under the Apache License, Version 2.0 or the MIT License.
# SPDX-License-Identifier: Apache-2.0 OR MIT
# Copyright Tock Contributors 2022.

# Makefile for building the tock kernel for the ESP32-C3 platform, with
# Espressif's BLE controller in an Omniglot sandbox
# (`chips/esp32-c3-ble-omniglot`). The sandbox is linked separately
# (`make ble-sandbox`) and merged into the kernel binary.

RISC_PREFIX = riscv64-linux-gnu

include ../Makefile.common

# Default target for installing the kernel.
.PHONY: install
install: flash

include $(TOCK_ROOT_DIRECTORY)chips/esp32-c3-ble-omniglot/sandbox.mk

# The kernel binary with the sandbox written into it at `_ble_load_start`
# (layout.ld, offset 0x70000), which is what `make` builds and `make flash`
# writes. With BLE_SANDBOX=0, both use the kernel alone, which then runs
# without BLE.
BLE_SANDBOX ?= 1
KERNEL_BLE := $(TARGET_PATH)/release/$(PLATFORM)-ble

$(KERNEL_BLE).bin: $(TARGET_PATH)/release/$(PLATFORM).bin $(BLE_SANDBOX_BIN)
	cp $< $@
	dd if=$(word 2,$^) of=$@ bs=4096 seek=$$((0x70000 / 4096)) conv=notrunc status=none

ifeq ($(BLE_SANDBOX),1)
KERNEL_BIN := $(KERNEL_BLE).bin
else
KERNEL_BIN := $(TARGET_PATH)/release/$(PLATFORM).bin
endif

release: $(KERNEL_BIN)

# Write the kernel (and the sandbox) to the board with tockloader, which uses
# esptool to talk to the ROM serial bootloader of the ESP32-C3. Apps start
# at 0x42100000 and are not touched.
.PHONY: flash
flash: $(KERNEL_BIN)
	tockloader flash --esptool --address 0x42000000 $<

test:
	$(Q)$(CARGO) test $(NO_RUN) --bin $(PLATFORM) --release
