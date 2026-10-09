# Licensed under the Apache License, Version 2.0 or the MIT License.
# SPDX-License-Identifier: Apache-2.0 OR MIT
# Copyright Tock Contributors 2022.

# Makefile for building the tock kernel for the ESP32-C3 platform

RISC_PREFIX = riscv64-linux-gnu

include ../Makefile.common

# Default target for installing the kernel.
.PHONY: install
install: flash

# Write the kernel to the board with tockloader, which uses esptool to talk to
# the ROM serial bootloader of the ESP32-C3.
.PHONY: flash
flash: $(TARGET_PATH)/release/$(PLATFORM).bin
	tockloader flash --esptool --address 0x42000000 $<

test:
	$(Q)$(CARGO) test $(NO_RUN) --bin $(PLATFORM) --release
