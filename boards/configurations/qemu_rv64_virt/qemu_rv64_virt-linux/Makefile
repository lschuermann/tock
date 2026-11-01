# Licensed under the Apache License, Version 2.0 or the MIT License.
# SPDX-License-Identifier: Apache-2.0 OR MIT
# Copyright Tock Contributors 2026.

# Runs Linux as an SBI guest of Tock, next to regular Tock processes, on the
# qemu-system-riscv64 `virt` machine. See README.md.

include ../../../Makefile.common

QEMU_CMD := qemu-system-riscv64

# Must match the guest memory region and DTB address in src/main.rs.
LINUX_IMAGE_ADDR := 0x80400000
LINUX_DTB_ADDR   := 0x87000000

# Built by linux/build-linux.sh.
LINUX_DIR ?= $(TARGET_DIRECTORY)linux-guest

QEMU_CMDLINE := \
  $(QEMU_CMD) \
    -machine virt \
    -m 128M \
    -semihosting \
    -global virtio-mmio.force-legacy=false \
    -nographic \
    -bios $(TARGET_PATH)/release/$(PLATFORM).elf \
    -device loader,file=$(LINUX_DIR)/Image,addr=$(LINUX_IMAGE_ADDR) \
    -device loader,file=$(LINUX_DIR)/guest.dtb,addr=$(LINUX_DTB_ADDR)

# Run Tock with the Linux guest, and optionally a Tock app (APP=<path to .tbf>).
.PHONY: run
run: $(TARGET_PATH)/release/$(PLATFORM).elf
	@echo "To exit type C-a x"
	$(QEMU_CMDLINE) $(if $(APP),-device loader$(comma)file=$(APP)$(comma)addr=0x80100000)

comma := ,
