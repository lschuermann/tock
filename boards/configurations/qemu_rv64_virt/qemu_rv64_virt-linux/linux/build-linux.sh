#!/usr/bin/env bash

# Licensed under the Apache License, Version 2.0 or the MIT License.
# SPDX-License-Identifier: Apache-2.0 OR MIT
# Copyright Tock Contributors 2026.

# Build the Linux SBI guest: a minimal kernel `Image` with a built-in
# initramfs containing a nolibc `init`, and the guest's device tree.
#
# Usage: build-linux.sh <linux-source-dir> <output-dir>
#
# Requires a riscv64 Linux cross compiler (CROSS_COMPILE, default
# `riscv64-unknown-linux-gnu-`; a bare-metal toolchain cannot link the vDSO),
# the usual kernel build dependencies (make, flex, bison, bc, perl), and dtc.

set -euo pipefail

HERE="$(cd "$(dirname "$0")" && pwd)"
SRC="$(realpath "$1")"
OUT="$(realpath -m "$2")"
CROSS="${CROSS_COMPILE:-riscv64-unknown-linux-gnu-}"
BUILD="$OUT/build"

mkdir -p "$BUILD"
kmake() {
    make -C "$SRC" O="$BUILD" ARCH=riscv CROSS_COMPILE="$CROSS" -j"$(nproc)" "$@"
}

# Kernel configuration: tinyconfig + our fragment.
kmake tinyconfig
"$SRC/scripts/kconfig/merge_config.sh" -m -O "$BUILD" "$BUILD/.config" "$HERE/linux.config"
"$SRC/scripts/config" --file "$BUILD/.config" --set-str INITRAMFS_SOURCE "$BUILD/initramfs.list"
kmake olddefconfig

# Check that the options we rely on survived dependency resolution.
for opt in MMU 64BIT TTY HVC_RISCV_SBI SERIAL_EARLYCON_RISCV_SBI BLK_DEV_INITRD BINFMT_ELF; do
    grep -q "^CONFIG_$opt=y" "$BUILD/.config" || { echo "CONFIG_$opt not enabled" >&2; exit 1; }
done

# Userspace: a static nolibc init.
kmake headers_install INSTALL_HDR_PATH="$OUT/headers"
"${CROSS}gcc" -Os -static -nostdlib -ffreestanding -fno-pic -fno-pie -no-pie \
    -fno-asynchronous-unwind-tables -fno-stack-protector \
    -I "$OUT/headers/include" -include "$SRC/tools/include/nolibc/nolibc.h" \
    -o "$OUT/init" "$HERE/init.c" -lgcc
sed "s|@INIT@|$OUT/init|" "$HERE/initramfs.list" > "$BUILD/initramfs.list"

kmake Image
cp "$BUILD/arch/riscv/boot/Image" "$OUT/Image"
dtc -I dts -O dtb -o "$OUT/guest.dtb" "$HERE/guest.dts"

echo "Built $OUT/Image and $OUT/guest.dtb"
