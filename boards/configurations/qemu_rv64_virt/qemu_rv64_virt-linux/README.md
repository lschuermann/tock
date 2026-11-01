QEMU RISC-V 64-bit `virt` Platform: Linux as an SBI Process
===========================================================

This board configuration runs a Linux kernel as a process of Tock, next to
regular Tock processes. Linux runs in supervisor mode with its own virtual
memory, and uses Tock as its SBI implementation (console, timer, and a few
single-hart basics). See `arch/riscv/src/sbi` for the implementation.

Memory layout (QEMU provides 128 MiB of RAM at `0x80000000`):

| Region                    | Use                                     |
|---------------------------|-----------------------------------------|
| `0x80000000 - 0x80400000` | Tock kernel, app flash, kernel/app RAM  |
| `0x80400000 - 0x88000000` | Linux (its device tree is at `0x87000000`) |

Building the Linux guest
------------------------

`linux/build-linux.sh <linux-source> <output-dir>` builds a minimal kernel
(`tinyconfig` + `linux/linux.config`) with a built-in initramfs containing a
small nolibc `init` (`linux/init.c`), and the device tree (`linux/guest.dts`).
It requires a riscv64 Linux cross compiler, the usual kernel build
dependencies, and `dtc`. By default, the Makefile expects its output in
`target/linux-guest`:

    linux/build-linux.sh ~/src/linux target/linux-guest

Running
-------

    make run                      # Tock + Linux
    make run APP=path/to/app.tbf  # Tock + a Tock app + Linux
