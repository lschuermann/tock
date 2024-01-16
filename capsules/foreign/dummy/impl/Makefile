# RISC-V toolchains, irrespective of their name-tuple, can compile for
# essentially any target. Thus, try a few known names and choose the one for
# which a compiler is found.
ifneq (,$(shell which riscv64-none-elf-gcc 2>/dev/null))
  TOOLCHAIN_rv32i := riscv64-none-elf
else ifneq (,$(shell which riscv32-none-elf-gcc 2>/dev/null))
  TOOLCHAIN_rv32i := riscv32-none-elf
else ifneq (,$(shell which riscv64-elf-gcc 2>/dev/null))
  TOOLCHAIN_rv32i := riscv64-elf
else ifneq (,$(shell which riscv64-unknown-elf-clang 2>/dev/null))
  TOOLCHAIN_rv32i := riscv64-unknown-elf
else ifneq (,$(shell which riscv32-unknown-elf-clang 2>/dev/null))
  TOOLCHAIN_rv32i := riscv32-unknown-elf
else
  # $(error Failed to find a suitable RISC-V toolchain.)
endif

TOOLCHAIN_cortexm = arm-none-eabi

# CC := $(TOOLCHAIN_rv32i)-gcc
# CXX := $(TOOLCHAIN_rv32i)-g++
# AS := $(TOOLCHAIN_rv32i)-as
# LD := $(TOOLCHAIN_rv32i)-ld

# ARCH := rv32imc
# CFLAGS := -march=rv32imac -mabi=ilp32
# ASFLAGS := $(CFLAGS)
# CXXFLAGS := $(CFLAGS)
# LDFLAGS := -melf32lriscv
# INIT_S := ../../encapfn-rt/init_riscv32.S


CC := $(TOOLCHAIN_cortexm)-gcc
CXX := $(TOOLCHAIN_cortexm)-g++
AS := $(TOOLCHAIN_cortexm)-as
LD := $(TOOLCHAIN_cortexm)-ld

ARCH := cortex-m4
CFLAGS := -std=gnu11
ASFLAGS := -mthumb
CXXFLAGS := $(CFLAGS)
LDFLAGS :=
INIT_S := ../../encapfn-rt/init_cortexm.S


SVCNAME := dummyfn
BUILDDIR ?= build

# SRC := $(foreach x, ./, $(wildcard $(addprefix $(x)/*,.c*)))
CSRC := $(foreach x, ./, $(wildcard $(addprefix $(x)/*,.c)))
COBJ := $(addprefix $(BUILDDIR)/, $(addsuffix .c.o, $(notdir $(basename $(CSRC)))))
ASSRC := $(foreach x, ./, $(wildcard $(addprefix $(x)/*,.S))) $(INIT_S)
ASOBJ := $(addprefix $(BUILDDIR)/, $(addsuffix .S.o, $(notdir $(basename $(ASSRC)))))

.PHONY: all
all: $(BUILDDIR)/$(SVCNAME).tab

.PHONY: clean
clean:
	rm -rf build

$(BUILDDIR):
	mkdir -p $(BUILDDIR)

$(BUILDDIR)/%.c.o: %.c* | $(BUILDDIR)
	$(CC) $(CFLAGS) -o $@ -g -O -c $<

$(BUILDDIR)/init_riscv32.S.o: ../../encapfn-rt/init_riscv32.S | $(BUILDDIR)
	$(AS) $(ASFLAGS) -o $@ -g -c $<

$(BUILDDIR)/init_cortexm.S.o: ../../encapfn-rt/init_cortexm.S | $(BUILDDIR)
	$(AS) $(ASFLAGS) -o $@ -g -c $<

$(BUILDDIR)/%.S.o: %.S* | $(BUILDDIR)
	$(AS) $(ASFLAGS) -o $@ -g -c $<

$(BUILDDIR)/$(SVCNAME).elf: $(COBJ) $(ASOBJ) layout.ld | $(BUILDDIR)
	$(LD) -o $@ $(COBJ) $(ASOBJ) -Tlayout.ld $(LDFLAGS)

$(BUILDDIR)/$(SVCNAME).tab: $(BUILDDIR)/$(SVCNAME).elf | $(BUILDDIR)
	elf2tab --verbose --minimum-footer-size 3000 --disable -o $@ -n $(SVCNAME) $<,$(ARCH)

# $(BUILDDIR)/$(SVCNAME).tbf: $(BUILDDIR)/$(SVCNAME).tab | $(BUILDDIR)
# 	tar -C$(BUILDDIR) -xf $< $(ARCH).tbf

