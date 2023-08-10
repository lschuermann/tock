CC := riscv64-unknown-elf-gcc
CXX := riscv64-unknown-elf-g++
AS := riscv64-unknown-elf-as
# LD := riscv64-unknown-elf-ld
LD := ld.lld

ARCH := rv32imc
CFLAGS := -march=rv32imac -mabi=ilp32
ASFLAGS := $(CFLAGS)
CXXFLAGS := $(CFLAGS)
LDFLAGS := -melf32lriscv --lto-O3

SVCNAME := dummyfn
BUILDDIR ?= build

# SRC := $(foreach x, ./, $(wildcard $(addprefix $(x)/*,.c*)))
CSRC := $(foreach x, ./, $(wildcard $(addprefix $(x)/*,.c)))
COBJ := $(addprefix $(BUILDDIR)/, $(addsuffix .c.o, $(notdir $(basename $(CSRC)))))
ASSRC := $(foreach x, ./, $(wildcard $(addprefix $(x)/*,.S))) ../../encapfn-rt/init_riscv32.S
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
	$(AS) $(CFLAGS) -o $@ -g -c $<

$(BUILDDIR)/%.S.o: %.S* | $(BUILDDIR)
	$(AS) $(CFLAGS) -o $@ -g -c $<

$(BUILDDIR)/$(SVCNAME).elf: $(COBJ) $(ASOBJ) layout.ld | $(BUILDDIR)
	$(LD) -o $@ $(COBJ) $(ASOBJ) -Tlayout.ld $(LDFLAGS)

$(BUILDDIR)/$(SVCNAME).tab: $(BUILDDIR)/$(SVCNAME).elf | $(BUILDDIR)
	elf2tab --verbose --minimum-footer-size 3000 --disable -o $@ -n $(SVCNAME) $<,$(ARCH)

# $(BUILDDIR)/$(SVCNAME).tbf: $(BUILDDIR)/$(SVCNAME).tab | $(BUILDDIR)
# 	tar -C$(BUILDDIR) -xf $< $(ARCH).tbf

