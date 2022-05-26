include config.mk

JULIA = julia
JULIA_CMD ?= $(JULIA) --color=yes --startup-file=no

export JULIA_LOAD_PATH ?= @
export JULIA_PROJECT ?= $(shell pwd)

SUB_MAKEFILES = $(wildcard */Makefile)
BENCHMARK_TARGETS = $(patsubst %/Makefile, benchmark-%, $(SUB_MAKEFILES))
PLOTS_TARGETS = $(patsubst %/Makefile, plots-%, $(SUB_MAKEFILES))
DUMP_CONFIG_TARGETS = $(patsubst %/Makefile, dump-config-%, $(SUB_MAKEFILES))

.PHONY: benchmark* reinstantiate repl

all: dump-config
	$(MAKE) benchmark
	$(MAKE) plots

benchmark: $(BENCHMARK_TARGETS)
$(BENCHMARK_TARGETS): benchmark-%: Manifest.toml
	$(MAKE) -C $* benchmark

plots: $(PLOTS_TARGETS)
$(PLOTS_TARGETS): plots-%: Manifest.toml
	$(MAKE) -C $* plots

dump-config: $(DUMP_CONFIG_TARGETS)
$(DUMP_CONFIG_TARGETS): dump-config-%:
	$(MAKE) -C $* dump-config

Manifest.toml:
	JULIA_LOAD_PATH=@:@stdlib $(JULIA_CMD) --project=. instantiate.jl

reinstantiate:
	rm -f Manifest.toml
	$(MAKE) Manifest.toml

repl:
	JULIA_LOAD_PATH=: $(JULIA)

config.mk:
	touch $@
#	ln -s default-config.mk $@
