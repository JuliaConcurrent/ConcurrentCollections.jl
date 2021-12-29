include config.mk

JULIA ?= julia
JULIA_CMD ?= $(JULIA) --color=yes --startup-file=no

export JULIA_LOAD_PATH ?= @
export JULIA_PROJECT ?= $(shell pwd)/..

BENCHMARK_NUM_THREADS ?= $(shell ../nthreads.sh)
BENCHMARK_EXCLUSIVE ?= 0

.PHONY: all benchmark plots print-* dump-*

all: dump-config
	$(MAKE) benchmark
	$(MAKE) plots

benchmark: build/results.json
build/results.json:
	JULIA_NUM_THREADS=$(BENCHMARK_NUM_THREADS) \
	JULIA_EXCLUSIVE=$(BENCHMARK_EXCLUSIVE) \
		$(JULIA_CMD) run.jl

plots: build/results.png
build/results.png:
	$(JULIA_CMD) plot.jl

clean:
	rm -fv build/*.*

backup:
	test -e build/results.json
	mkdir -pv backup
	rm -rf tmp/backup
	mkdir -pv tmp/backup/build
	mv build/* tmp/backup/build/
	mv tmp/backup backup/backup-$$(date +%Y-%m-%d-%H%M%S)

print-%:
	@echo "    "$*=${$*}

dump-config: \
print-JULIA \
print-JULIA_CMD \
print-JULIA_LOAD_PATH \
print-JULIA_PROJECT \
print-BENCHMARK_NUM_THREADS \
print-BENCHMARK_EXCLUSIVE

config.mk:
	touch $@
#	ln -s default-config.mk $@
