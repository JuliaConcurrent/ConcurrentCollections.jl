JULIA = julia1.7
JULIA_CMD ?= $(JULIA) --color=yes --startup-file=no

export JULIA_PROJECT = $(shell pwd)/../../test/ConcurrentCollectionsTests
# export JULIA_LOAD_PATH = @

.PHONY: benchmark clean backup

BUILD = build

benchmark: $(BUILD)/results.json

$(BUILD)/results.json:
	$(JULIA_CMD) -t16 run.jl

clean:
	rm -fv $(BUILD)/*.json

backup:
	test -e $(BUILD)/results.json
	mkdir -pv backup
	rm -rf tmp/backup
	mkdir -pv tmp/backup/build
	mv $(BUILD)/* tmp/backup/build/
	mv tmp/backup backup/backup-$$(date +%Y-%m-%d-%H%M%S)
