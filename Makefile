ELF_DIR := $(shell nix build --no-link --print-out-paths .#riscv-tests)/riscv32-unknown-elf/share/riscv-tests/isa/
WAVE_DIR := ./wave
RV32I_ELFS := $(notdir $(shell find $(ELF_DIR) -name 'rv32ui-p-*' ! -name "*.dump"))
LOG_LEVEL := ERROR
DUMP_START := 0
DUMP_END := 10000
SIM_ARGS := +log-level=$(LOG_LEVEL) +dump-start=$(DUMP_START) +dump-end=$(DUMP_END)

RV32I_OBJS := $(RV32I_ELFS:%=%.o)
TEST_RESULTS := success.log failure.log

.PHONY: rv32_i clean

rv32_i: $(RV32I_OBJS)
	@echo "===================="
	@echo "TEST SUMMARY:"
	@echo "Total:  $$(($$(wc -l < success.log 2>/dev/null || echo 0) + $$(wc -l < failure.log 2>/dev/null || echo 0)))"
	@echo "Passed: $$(wc -l < success.log 2>/dev/null || echo 0)"
	@echo "Failed: $$(wc -l < failure.log 2>/dev/null || echo 0)"
	@if [ -s failure.log ]; then \
		echo "Failed cases:"; \
		cat failure.log; \
		false; \
	fi
	@rm failure.log success.log

%.o: $(ELF_DIR)/%
	@mkdir -p $(WAVE_DIR)
	@echo "[TEST] $(notdir $<) start"
	@if nix run .#hia.verilated-trace -- +elf-file=$< +wave-path=$(WAVE_DIR)/$(notdir $<).fst $(SIM_ARGS); then \
		echo "$(notdir $<)" >> success.log; \
		echo "[PASS] $(notdir $<)"; \
	else \
		echo "$(notdir $<)" >> failure.log; \
		echo "[FAIL] $(notdir $<)"; \
	fi

clean:
	rm -rf $(WAVE_DIR) $(TEST_RESULTS)
