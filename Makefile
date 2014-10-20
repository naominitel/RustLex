RUSTC = rustc
BUILD = build
CTEST_FLAGS = \
	--target x86_64-apple-darwin \
	--target-rustcflags "-Lbuild" \
	--compile-lib-path . \
	--run-lib-path . \
	--rustc-path $(shell which rustc) \
	--build-base build/tests \
	--aux-base . \
	--stage-id 0

all: $(BUILD) rustlex

$(BUILD):
	mkdir -pv $(BUILD)/

$(BUILD)/tests:
	mkdir -pv $(BUILD)/tests/

rustlex:
	$(RUSTC) rustlex.rs --out-dir $(BUILD)/

#check: all build/tests build/compiletest compile-fail run-pass
check: all build/tests compile-fail run-pass
	
#build/compiletest:
#	$(RUSTC) tests/compiletest/compiletest.rs --out-dir $(BUILD)/

compile-fail:
	./$(BUILD)/compiletest $(CTEST_FLAGS) --mode $@ --src-base tests/$@

run-pass:
	./$(BUILD)/compiletest $(CTEST_FLAGS) --mode $@ --src-base tests/$@
