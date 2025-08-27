# Use OS-agnostic path separator
ifeq ($(OS),Windows_NT)
    PATH_SEP = \\
    TSC = $(shell cygpath -w $(CURDIR)\lua_modules\bin\tsc.bat)
    PATHSEP = \\
else
    PATH_SEP = /
    # 使用 $(CURDIR) 来确保路径是绝对的
    TSC = $(CURDIR)/lua_modules/bin/tsc
    PATHSEP = /
endif

# Directories
TEST_DIR = test
SRC_DIR = src

# Automatically find all test files
TEST_FILES := $(wildcard $(TEST_DIR)$(PATHSEP)*_spec.lua)
TEST_TARGETS := $(notdir $(basename $(TEST_FILES)))

# Main file
MAIN_FILE = $(SRC_DIR)$(PATHSEP)main.lua

# Debug info (uncomment to debug)
# $(info TEST_FILES = $(TEST_FILES))
# $(info TEST_TARGETS = $(TEST_TARGETS))

# General TSC command for any file
tsc:
	$(TSC) -f $(file)

# Run all tests
test: $(TEST_TARGETS)
	@echo "All tests completed"

# 修改：使用函数来运行特定测试，而不是模式规则
define run_test
	@echo "Running test: $(TEST_DIR)$(PATHSEP)$(1).lua"
	@if [ ! -f "$(TEST_DIR)$(PATHSEP)$(1).lua" ]; then \
		echo "Error: Test file $(TEST_DIR)$(PATHSEP)$(1).lua not found"; \
		exit 1; \
	fi
	$(TSC) -f $(TEST_DIR)$(PATHSEP)$(1).lua
endef

# 为每个测试创建显式规则
$(TEST_TARGETS):
	$(call run_test,$@)

main:
	@lua $(MAIN_FILE)

repl:
	pwsh.exe abcl.ps1

# Clean target
clean:
	@echo "Cleaning..."
	# Add commands to clean temporary files if needed

# Mark targets as phony
.PHONY: tsc test main clean repl $(TEST_TARGETS)

# Help target
help:
	@echo "Available targets:"
	@echo "  test          - Run all tests"
	@echo "  <name>_spec   - Run specific test (e.g., make lexer_spec)"
	@echo "  main          - Run main program"
	@echo "  repl          - Start REPL"
	@echo "  clean         - Clean temporary files"
	@echo "  help          - Show this help message"
	@echo ""
	@echo "Available test targets:"
	@echo "  $(TEST_TARGETS)"

.PHONY: help