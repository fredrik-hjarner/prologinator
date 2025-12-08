default:
	@just --list

# Run the game with Scryer Prolog
game:
	scryer-prolog prolog/game.pl -g "main"

# Run tests for a module (verbose output)
# Usage: just test prolog/execute_action
# Usage: just test prolog/engine_test (for test files with _test.pl suffix)
# Usage: just test some/other/path/my_module
# The MODULE argument should be the file path (without .pl extension)
# The module name will be extracted from the last path component
test MODULE:
	@base=$(basename "{{MODULE}}") && \
	VALIDATION_ERR_MSG=false scryer-prolog -g "catch((use_module('submodules/scryer-prolog/src/tests/test_framework'), use_module('./{{MODULE}}'), main($base)), Error, (format(\"Uncaught exception: ~w~n\", [Error]), halt(1)))." 2>&1 | tee /tmp/just_test_output.txt; \
	if grep -q "Failed test" /tmp/just_test_output.txt; then rm -f /tmp/just_test_output.txt; exit 1; fi; \
	rm -f /tmp/just_test_output.txt

test-verbose MODULE:
	@base=$(basename "{{MODULE}}") && \
	scryer-prolog -g "catch((use_module('submodules/scryer-prolog/src/tests/test_framework'), use_module('./{{MODULE}}'), main($base)), Error, (format(\"Uncaught exception: ~w~n\", [Error]), halt(1)))." 2>&1 | tee /tmp/just_test_output.txt; \
	if grep -q "Failed test" /tmp/just_test_output.txt; then rm -f /tmp/just_test_output.txt; exit 1; fi; \
	rm -f /tmp/just_test_output.txt

# Run tests for a module (quiet output - just pass/fail summary)
# Usage: just test-quiet prolog/execute_action
test-quiet MODULE:
	@base=$(basename "{{MODULE}}") && \
	scryer-prolog -g "use_module('submodules/scryer-prolog/src/tests/test_framework'), use_module('./{{MODULE}}'), main_quiet($base)."

test-all:
    @echo "\nTesting execute_action..."
    @just test prolog/execute_action_test || exit 1
    @echo "\nTesting engine..."
    @just test prolog/engine_test || exit 1
    @echo "\nTesting validation..."
    @just test prolog/types/validation || exit 1
    @echo "\nTesting validation2..."
    @just test prolog/types/validation2 || exit 1
    @echo "\nTesting macros..."
    @just test prolog/macros || exit 1
    @echo "\nTesting xod..."
    @just test prolog/xod/xod || exit 1

test-all-verbose:
    @echo "\nTesting execute_action..."
    @just test-verbose prolog/execute_action_test || exit 1
    @echo "\nTesting engine..."
    @just test-verbose prolog/engine_test || exit 1
    @echo "\nTesting validation..."
    @just test-verbose prolog/types/validation || exit 1
    @echo "\nTesting validation2..."
    @just test-verbose prolog/types/validation2 || exit 1
    @echo "\nTesting macros..."
    @just test-verbose prolog/macros || exit 1
    @echo "\nTesting xod..."
    @just test-verbose prolog/xod/xod || exit 1

# Check a Prolog file for syntax errors
# Usage: just lint prolog/engine.pl
lint FILE:
	@! scryer-prolog {{FILE}} -g "halt" 2>&1 | grep -q "error" || { scryer-prolog {{FILE}} -g "halt" 2>&1; echo "ERROR: {{FILE}} has syntax errors"; exit 1; }

# Check all Prolog files for syntax errors
lint-all:
    @echo "Linting prolog files..."
    @just lint prolog/engine.pl || exit 1
    @just lint prolog/engine_test.pl || exit 1
    @just lint prolog/game.pl || exit 1
    @just lint prolog/types/constraints.pl || exit 1
    @just lint prolog/types/accessors.pl || exit 1
    @just lint prolog/execute_action.pl || exit 1
    @just lint prolog/execute_action_test.pl || exit 1
    @just lint prolog/collisions.pl || exit 1
    @just lint prolog/types/validation.pl || exit 1
    @just lint prolog/macros.pl || exit 1
    @just lint prolog/test_macros.pl || exit 1
    @just lint prolog/xod/xod.pl || exit 1
    @echo "All files passed linting!"

# lint the max length of files.
lint-len:
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/engine.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/engine_test.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/execute_action.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/execute_action_test.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/collisions.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/game.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/macros.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/test_macros.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/types/accessors.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/types/constraints.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/types/validation.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/xod/xod.pl

# Run sconcat script to concatenate files
# Usage: just sconcat [output_file]
# If output_file is not provided, defaults to concat.xml
sconcat OUTPUT_FILE="concat.xml":
	bun scripts/sconcat/sconcat.ts {{OUTPUT_FILE}}

# Run CI pipeline: lint-all then test-all
# Fails if any step fails
ci:
    just lint-len
    just lint-all
    just test-all