default:
	@just --list

# Run the game with Scryer Prolog
# Usage: just game (uses games/default)
# Usage: just game ./games/my_game
# Usage: just game ./games/my_game ./games/my_input
game GAME='games/default' INPUTS='games/input_demo.pl':
	@if [ -z "{{INPUTS}}" ]; then \
		GAME={{GAME}} scryer-prolog prolog/game.pl -g "main"; \
	else \
		GAME={{GAME}} INPUTS={{INPUTS}} scryer-prolog prolog/game.pl -g "main"; \
	fi

# Run tests for a module (verbose output)
# Usage: just test prolog/execute_action
# Usage: just test prolog/engine_test (for test files with _test.pl suffix)
# Usage: just test some/other/path/my_module
# The MODULE argument should be the file path (without .pl extension)
# The module name will be extracted from the last path component
test MODULE:
	@base=$(basename "{{MODULE}}") && \
	VALIDATION_ERR_MSG=false timeout 10 scryer-prolog -g "catch((use_module('submodules/scryer-prolog/src/tests/test_framework'), use_module('./{{MODULE}}'), main($base)), Error, (format(\"Uncaught exception: ~w~n\", [Error]), halt(1)))." 2>&1 | tee /tmp/just_test_output.txt; \
	if grep -q "Failed test" /tmp/just_test_output.txt; then rm -f /tmp/just_test_output.txt; exit 1; fi; \
	rm -f /tmp/just_test_output.txt

test-verbose MODULE:
	@base=$(basename "{{MODULE}}") && \
	timeout 10 scryer-prolog -g "catch((use_module('submodules/scryer-prolog/src/tests/test_framework'), use_module('./{{MODULE}}'), main($base)), Error, (format(\"Uncaught exception: ~w~n\", [Error]), halt(1)))." 2>&1 | tee /tmp/just_test_output.txt; \
	if grep -q "Failed test" /tmp/just_test_output.txt; then rm -f /tmp/just_test_output.txt; exit 1; fi; \
	rm -f /tmp/just_test_output.txt

# Run tests for a module (quiet output - just pass/fail summary)
# Usage: just test-quiet prolog/execute_action
test-quiet MODULE:
	@base=$(basename "{{MODULE}}") && \
	timeout 10 scryer-prolog -g "use_module('submodules/scryer-prolog/src/tests/test_framework'), use_module('./{{MODULE}}'), main_quiet($base)."

test-all:
    @echo "\nTesting execute_action_fwd_test..."
    @just test prolog/execute_action_fwd_test || exit 1
    @echo "\nTesting execute_action_bwd_test..."
    @just test prolog/execute_action_bwd_test || exit 1
    @echo "\nTesting engine_test..."
    @just test prolog/engine_test || exit 1
    @echo "\nTesting yields_test..."
    @just test prolog/yields_test || exit 1
    @echo "\nTesting validation_test..."
    @just test prolog/types/validation_test || exit 1
    @echo "\nTesting validation2..."
    @just test prolog/types/validation2 || exit 1
    @echo "\nTesting macros..."
    @just test prolog/macros || exit 1
    @echo "\nTesting xod..."
    @just test prolog/xod/xod || exit 1

test-all-verbose:
    @echo "\nTesting execute_action_fwd_test..."
    @just test-verbose prolog/execute_action_fwd_test || exit 1
    @echo "\nTesting execute_action_bwd_test..."
    @just test-verbose prolog/execute_action_bwd_test || exit 1
    @echo "\nTesting engine_test..."
    @just test-verbose prolog/engine_test || exit 1
    @echo "\nTesting yields_test..."
    @just test-verbose prolog/yields_test || exit 1
    @echo "\nTesting validation_test..."
    @just test-verbose prolog/types/validation_test || exit 1
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
    @echo "Linting engine.pl..."
    @just lint prolog/engine.pl || exit 1
    @echo "Linting engine_test.pl..."
    @just lint prolog/engine_test.pl || exit 1
    @echo "Linting game.pl..."
    @just lint prolog/game.pl || exit 1
    @echo "Linting types/constraints.pl..."
    @just lint prolog/types/constraints.pl || exit 1
    @echo "Linting types/accessors.pl..."
    @just lint prolog/types/accessors.pl || exit 1
    @echo "Linting resolve_action.pl..."
    @just lint prolog/resolve_action.pl || exit 1
    @echo "Linting builtin_actions.pl..."
    @just lint prolog/builtin_actions.pl || exit 1
    @echo "Linting execute_action.pl..."
    @just lint prolog/execute_action.pl || exit 1
    @echo "Linting execute_action_fwd_test.pl..."
    @just lint prolog/execute_action_fwd_test.pl || exit 1
    @echo "Linting execute_action_bwd_test.pl..."
    @just lint prolog/execute_action_bwd_test.pl || exit 1
    @echo "Linting collisions.pl..."
    @just lint prolog/collisions.pl || exit 1
    @echo "Linting types/validation.pl..."
    @just lint prolog/types/validation.pl || exit 1
    @echo "Linting macros.pl..."
    @just lint prolog/macros.pl || exit 1
    @echo "Linting test_macros.pl..."
    @just lint prolog/test_macros.pl || exit 1
    @echo "Linting yields.pl..."
    @just lint prolog/yields.pl || exit 1
    @echo "Linting yields_test.pl..."
    @just lint prolog/yields_test.pl || exit 1
    @echo "Linting xod/xod.pl..."
    @just lint prolog/xod/xod.pl || exit 1
    @echo "All files passed linting!"

# lint the max length of files.
lint-len:
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/engine.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/engine_test.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/resolve_action.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/builtin_actions.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/execute_action.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/execute_action_fwd_test.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/execute_action_bwd_test.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/collisions.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/game.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/macros.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/test_macros.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/types/accessors.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/types/constraints.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/types/validation.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/yields.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/yields_test.pl
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

aider:
    aider --no-gitignore