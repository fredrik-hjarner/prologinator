default:
	@just --list

# Run the game with Scryer Prolog
game:
	scryer-prolog prolog/game.pl -g "main"

# Run tests for a module (verbose output)
# Usage: just test prolog/execute_action
# Usage: just test prolog/engine
# Usage: just test some/other/path/my_module
# The MODULE argument should be the file path (without .pl extension)
# The module name will be extracted from the last path component
test MODULE:
	@base=$(basename "{{MODULE}}") && \
	scryer-prolog -g "use_module('submodules/scryer-prolog/src/tests/test_framework'), use_module('./{{MODULE}}'), main($base)."

# Run tests for a module (quiet output - just pass/fail summary)
# Usage: just test-quiet prolog/execute_action
test-quiet MODULE:
	@base=$(basename "{{MODULE}}") && \
	scryer-prolog -g "use_module('submodules/scryer-prolog/src/tests/test_framework'), use_module('./{{MODULE}}'), main_quiet($base)."

test-all:
	@echo "\nTesting execute_action..."
	@just test prolog/execute_action
	@echo "\nTesting engine..."
	@just test prolog/engine

# Check a Prolog file for syntax errors
# Usage: just lint prolog/engine.pl
lint FILE:
	@! scryer-prolog {{FILE}} -g "halt" 2>&1 | grep -q "error" || { scryer-prolog {{FILE}} -g "halt" 2>&1; echo "ERROR: {{FILE}} has syntax errors"; exit 1; }

# Check all Prolog files for syntax errors
lint-all:
	@echo "Linting prolog files..."
	@just lint prolog/engine.pl
	@just lint prolog/game.pl
	@just lint prolog/types.pl
	@just lint prolog/execute_action.pl
	@echo "All files passed linting!"

# Run CI pipeline: lint-all then test-all
# Fails if any step fails
ci:
	@just lint-all
	@just test-all