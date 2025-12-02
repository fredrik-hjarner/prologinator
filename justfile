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