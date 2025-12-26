default:
	@just --list

# Simple performance test
perf: build
    time scryer-prolog prolog/perf.pl -g "main_perf, halt"; \

# Run the monolithic build with Scryer Prolog
# Usage: just game <game_name> (e.g., just game default)
game GAME='game0': build
    GAME={{GAME}} scryer-prolog build/prologinator.pl -g "main, halt";

tpl +FLAGS='': build-tpl
       GAME=${GAME:-game0} tpl {{FLAGS}} -g \
         "main" -l build/prologinator.pl

test MODULE: build
    @ VALIDATION_ERR_MSG=false ./scripts/test.ts {{MODULE}}

test-verbose MODULE: build
	@ ./scripts/test.ts {{MODULE}}

# Run tests for a module (quiet output - just pass/fail summary)
# Usage: just test-quiet prolog/execute_action
# TODO: Needs to updated and be a bun script
test-quiet MODULE:
	@base=$(basename "{{MODULE}}") && \
	timeout 12 scryer-prolog -g "use_module('submodules/scryer-prolog/src/tests/test_framework'), use_module('./{{MODULE}}'), main_quiet($base)."

test-all: build
    @ VALIDATION_ERR_MSG=false ./scripts/test-all.ts

test-all-verbose: build
    @ ./scripts/test-all.ts

# Check a Prolog file for syntax errors
# Usage: just lint prolog/engine.pl
lint FILE:
	@! scryer-prolog {{FILE}} -g "halt" 2>&1 | grep -q "error" || { scryer-prolog {{FILE}} -g "halt" 2>&1; echo "ERROR: {{FILE}} has syntax errors"; exit 1; }

# Check all Prolog files for syntax errors
lint-all: build
    @echo "Linting files..."
    @echo "Linting prologinator.pl..."
    @just lint build/prologinator.pl

    # Commented out these because tests now use gpp so
    #     they would need to be processed by gpp first.

    # @echo "Linting engine_test.pl..."
    # @just lint prolog/engine_test.pl || exit 1
    # @echo "Linting types/adv_accessors_fwd_test.pl..."
    # @just lint prolog/types/adv_accessors_fwd_test.pl || exit 1
    # @echo "Linting types/adv_accessors_bwd_test.pl..."
    # @just lint prolog/types/adv_accessors_bwd_test.pl || exit 1
    # @echo "Linting types/validation_test.pl..."
    # @just lint prolog/types/validation_test.pl || exit 1
    # @echo "Linting custom_actions_test.pl..."
    # @just lint prolog/custom_actions_test.pl || exit 1
    # @echo "Linting collisions_fwd_test.pl..."
    # @just lint prolog/collisions_fwd_test.pl || exit 1

    # @echo "Linting actions/__test__/attr_if_fwd_test.pl..."
    # @just lint prolog/actions/__test__/attr_if_fwd_test.pl || exit 1

    # @echo "Linting execute_action_fwd_test.pl..."
    # @just lint prolog/execute_action_fwd_test.pl || exit 1

    # @echo "Linting actions/__test__/move_to_fwd_test.pl..."
    # @just lint prolog/actions/__test__/move_to_fwd_test.pl || exit 1
    # @echo "Linting actions/__test__/trigger_state_change_fwd_test.pl..."
    # @just lint prolog/actions/__test__/trigger_state_change_fwd_test.pl || exit 1
    # @echo "Linting actions/__test__/despawn_fwd_test.pl..."
    # @just lint prolog/actions/__test__/despawn_fwd_test.pl || exit 1
    # @echo "Linting actions/__test__/noop_fwd_test.pl..."
    # @just lint prolog/actions/__test__/noop_fwd_test.pl || exit 1
    # @echo "Linting actions/__test__/list_fwd_test.pl..."
    # @just lint prolog/actions/__test__/list_fwd_test.pl || exit 1
    # @echo "Linting actions/__test__/set_attr_fwd_test.pl..."
    # @just lint prolog/actions/__test__/set_attr_fwd_test.pl || exit 1
    # @echo "Linting actions/__test__/parallel_race_fwd_test.pl..."
    # @just lint prolog/actions/__test__/parallel_race_fwd_test.pl || exit 1
    # @echo "Linting actions/__test__/repeat_fwd_test.pl..."
    # @just lint prolog/actions/__test__/repeat_fwd_test.pl || exit 1
    # @echo "Linting actions/__test__/move_delta_fwd_test.pl..."
    # @just lint prolog/actions/__test__/move_delta_fwd_test.pl || exit 1
    # @echo "Linting actions/__test__/value_resolution_fwd_test.pl..."
    # @just lint prolog/actions/__test__/value_resolution_fwd_test.pl || exit 1
    # @echo "Linting actions/__test__/wait_key_down_fwd_test.pl..."
    # @just lint prolog/actions/__test__/wait_key_down_fwd_test.pl || exit 1
    # @echo "Linting actions/__test__/wait_key_up_fwd_test.pl..."
    # @just lint prolog/actions/__test__/wait_key_up_fwd_test.pl || exit 1
    # @echo "Linting actions/__test__/wait_key_held_fwd_test.pl..."
    # @just lint prolog/actions/__test__/wait_key_held_fwd_test.pl || exit 1
    # @echo "Linting actions/__test__/wait_until_fwd_test.pl..."
    # @just lint prolog/actions/__test__/wait_until_fwd_test.pl || exit 1
    @echo "All files passed linting!"

# lint the max length of files.
lint-len: build
    MAX_LENGTH=60 bun scripts/max-lens.ts

# lint the number of lines in each file.
# NOTE: optional. I don't strictly enforce this.
lint-lines: build
    MAX_LINES=500 bun scripts/max-lines.ts

# Run sconcat script to concatenate files
# Usage: just sconcat [output_file]
# If output_file is not provided, defaults to concat.xml
sconcat OUTPUT_FILE="concat.xml":
	bun scripts/sconcat/main.ts {{OUTPUT_FILE}}

# Run CI pipeline: lint-all then test-all
# Fails if any step fails
ci: build
    just lint-len
    just lint-all
    just test-all

aider:
    # aider --no-git --no-gitignore
    aider --no-git --no-gitignore --model gemini/gemini-flash-lite-latest
    # aider --no-git --no-gitignore --model gemini/gemini-flash-latest
    # aider --no-git --no-gitignore --model gemini/gemini-3-flash-preview
    # aider --no-git --no-gitignore --model gemini/gemini-2.5-flash-lite
    # aider --no-git --no-gitignore --model gemini/gemini-2.5-pro
    # aider --no-git --no-gitignore --model gemini/gemini-3-pro-preview

# Generate call graph for all Prolog files
# Usage: just callgraph
callgraph:
	bun scripts/call_graph/callgraph.ts

# Build monolithic Prolog file from preprocessor source
# Usage: just build
# Outputs: build/prologinator.pl
build:
	@mkdir -p build
	@gpp -P --warninglevel 0 scripts/prologinator.pp -o build/prologinator.pl
	@echo "Built: build/prologinator.pl"

build-tpl:
	@mkdir -p build
	@gpp -DTPL -P --warninglevel 0 scripts/prologinator.pp -o build/prologinator.pl
	@echo "Built: build/prologinator.pl"
