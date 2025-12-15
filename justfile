default:
	@just --list

# Run the monolithic build with Scryer Prolog
# Usage: just game (uses games/default, games/input_demo.pl)
# Usage: just game ./games/my_game
# Usage: just game ./games/my_game ./games/my_input
# Usage: just game ./games/my_game '' (no input timeline)
game GAME='games/default' INPUTS='games/input_demo.pl': build
	@if [ -z "{{INPUTS}}" ]; then \
		GAME={{GAME}} scryer-prolog build/prologinator.pl -g "main, halt"; \
	else \
		GAME={{GAME}} INPUTS={{INPUTS}} scryer-prolog build/prologinator.pl -g "main, halt"; \
	fi

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
lint-all:
    @echo "Linting files..."
    @echo "Linting prologinator.pl..."
    @just lint build/prologinator.pl
    @echo "Linting engine_test.pl..."
    @just lint prolog/engine_test.pl || exit 1
    @echo "Linting types/adv_accessors_fwd_test.pl..."
    @just lint prolog/types/adv_accessors_fwd_test.pl || exit 1
    @echo "Linting types/adv_accessors_bwd_test.pl..."
    @just lint prolog/types/adv_accessors_bwd_test.pl || exit 1
    @echo "Linting types/validation_test.pl..."
    @just lint prolog/types/validation_test.pl || exit 1
    @echo "Linting custom_actions_test.pl..."
    @just lint prolog/custom_actions_test.pl || exit 1
    @echo "Linting collisions_fwd_test.pl..."
    @just lint prolog/collisions_fwd_test.pl || exit 1
    @echo "Linting execute_action_fwd_test.pl..."
    @just lint prolog/execute_action_fwd_test.pl || exit 1
    @echo "Linting input_helpers.pl..."
    @just lint prolog/input_helpers.pl || exit 1
    @echo "Linting resolve_action.pl..."
    @just lint prolog/resolve_action.pl || exit 1
    @echo "Linting builtin_actions.pl..."
    @just lint prolog/builtin_actions.pl || exit 1
    @echo "Linting execute_action.pl..."
    @just lint prolog/execute_action.pl || exit 1
    @echo "Linting actions/wait_until.pl..."
    @just lint prolog/actions/wait_until.pl || exit 1
    @echo "Linting actions/__test__/move_to_fwd_test.pl..."
    @just lint prolog/actions/__test__/move_to_fwd_test.pl || exit 1
    @echo "Linting actions/__test__/trigger_state_change_fwd_test.pl..."
    @just lint prolog/actions/__test__/trigger_state_change_fwd_test.pl || exit 1
    @echo "Linting actions/__test__/despawn_fwd_test.pl..."
    @just lint prolog/actions/__test__/despawn_fwd_test.pl || exit 1
    @echo "Linting actions/__test__/noop_fwd_test.pl..."
    @just lint prolog/actions/__test__/noop_fwd_test.pl || exit 1
    @echo "Linting actions/__test__/list_fwd_test.pl..."
    @just lint prolog/actions/__test__/list_fwd_test.pl || exit 1
    @echo "Linting actions/__test__/set_attr_fwd_test.pl..."
    @just lint prolog/actions/__test__/set_attr_fwd_test.pl || exit 1
    @echo "Linting actions/__test__/parallel_race_fwd_test.pl..."
    @just lint prolog/actions/__test__/parallel_race_fwd_test.pl || exit 1
    @echo "Linting actions/__test__/repeat_fwd_test.pl..."
    @just lint prolog/actions/__test__/repeat_fwd_test.pl || exit 1
    @echo "Linting actions/__test__/move_delta_fwd_test.pl..."
    @just lint prolog/actions/__test__/move_delta_fwd_test.pl || exit 1
    @echo "Linting actions/__test__/value_resolution_fwd_test.pl..."
    @just lint prolog/actions/__test__/value_resolution_fwd_test.pl || exit 1
    @echo "Linting actions/__test__/wait_key_down_fwd_test.pl..."
    @just lint prolog/actions/__test__/wait_key_down_fwd_test.pl || exit 1
    @echo "Linting actions/__test__/wait_key_up_fwd_test.pl..."
    @just lint prolog/actions/__test__/wait_key_up_fwd_test.pl || exit 1
    @echo "Linting actions/__test__/wait_key_held_fwd_test.pl..."
    @just lint prolog/actions/__test__/wait_key_held_fwd_test.pl || exit 1
    @echo "Linting actions/__test__/wait_until_fwd_test.pl..."
    @just lint prolog/actions/__test__/wait_until_fwd_test.pl || exit 1
    @echo "All files passed linting!"

# lint the max length of files.
lint-len:
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/engine.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/engine_test.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/tick_object.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/resolve_action.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/builtin_actions.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/execute_action.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/actions/__test__/move_to_fwd_test.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/actions/__test__/trigger_state_change_fwd_test.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/actions/__test__/despawn_fwd_test.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/actions/__test__/noop_fwd_test.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/actions/__test__/list_fwd_test.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/actions/__test__/set_attr_fwd_test.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/actions/__test__/parallel_race_fwd_test.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/actions/__test__/repeat_fwd_test.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/actions/__test__/move_delta_fwd_test.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/actions/__test__/value_resolution_fwd_test.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/actions/__test__/wait_key_down_fwd_test.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/actions/__test__/wait_key_up_fwd_test.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/actions/__test__/wait_key_held_fwd_test.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/actions/__test__/wait_until_fwd_test.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/collisions.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/game.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/macros.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/test_macros.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/types/accessors/obj.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/types/accessors/ctx.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/types/adv_accessors.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/types/constructors.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/types/constraints.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/types/validation.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/types/validation2.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/types/adv_accessors_fwd_test.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/types/adv_accessors_bwd_test.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/types/validation_test.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/util/util.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/custom_actions_test.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/collisions_fwd_test.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/execute_action_fwd_test.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/input_helpers.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/xod/xod.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/actions/wait.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/actions/move_to.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/actions/move_delta.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/actions/despawn.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/actions/noop.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/actions/define_action.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/actions/set_attr.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/actions/incr.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/actions/decr.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/actions/log.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/actions/spawn.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/actions/loop.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/actions/list.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/actions/repeat.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/actions/load.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/actions/trigger_state_change.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/actions/wait_key_down.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/actions/wait_key_up.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/actions/wait_key_held.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/actions/wait_until.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/actions/parallel_all.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/actions/parallel_race.pl

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
    aider --no-gitignore

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