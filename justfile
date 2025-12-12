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

test MODULE:
    @ VALIDATION_ERR_MSG=false ./scripts/test.ts {{MODULE}}

test-verbose MODULE:
	@ ./scripts/test.ts {{MODULE}}

# Run tests for a module (quiet output - just pass/fail summary)
# Usage: just test-quiet prolog/execute_action
# TODO: Needs to updated and be a bun script
test-quiet MODULE:
	@base=$(basename "{{MODULE}}") && \
	timeout 12 scryer-prolog -g "use_module('submodules/scryer-prolog/src/tests/test_framework'), use_module('./{{MODULE}}'), main_quiet($base)."

test-all:
    @ VALIDATION_ERR_MSG=false ./scripts/test-all.ts

test-all-verbose:
    @ ./scripts/test-all.ts

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
    @echo "Linting types/adv_accessors.pl..."
    @just lint prolog/types/adv_accessors.pl || exit 1
    @echo "Linting types/constructors.pl..."
    @just lint prolog/types/constructors.pl || exit 1
    @echo "Linting types/validation2.pl..."
    @just lint prolog/types/validation2.pl || exit 1
    @echo "Linting types/adv_accessors_fwd_test.pl..."
    @just lint prolog/types/adv_accessors_fwd_test.pl || exit 1
    @echo "Linting types/adv_accessors_bwd_test.pl..."
    @just lint prolog/types/adv_accessors_bwd_test.pl || exit 1
    @echo "Linting types/validation_test.pl..."
    @just lint prolog/types/validation_test.pl || exit 1
    @echo "Linting util/util.pl..."
    @just lint prolog/util/util.pl || exit 1
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
    @echo "Linting collisions.pl..."
    @just lint prolog/collisions.pl || exit 1
    @echo "Linting types/validation.pl..."
    @just lint prolog/types/validation.pl || exit 1
    @echo "Linting macros.pl..."
    @just lint prolog/macros.pl || exit 1
    @echo "Linting test_macros.pl..."
    @just lint prolog/test_macros.pl || exit 1
    @echo "Linting xod/xod.pl..."
    @just lint prolog/xod/xod.pl || exit 1
    @echo "Linting actions/wait.pl..."
    @just lint prolog/actions/wait.pl || exit 1
    @echo "Linting actions/move_to.pl..."
    @just lint prolog/actions/move_to.pl || exit 1
    @echo "Linting actions/move_delta.pl..."
    @just lint prolog/actions/move_delta.pl || exit 1
    @echo "Linting actions/despawn.pl..."
    @just lint prolog/actions/despawn.pl || exit 1
    @echo "Linting actions/noop.pl..."
    @just lint prolog/actions/noop.pl || exit 1
    @echo "Linting actions/define_action.pl..."
    @just lint prolog/actions/define_action.pl || exit 1
    @echo "Linting actions/set_attr.pl..."
    @just lint prolog/actions/set_attr.pl || exit 1
    @echo "Linting actions/incr.pl..."
    @just lint prolog/actions/incr.pl || exit 1
    @echo "Linting actions/decr.pl..."
    @just lint prolog/actions/decr.pl || exit 1
    @echo "Linting actions/log.pl..."
    @just lint prolog/actions/log.pl || exit 1
    @echo "Linting actions/spawn.pl..."
    @just lint prolog/actions/spawn.pl || exit 1
    @echo "Linting actions/loop.pl..."
    @just lint prolog/actions/loop.pl || exit 1
    @echo "Linting actions/list.pl..."
    @just lint prolog/actions/list.pl || exit 1
    @echo "Linting actions/repeat.pl..."
    @just lint prolog/actions/repeat.pl || exit 1
    @echo "Linting actions/load.pl..."
    @just lint prolog/actions/load.pl || exit 1
    @echo "Linting actions/trigger_state_change.pl..."
    @just lint prolog/actions/trigger_state_change.pl || exit 1
    @echo "Linting actions/wait_key_down.pl..."
    @just lint prolog/actions/wait_key_down.pl || exit 1
    @echo "Linting actions/wait_key_up.pl..."
    @just lint prolog/actions/wait_key_up.pl || exit 1
    @echo "Linting actions/wait_key_held.pl..."
    @just lint prolog/actions/wait_key_held.pl || exit 1
    @echo "Linting actions/parallel_all.pl..."
    @just lint prolog/actions/parallel_all.pl || exit 1
    @echo "Linting actions/parallel_race.pl..."
    @just lint prolog/actions/parallel_race.pl || exit 1
    @echo "All files passed linting!"

# lint the max length of files.
lint-len:
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/engine.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/engine_test.pl
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
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/collisions.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/game.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/macros.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/test_macros.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/types/accessors.pl
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
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/actions/parallel_all.pl
    MAX_LENGTH=60 bun scripts/max-len.ts prolog/actions/parallel_race.pl

# Run sconcat script to concatenate files
# Usage: just sconcat [output_file]
# If output_file is not provided, defaults to concat.xml
sconcat OUTPUT_FILE="concat.xml":
	bun scripts/sconcat/main.ts {{OUTPUT_FILE}}

# Run CI pipeline: lint-all then test-all
# Fails if any step fails
ci:
    just lint-len
    just lint-all
    just test-all

aider:
    aider --no-gitignore

# Generate call graph for all Prolog files
# Usage: just callgraph
callgraph:
	bun scripts/call_graph/callgraph.ts