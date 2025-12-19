% ==========================================================
% THE PROLOGINATOR MONOLITH BUILD
% ==========================================================

:- module(prologinator, [
#include "../prolog/exports.pl"
]).

% 1. Global Imports (Import all libraries ONCE)
#include "../prolog/imports.pl"

% 1.5. Discontiguous Declarations (must be before any clauses)
#include "../prolog/discontiguous.pl"

% 1.6. Dynamic Declarations (must be before any clauses)
#include "../prolog/dynamic.pl"

% 2. Types (foundational - no dependencies on game logic)
#include "../prolog/types/accessors/ctx.pl"
#include "../prolog/types/accessors/obj.pl"
#include "../prolog/types/constructors.pl"
% #include "../prolog/types/constraints.pl" % no dont import
#include "../prolog/types/validation.pl"
#include "../prolog/types/adv_accessors.pl"
% #include "../prolog/types/validation2.pl" % no dont import

% 3. Utilities and Macros
#include "../prolog/util/util.pl"
% #include "../prolog/util/test_util.pl" % no dont import
% #include "../prolog/macros.pl" % no dont import
% #include "../prolog/test_macros.pl" % no dont import

% 4. Action Resolution and Builtins
#include "../prolog/resolve_action.pl"
#include "../prolog/builtin_actions.pl"

% 5. Execute Action (declares multifile, must come before actions)
#include "../prolog/execute_action.pl"

% 6. Action Implementations (all actions)
#include "../prolog/actions/wait.pl"
#include "../prolog/actions/move_to.pl"
#include "../prolog/actions/move_delta.pl"
#include "../prolog/actions/despawn.pl"
#include "../prolog/actions/noop.pl"
#include "../prolog/actions/define_action.pl"
#include "../prolog/actions/set_attr.pl"
#include "../prolog/actions/copy_attr.pl"
#include "../prolog/actions/incr.pl"
#include "../prolog/actions/decr.pl"
#include "../prolog/actions/log.pl"
#include "../prolog/actions/spawn.pl"
#include "../prolog/actions/fork.pl"
#include "../prolog/actions/loop.pl"
#include "../prolog/actions/list.pl"
#include "../prolog/actions/repeat.pl"
#include "../prolog/actions/load.pl"
#include "../prolog/actions/trigger_state_change.pl"
#include "../prolog/actions/wait_key_down.pl"
#include "../prolog/actions/wait_key_up.pl"
#include "../prolog/actions/wait_key_held.pl"
#include "../prolog/actions/wait_until.pl"
#include "../prolog/actions/parallel_all.pl"
#include "../prolog/actions/parallel_race.pl"

% 7. Core Engine Components
#include "../prolog/tick_action_streams.pl"
#include "../prolog/tick_object.pl"
#include "../prolog/collisions.pl"
#include "../prolog/input_helpers.pl"

% 8. Main Engine
#include "../prolog/engine.pl"

% 9. Game Entry Point
#include "../prolog/game.pl"
