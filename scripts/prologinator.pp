% ==========================================================
% THE PROLOGINATOR MONOLITH BUILD
% ==========================================================

:- module(prologinator, [
    action_validation/1,
    action_validation_helper/1,
    builtin_action/1,
    command_validation/1,
    context_validation/1,
    ctx_attr_val/3,
    ctx_attr_val_ctx/4,
    ctx_attrs/2,
    ctx_attrs_ctx/3,
    ctx_cmds/2,
    ctx_cmds_ctx/3,
    ctx_events/2,
    ctx_frame/2,
    ctx_frame_ctx/3,
    ctx_held/2,
    ctx_input/2,
    ctx_input_ctx/3,
    ctx_nextid/2,
    ctx_nextid_cmds_ctx/4,
    ctx_nextid_ctx/3,
    ctx_objs/2,
    ctx_objs_attrs/3,
    ctx_objs_attrs_ctx/4,
    ctx_objs_cmds/3,
    ctx_objs_cmds_ctx/4,
    ctx_objs_ctx/3,
    ctx_objs_nextid_cmds/4,
    ctx_objs_nextid_cmds_ctx/5,
    ctx_state/2,
    ctx_state_ctx/3,
    ctx_status/2,
    ctx_status_cmds/3,
    ctx_status_cmds_ctx/4,
    ctx_status_ctx/3,
    ctx_with_attrs/2,
    ctx_with_frame_attrs/3,
    detect_collisions/2,
    empty_attr_store/1,
    empty_ctx/1,
    execute_action/5,
    execute_action_impl/5,
    execute_action_resolved/5,
    execute_decr/6,
    execute_define_action/5,
    execute_despawn/3,
    execute_incr/6,
    execute_list/4,
    execute_load/4,
    execute_log/4,
    execute_loop/4,
    execute_move_delta/9,
    execute_move_to/9,
    execute_noop/3,
    execute_parallel_all/5,
    execute_parallel_race/6,
    execute_parallel_race_running/6,
    execute_repeat/5,
    execute_set_attr/6,
    execute_set_attr/7,
    execute_spawn/8,
    execute_trigger_state_change/5,
    execute_wait/5,
    execute_wait_key_down/5,
    execute_wait_key_held/5,
    execute_wait_key_up/5,
    game_status_validation/1,
    is_list/1,
    main/0,
    obj_acns/2,
    obj_acns_obj/3,
    obj_collisions/2,
    obj_id/2,
    obj_id_type/3,
    obj_type/2,
    object_type_validation/1,
    object_validation/1,
    object_validation_helper/1,
    pos_validation/1,
    resolve_action/4,
    resolve_arg/4,
    resolve_path/4,
    spawn_request_validation/1,
    state_attrs/2,
    state_attrs_state/3,
    state_change_validation/1,
    state_change_validation_helper/1,
    state_status_state/3,
    state_validation/1,
    state_validation_helper/1,
    tick/2,
    tick_object/4,
    user_action/2
]).

% 1. Global Imports (Import all libraries ONCE)
#include "../prolog/imports.pl"

% 1.5. Discontiguous Declarations (must be before any clauses)
#include "../prolog/discontiguous.pl"

% 1.6. Dynamic Declarations (must be before any clauses)
#include "../prolog/dynamic.pl"

% 2. Types (foundational - no dependencies on game logic)
#include "../prolog/types/accessors.pl"
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
#include "../prolog/actions/incr.pl"
#include "../prolog/actions/decr.pl"
#include "../prolog/actions/log.pl"
#include "../prolog/actions/spawn.pl"
#include "../prolog/actions/loop.pl"
#include "../prolog/actions/list.pl"
#include "../prolog/actions/repeat.pl"
#include "../prolog/actions/load.pl"
#include "../prolog/actions/trigger_state_change.pl"
#include "../prolog/actions/wait_key_down.pl"
#include "../prolog/actions/wait_key_up.pl"
#include "../prolog/actions/wait_key_held.pl"
#include "../prolog/actions/parallel_all.pl"
#include "../prolog/actions/parallel_race.pl"

% 7. Core Engine Components
#include "../prolog/tick_object.pl"
#include "../prolog/collisions.pl"
#include "../prolog/input_helpers.pl"

% 8. Main Engine
#include "../prolog/engine.pl"

% 9. Game Entry Point
#include "../prolog/game.pl"
