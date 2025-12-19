:- module(value_resolution_fwd_test, []).

:- use_module('../../../build/prologinator').
:- use_module(library(lists), [member/2]).
:- use_module(library(assoc), [
    empty_assoc/1,
    put_assoc/4
]).
:- use_module(library(format)).
% ==========================================================
% Forward Tests (all inputs ground, normal use case)
% ==========================================================

% ==========================================================
% Tests: Value Resolution (attr() syntax)
% ==========================================================

test("value_resolution: move_to with attr() references", (
    % Object moves to position stored in its attributes
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(type, static), attr(x, 0), attr(y, 0),
               attr(target_x, 100),
               attr(target_y, 200)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    ActionsIn = [move_to(attr(target_x),
                         attr(target_y), 5)],
    execute_action(
        action(move_to(attr(target_x),
                       attr(target_y), 5)),
        actions_old(ActionsIn),
        obj_id(1),
        result(_, actions_new(_)),
        Ctx,
        CtxNew
    ),
    % Should start moving toward target
    ctx_attr_val(1/x, NewX, CtxNew, CtxNew),
    ctx_attr_val(1/y, NewY, CtxNew, CtxNew),
    NewX > 0,
    NewY > 0
)).

test("value_resolution: set_attr with attr() source", (
    % Copy one attribute to another
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(type, static), attr(x, 50), attr(y, 75),
               attr(source_x, 0)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    ActionsIn = [set_attr(source_x, attr(x))],
    execute_action(
        action(set_attr(source_x, attr(x))),
        actions_old(ActionsIn),
        obj_id(1),
        result(_, actions_new(_)),
        Ctx,
        CtxNew
    ),
    % source_x should now equal x
    ctx_attr_val(1/source_x, 50, CtxNew, CtxNew),
    ctx_attr_val(1/x, 50, CtxNew, CtxNew)
)).

test("value_resolution: path syntax parent_id/target_y", (
    % Object references another object's attribute via path
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(type, static), attr(x, 0), attr(y, 0),
               attr(parent_id, 2)],
              Attrs1),
    put_assoc(2, Attrs1,
              [attr(x, 100), attr(y, 200),
               attr(target_y, 250)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    ActionsIn = [set_attr(my_target_y,
                          attr(parent_id/target_y))],
    execute_action(
        action(set_attr(my_target_y,
                        attr(parent_id/target_y))),
        actions_old(ActionsIn),
        obj_id(1),
        result(_, actions_new(_)),
        Ctx,
        CtxNew
    ),
    % Should copy parent's target_y (250) to my_target_y
    ctx_attr_val(1/my_target_y, 250, CtxNew, CtxNew),
    ctx_attr_val(2/target_y, 250, CtxNew, CtxNew)
)).

test("value_resolution: multi-hop path a/b/c", (
    % Navigate through multiple object IDs
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(type, static), attr(x, 0), attr(y, 0),
               attr(first_id, 2)],
              Attrs1),
    put_assoc(2, Attrs1,
              [attr(x, 10), attr(y, 20),
               attr(second_id, 3)],
              Attrs2),
    put_assoc(3, Attrs2,
              [attr(x, 100), attr(y, 200),
               attr(final_value, 999)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    ActionsIn = [set_attr(result,
                          attr(first_id/second_id/
                                final_value))],
    execute_action(
        action(set_attr(result,
                        attr(first_id/second_id/
                              final_value))),
        actions_old(ActionsIn),
        obj_id(1),
        result(_, actions_new(_)),
        Ctx,
        CtxNew
    ),
    % Should navigate: 1 -> first_id(2) -> second_id(3) ->
    % final_value(999)
    ctx_attr_val(1/result, 999, CtxNew, CtxNew)
)).

test("value_resolution: spawn at attr() position", (
    % Spawn object at position stored in attributes
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(type, static),
               attr(x, 50), attr(y, 100),
               attr(spawn_x, 200),
               attr(spawn_y, 300)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    ActionsIn = [spawn(enemy, attr(spawn_x),
                       attr(spawn_y), [])],
    execute_action(
        action(spawn(enemy, attr(spawn_x),
                     attr(spawn_y), [])),
        actions_old(ActionsIn),
        obj_id(1),
        result(_, actions_new(_)),
        Ctx,
        CtxNew
    ),
    % Check that spawn command was created with resolved
    % values (attr() references should be resolved to 200,
    % 300)
    ctx_spawnCmds(SpawnCmds, CtxNew, CtxNew),
    % Should have one spawn command with resolved values
    SpawnCmds = [spawn_cmd(actions([set_attr(type, enemy),
                                     set_attr(x, 200),
                                     set_attr(y, 300)]))]
)).

test("value_resolution: mixed plain and attr() values", (
    % Mix plain values with attribute references
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(type, static), attr(x, 0), attr(y, 0),
               attr(target_x, 100),
               attr(speed, 5)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    ActionsIn = [move_to(attr(target_x), 200,
                         attr(speed))],
    execute_action(
        action(move_to(attr(target_x), 200,
                       attr(speed))),
        actions_old(ActionsIn),
        obj_id(1),
        result(_, actions_new(_)),
        Ctx,
        CtxNew
    ),
    % Should move toward target (100, 200) with speed=5
    ctx_attr_val(1/x, NewX, CtxNew, CtxNew),
    ctx_attr_val(1/y, NewY, CtxNew, CtxNew),
    NewX > 0,
    NewY > 0
)).

% Note: list() actions are resolved when the list action
% itself is resolved, but testing this is complex because
% list() doesn't execute actions immediately. The other
% tests verify that attr() resolution works correctly for
% all action types.

test("value_resolution: backward compatible plain values", (
    % Plain values still work without attr() wrapper
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(type, static), attr(x, 0), attr(y, 0)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    ActionsIn = [move_to(100, 200, 5)],
    execute_action(
        action(move_to(100, 200, 5)),
        actions_old(ActionsIn),
        obj_id(1),
        result(_, actions_new(_ActionsOut)),
        Ctx,
        CtxNew
    ),
    % Should work exactly as before
    ctx_attr_val(1/x, NewX, CtxNew, CtxNew),
    ctx_attr_val(1/y, NewY, CtxNew, CtxNew),
    NewX > 0,
    NewY > 0
)).

