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
              [attr(x, 0), attr(y, 0),
               attr(target_x, 100),
               attr(target_y, 200)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    ObjIn = object(
        id(1),
        type(static),
        actions([move_to(attr(target_x),
                         attr(target_y), 5)]),
        collisions([])
    ),
    execute_action(
        action(move_to(attr(target_x),
                       attr(target_y), 5)),
        obj_old(ObjIn),
        result(_, _ObjOut),
        Ctx,
        CtxNew
    ),
    % Should start moving toward target
    ctx_attr_val(CtxNew, 1/x, NewX),
    ctx_attr_val(CtxNew, 1/y, NewY),
    NewX > 0,
    NewY > 0
)).

test("value_resolution: set_attr with attr() source", (
    % Copy one attribute to another
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(x, 50), attr(y, 75),
               attr(source_x, 0)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    ObjIn = object(
        id(1),
        type(static),
        actions([set_attr(source_x, attr(x))]),
        collisions([])
    ),
    execute_action(
        action(set_attr(source_x, attr(x))),
        obj_old(ObjIn),
        result(_, _ObjOut),
        Ctx,
        CtxNew
    ),
    % source_x should now equal x
    ctx_attr_val(CtxNew, 1/source_x, 50),
    ctx_attr_val(CtxNew, 1/x, 50)
)).

test("value_resolution: path syntax parent_id/target_y", (
    % Object references another object's attribute via path
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(x, 0), attr(y, 0),
               attr(parent_id, 2)],
              Attrs1),
    put_assoc(2, Attrs1,
              [attr(x, 100), attr(y, 200),
               attr(target_y, 250)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    ObjIn = object(
        id(1),
        type(static),
        actions([set_attr(my_target_y,
                          attr(parent_id/target_y))]),
        collisions([])
    ),
    execute_action(
        action(set_attr(my_target_y,
                        attr(parent_id/target_y))),
        obj_old(ObjIn),
        result(_, _ObjOut),
        Ctx,
        CtxNew
    ),
    % Should copy parent's target_y (250) to my_target_y
    ctx_attr_val(CtxNew, 1/my_target_y, 250),
    ctx_attr_val(CtxNew, 2/target_y, 250)
)).

test("value_resolution: multi-hop path a/b/c", (
    % Navigate through multiple object IDs
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(x, 0), attr(y, 0),
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
    ObjIn = object(
        id(1),
        type(static),
        actions([set_attr(result,
                          attr(first_id/second_id/
                                final_value))]),
        collisions([])
    ),
    execute_action(
        action(set_attr(result,
                        attr(first_id/second_id/
                              final_value))),
        obj_old(ObjIn),
        result(_, _ObjOut),
        Ctx,
        CtxNew
    ),
    % Should navigate: 1 -> first_id(2) -> second_id(3) ->
    % final_value(999)
    ctx_attr_val(CtxNew, 1/result, 999)
)).

test("value_resolution: spawn at attr() position", (
    % Spawn object at position stored in attributes
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(x, 50), attr(y, 100),
               attr(spawn_x, 200),
               attr(spawn_y, 300)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    ObjIn = object(
        id(1),
        type(static),
        actions([spawn(enemy, attr(spawn_x),
                       attr(spawn_y), [])]),
        collisions([])
    ),
    execute_action(
        action(spawn(enemy, attr(spawn_x),
                     attr(spawn_y), [])),
        obj_old(ObjIn),
        result(_, _ObjOut),
        Ctx,
        CtxNew
    ),
    % Should spawn enemy at (200, 300) - spawn immediately
    % creates object, so check it exists in context
    ctx_objs(Objects, CtxNew),
    member(object(id(_ID), type(enemy), _, _), Objects),
    ctx_attr_val(CtxNew, _ID/x, 200),
    ctx_attr_val(CtxNew, _ID/y, 300)
)).

test("value_resolution: mixed plain and attr() values", (
    % Mix plain values with attribute references
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(x, 0), attr(y, 0),
               attr(target_x, 100),
               attr(speed, 5)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    ObjIn = object(
        id(1),
        type(static),
        actions([move_to(attr(target_x), 200,
                         attr(speed))]),
        collisions([])
    ),
    execute_action(
        action(move_to(attr(target_x), 200,
                       attr(speed))),
        obj_old(ObjIn),
        result(_, _ObjOut),
        Ctx,
        CtxNew
    ),
    % Should move toward target (100, 200) with speed=5
    ctx_attr_val(CtxNew, 1/x, NewX),
    ctx_attr_val(CtxNew, 1/y, NewY),
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
              [attr(x, 0), attr(y, 0)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    ObjIn = object(
        id(1),
        type(static),
        actions([move_to(100, 200, 5)]),
        collisions([])
    ),
    execute_action(
        action(move_to(100, 200, 5)),
        obj_old(ObjIn),
        result(_, _ObjOut),
        Ctx,
        CtxNew
    ),
    % Should work exactly as before
    ctx_attr_val(CtxNew, 1/x, NewX),
    ctx_attr_val(CtxNew, 1/y, NewY),
    NewX > 0,
    NewY > 0
)).

