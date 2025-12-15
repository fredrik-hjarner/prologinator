:- module(wait_key_held_fwd_test, []).

:- use_module('../../..'/build/prologinator).
:- use_module('../../..'/prolog/util/test_util).

:- use_module(library(assoc), [
    empty_assoc/1,
    put_assoc/4
]).

% ==========================================================
% Tests: wait_key_held Action (Forward)
% ==========================================================

test("wait_key_held: completes when key is held", (
    % --------------------------------------------------
    % Arrange
    % --------------------------------------------------
    ObjIn = object(
        id(1),
        type(static),
        actions([wait_key_held(39), noop]),
        collisions([])
    ),
    % Create context with key 39 held
    Ctx = ctx(
        state(
            frame(0),
            objects([]),
            attrs(EmptyAttrs),
            status(playing),
            next_id(1),
            commands([])
        ),
        input(events([]), held([39]))
    ),
    empty_attr_store(EmptyAttrs),
    % --------------------------------------------------
    % Act
    % --------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(wait_key_held(39)),
        obj_old(ObjIn),
        result(Status, ObjOut)
    ),
    % --------------------------------------------------
    % Assert
    % --------------------------------------------------
    expect(Status = completed, 'Status != completed'),
    expect(obj_acns(ObjOut, [noop]),
        'Actions != [noop]'),
    expect(ctx_cmds([], CtxNew), 'Commands != []')
)).

test("wait_key_held: yields when key not held", (
    % --------------------------------------------------
    % Arrange
    % --------------------------------------------------
    ObjIn = object(
        id(1),
        type(static),
        actions([wait_key_held(39), noop]),
        collisions([])
    ),
    % Create context with no keys held
    empty_ctx(Ctx),
    % --------------------------------------------------
    % Act
    % --------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(wait_key_held(39)),
        obj_old(ObjIn),
        result(Status, ObjOut)
    ),
    % --------------------------------------------------
    % Assert
    % --------------------------------------------------
    expect(Status = yielded, 'Status != yielded'),
    expect(obj_acns(ObjOut, [wait_key_held(39), noop]),
        'Action not preserved'),
    expect(ctx_cmds([], CtxNew), 'Commands != []')
)).

test("wait_key_held: waits for different key", (
    % --------------------------------------------------
    % Arrange
    % --------------------------------------------------
    ObjIn = object(
        id(1),
        type(static),
        actions([wait_key_held(37)]),
        collisions([])
    ),
    % Create context with key 39 held (not 37)
    Ctx = ctx(
        state(
            frame(0),
            objects([]),
            attrs(EmptyAttrs),
            status(playing),
            next_id(1),
            commands([])
        ),
        input(events([]), held([39]))
    ),
    empty_attr_store(EmptyAttrs),
    % --------------------------------------------------
    % Act
    % --------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(_),
        action(wait_key_held(37)),
        obj_old(ObjIn),
        result(Status, ObjOut)
    ),
    % --------------------------------------------------
    % Assert
    % --------------------------------------------------
    expect(Status = yielded,
        'Should yield when different key held'),
    expect(obj_acns(ObjOut, [wait_key_held(37)]),
        'Action should remain in queue')
)).

test("wait_key_held: multiple keys held", (
    % --------------------------------------------------
    % Arrange
    % --------------------------------------------------
    ObjIn = object(
        id(1),
        type(static),
        actions([wait_key_held(37)]),
        collisions([])
    ),
    % Create context with multiple keys held
    Ctx = ctx(
        state(
            frame(0),
            objects([]),
            attrs(EmptyAttrs),
            status(playing),
            next_id(1),
            commands([])
        ),
        input(events([]), held([37, 39, 65]))
    ),
    empty_attr_store(EmptyAttrs),
    % --------------------------------------------------
    % Act
    % --------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(_),
        action(wait_key_held(37)),
        obj_old(ObjIn),
        result(Status, ObjOut)
    ),
    % --------------------------------------------------
    % Assert
    % --------------------------------------------------
    expect(Status = completed,
        'Should complete when target key held'),
    expect(obj_acns(ObjOut, []),
        'Action should be removed')
)).

test("wait_key_held: in loop pattern", (
    % --------------------------------------------------
    % Arrange
    % --------------------------------------------------
    ObjIn = object(
        id(1),
        type(static),
        actions([
            loop([wait_key_held(39), set_attr(x, 10)])
        ]),
        collisions([])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(
        1, EmptyAttrs0, [attr(x, 0)], EmptyAttrs
    ),
    % Context without key held
    Ctx = ctx(
        state(
            frame(0),
            objects([]),
            attrs(EmptyAttrs),
            status(playing),
            next_id(1),
            commands([])
        ),
        input(events([]), held([]))
    ),
    % --------------------------------------------------
    % Act
    % --------------------------------------------------
    tick_object(
        ctx_old(Ctx),
        ctx_new(Ctx1),
        obj_old(ObjIn),
        result(Status1, Obj1)
    ),
    % --------------------------------------------------
    % Assert
    % --------------------------------------------------
    % Should yield waiting for key
    expect(Status1 = yielded, 'Should yield'),
    expect(ctx_attr_val(Ctx1, 1/x, 0),
        'x should still be 0'),
    % Loop expands body and adds itself back
    expect(obj_acns(Obj1, [
        wait_key_held(39),
        set_attr(x, 10),
        loop([wait_key_held(39), set_attr(x, 10)])
    ]), 'Loop should expand and remain')
)).

