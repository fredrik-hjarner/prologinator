:- module(wait_key_down_fwd_test, []).

:- use_module('../../..'/build/prologinator).
:- use_module('../../..'/prolog/util/test_util).

:- use_module(library(assoc), [
    empty_assoc/1,
    put_assoc/4
]).

% ==========================================================
% Tests: wait_key_down Action (Forward)
% ==========================================================

test("wait_key_down: completes when key pressed", (
    % --------------------------------------------------
    % Arrange
    % --------------------------------------------------
    ObjIn = object(
        id(1),
        type(static),
        actions([wait_key_down(39), noop]),
        collisions([])
    ),
    % Create context with key 39 down event
    Ctx = ctx(
        state(
            frame(0),
            objects([]),
            attrs(EmptyAttrs),
            status(playing),
            next_id(1),
            commands([])
        ),
        input(events([event(key(39), down)]), held([]))
    ),
    empty_attr_store(EmptyAttrs),
    % --------------------------------------------------
    % Act
    % --------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(wait_key_down(39)),
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

test("wait_key_down: yields when key not pressed", (
    % --------------------------------------------------
    % Arrange
    % --------------------------------------------------
    ObjIn = object(
        id(1),
        type(static),
        actions([wait_key_down(39), noop]),
        collisions([])
    ),
    % Create context with no key events
    empty_ctx(Ctx),
    % --------------------------------------------------
    % Act
    % --------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(wait_key_down(39)),
        obj_old(ObjIn),
        result(Status, ObjOut)
    ),
    % --------------------------------------------------
    % Assert
    % --------------------------------------------------
    expect(Status = yielded, 'Status != yielded'),
    expect(obj_acns(ObjOut, [wait_key_down(39), noop]),
        'Action not preserved'),
    expect(ctx_cmds([], CtxNew), 'Commands != []')
)).

test("wait_key_down: waits for different key", (
    % --------------------------------------------------
    % Arrange
    % --------------------------------------------------
    ObjIn = object(
        id(1),
        type(static),
        actions([wait_key_down(37)]),
        collisions([])
    ),
    % Create context with key 39 down event (not 37)
    Ctx = ctx(
        state(
            frame(0),
            objects([]),
            attrs(EmptyAttrs),
            status(playing),
            next_id(1),
            commands([])
        ),
        input(events([event(key(39), down)]), held([]))
    ),
    empty_attr_store(EmptyAttrs),
    % --------------------------------------------------
    % Act
    % --------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(_),
        action(wait_key_down(37)),
        obj_old(ObjIn),
        result(Status, ObjOut)
    ),
    % --------------------------------------------------
    % Assert
    % --------------------------------------------------
    expect(Status = yielded,
        'Should yield when different key pressed'),
    expect(obj_acns(ObjOut, [wait_key_down(37)]),
        'Action should remain in queue')
)).

test("wait_key_down: multiple key events", (
    % --------------------------------------------------
    % Arrange
    % --------------------------------------------------
    ObjIn = object(
        id(1),
        type(static),
        actions([wait_key_down(37)]),
        collisions([])
    ),
    % Create context with multiple key events
    Ctx = ctx(
        state(
            frame(0),
            objects([]),
            attrs(EmptyAttrs),
            status(playing),
            next_id(1),
            commands([])
        ),
        input(events([
            event(key(65), down),
            event(key(37), down),
            event(key(39), down)
        ]), held([]))
    ),
    empty_attr_store(EmptyAttrs),
    % --------------------------------------------------
    % Act
    % --------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(_),
        action(wait_key_down(37)),
        obj_old(ObjIn),
        result(Status, ObjOut)
    ),
    % --------------------------------------------------
    % Assert
    % --------------------------------------------------
    expect(Status = completed,
        'Should complete when target key pressed'),
    expect(obj_acns(ObjOut, []),
        'Action should be removed')
)).

test("wait_key_down: ignores key up events", (
    % --------------------------------------------------
    % Arrange
    % --------------------------------------------------
    ObjIn = object(
        id(1),
        type(static),
        actions([wait_key_down(39)]),
        collisions([])
    ),
    % Create context with key 39 up event (not down)
    Ctx = ctx(
        state(
            frame(0),
            objects([]),
            attrs(EmptyAttrs),
            status(playing),
            next_id(1),
            commands([])
        ),
        input(events([event(key(39), up)]), held([]))
    ),
    empty_attr_store(EmptyAttrs),
    % --------------------------------------------------
    % Act
    % --------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(_),
        action(wait_key_down(39)),
        obj_old(ObjIn),
        result(Status, ObjOut)
    ),
    % --------------------------------------------------
    % Assert
    % --------------------------------------------------
    expect(Status = yielded,
        'Should yield when only up event present'),
    expect(obj_acns(ObjOut, [wait_key_down(39)]),
        'Action should remain in queue')
)).

test("wait_key_down: in loop pattern", (
    % --------------------------------------------------
    % Arrange
    % --------------------------------------------------
    ObjIn = object(
        id(1),
        type(static),
        actions([
            loop([wait_key_down(39), set_attr(x, 10)])
        ]),
        collisions([])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(
        1, EmptyAttrs0, [attr(x, 0)], EmptyAttrs
    ),
    % Context without key event
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
        wait_key_down(39),
        set_attr(x, 10),
        loop([wait_key_down(39), set_attr(x, 10)])
    ]), 'Loop should expand and remain')
)).


