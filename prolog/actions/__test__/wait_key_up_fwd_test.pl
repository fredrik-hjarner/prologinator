:- module(wait_key_up_fwd_test, []).

:- use_module('../../..'/build/prologinator).
:- use_module('../../..'/prolog/util/test_util).

:- use_module(library(assoc), [
    empty_assoc/1,
    put_assoc/4
]).

% ==========================================================
% Tests: wait_key_up Action (Forward)
% ==========================================================

test("wait_key_up: completes when key released", (
    % --------------------------------------------------
    % Arrange
    % --------------------------------------------------
    ObjIn = object(
        id(1),
        actions([wait_key_up(39), noop])
    ),
    % Create context with key 39 up event
    ctx_with_inputevents_inputheld(
        [event(key(39), up)], [], Ctx0),
    ctx_set_attr_val(1/type, static, Ctx0, Ctx),
    % --------------------------------------------------
    % Act
    % --------------------------------------------------
    execute_action(
        action(wait_key_up(39)),
        obj_old(ObjIn),
        result(Status, ObjOut),
        Ctx,
        CtxNew
    ),
    % --------------------------------------------------
    % Assert
    % --------------------------------------------------
    expect(Status = completed, 'Status != completed'),
    expect(obj_acns(ObjOut, [noop]),
        'Actions != [noop]'),
    expect(ctx_cmds([], CtxNew, CtxNew), 'Commands != []')
)).

test("wait_key_up: yields when key not released", (
    % --------------------------------------------------
    % Arrange
    % --------------------------------------------------
    ObjIn = object(
        id(1),
        actions([wait_key_up(39), noop])
    ),
    % Create context with no key events
    empty_ctx(Ctx0),
    ctx_set_attr_val(1/type, static, Ctx0, Ctx),
    % --------------------------------------------------
    % Act
    % --------------------------------------------------
    execute_action(
        action(wait_key_up(39)),
        obj_old(ObjIn),
        result(Status, ObjOut),
        Ctx,
        CtxNew
    ),
    % --------------------------------------------------
    % Assert
    % --------------------------------------------------
    expect(Status = yielded, 'Status != yielded'),
    expect(obj_acns(ObjOut, [wait_key_up(39), noop]),
        'Action not preserved'),
    expect(ctx_cmds([], CtxNew, CtxNew), 'Commands != []')
)).

test("wait_key_up: waits for different key", (
    % --------------------------------------------------
    % Arrange
    % --------------------------------------------------
    ObjIn = object(
        id(1),
        actions([wait_key_up(37)])
    ),
    % Create context with key 39 up event (not 37)
    ctx_with_inputevents_inputheld(
        [event(key(39), up)], [], Ctx0),
    ctx_set_attr_val(1/type, static, Ctx0, Ctx),
    % --------------------------------------------------
    % Act
    % --------------------------------------------------
    execute_action(
        action(wait_key_up(37)),
        obj_old(ObjIn),
        result(Status, ObjOut),
        Ctx,
        _
    ),
    % --------------------------------------------------
    % Assert
    % --------------------------------------------------
    expect(Status = yielded,
        'Should yield when different key released'),
    expect(obj_acns(ObjOut, [wait_key_up(37)]),
        'Action should remain in queue')
)).

test("wait_key_up: multiple key events", (
    % --------------------------------------------------
    % Arrange
    % --------------------------------------------------
    ObjIn = object(
        id(1),
        actions([wait_key_up(37)])
    ),
    % Create context with multiple key events
    ctx_with_inputevents_inputheld([
        event(key(65), up),
        event(key(37), up),
        event(key(39), up)
    ], [], Ctx0),
    ctx_set_attr_val(1/type, static, Ctx0, Ctx),
    % --------------------------------------------------
    % Act
    % --------------------------------------------------
    execute_action(
        action(wait_key_up(37)),
        obj_old(ObjIn),
        result(Status, ObjOut),
        Ctx,
        _
    ),
    % --------------------------------------------------
    % Assert
    % --------------------------------------------------
    expect(Status = completed,
        'Should complete when target key released'),
    expect(obj_acns(ObjOut, []),
        'Action should be removed')
)).

test("wait_key_up: ignores key down events", (
    % --------------------------------------------------
    % Arrange
    % --------------------------------------------------
    ObjIn = object(
        id(1),
        actions([wait_key_up(39)])
    ),
    % Create context with key 39 down event (not up)
    ctx_with_inputevents_inputheld(
        [event(key(39), down)], [], Ctx0),
    ctx_set_attr_val(1/type, static, Ctx0, Ctx),
    % --------------------------------------------------
    % Act
    % --------------------------------------------------
    execute_action(
        action(wait_key_up(39)),
        obj_old(ObjIn),
        result(Status, ObjOut),
        Ctx,
        _
    ),
    % --------------------------------------------------
    % Assert
    % --------------------------------------------------
    expect(Status = yielded,
        'Should yield when only down event present'),
    expect(obj_acns(ObjOut, [wait_key_up(39)]),
        'Action should remain in queue')
)).

test("wait_key_up: ignores held keys", (
    % --------------------------------------------------
    % Arrange
    % --------------------------------------------------
    ObjIn = object(
        id(1),
        actions([wait_key_up(39)])
    ),
    % Create context with key 39 held (but no up event)
    ctx_with_inputevents_inputheld([], [39], Ctx0),
    ctx_set_attr_val(1/type, static, Ctx0, Ctx),
    % --------------------------------------------------
    % Act
    % --------------------------------------------------
    execute_action(
        action(wait_key_up(39)),
        obj_old(ObjIn),
        result(Status, ObjOut),
        Ctx,
        _
    ),
    % --------------------------------------------------
    % Assert
    % --------------------------------------------------
    expect(Status = yielded,
        'Should yield when key held without up event'),
    expect(obj_acns(ObjOut, [wait_key_up(39)]),
        'Action should remain in queue')
)).

test("wait_key_up: in loop pattern", (
    % --------------------------------------------------
    % Arrange
    % --------------------------------------------------
    ObjIn = object(
        id(1),
        actions([
            loop([wait_key_up(39), set_attr(x, 10)])
        ])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(
        1, EmptyAttrs0,
        [attr(type, static), attr(x, 0)], EmptyAttrs
    ),
    % Context without key event
    ctx_with_attrs(EmptyAttrs, Ctx),
    % --------------------------------------------------
    % Act
    % --------------------------------------------------
    tick_object(
        obj_old(ObjIn),
        result(Status1, Obj1),
        Ctx,
        Ctx1
    ),
    % --------------------------------------------------
    % Assert
    % --------------------------------------------------
    % Should yield waiting for key release
    expect(Status1 = yielded, 'Should yield'),
    expect(ctx_attr_val(1/x, 0, Ctx1, Ctx1),
        'x should still be 0'),
    % Loop expands body and adds itself back
    expect(obj_acns(Obj1, [
        wait_key_up(39),
        set_attr(x, 10),
        loop([wait_key_up(39), set_attr(x, 10)])
    ]), 'Loop should expand and remain')
)).


