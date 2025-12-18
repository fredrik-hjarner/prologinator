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
        actions([wait_key_down(39), noop])
    ),
    % Create context with key 39 down event
    ctx_with_inputevents_inputheld(
        [event(key(39), down)], [], Ctx0),
    ctx_set_attr_val(1/type, static, Ctx0, Ctx),
    % --------------------------------------------------
    % Act
    % --------------------------------------------------
    obj_acns(ObjIn, ActionsIn),
    obj_id(ObjIn, ID),
    execute_action(
        action(wait_key_down(39)),
        actions_old(ActionsIn),
        obj_id(ID),
        result(Status, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    obj_acns_obj(ObjIn, ActionsOut, ObjOut),
    % --------------------------------------------------
    % Assert
    % --------------------------------------------------
    expect(Status = completed, 'Status != completed'),
    expect(obj_acns(ObjOut, [noop]),
        'Actions != [noop]'),
    expect(ctx_cmds([], CtxNew, CtxNew), 'Commands != []')
)).

test("wait_key_down: yields when key not pressed", (
    % --------------------------------------------------
    % Arrange
    % --------------------------------------------------
    ObjIn = object(
        id(1),
        actions([wait_key_down(39), noop])
    ),
    % Create context with no key events
    empty_ctx(Ctx0),
    ctx_set_attr_val(1/type, static, Ctx0, Ctx),
    % --------------------------------------------------
    % Act
    % --------------------------------------------------
    obj_acns(ObjIn, ActionsIn),
    obj_id(ObjIn, ID),
    execute_action(
        action(wait_key_down(39)),
        actions_old(ActionsIn),
        obj_id(ID),
        result(Status, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    obj_acns_obj(ObjIn, ActionsOut, ObjOut),
    % --------------------------------------------------
    % Assert
    % --------------------------------------------------
    expect(Status = yielded, 'Status != yielded'),
    expect(obj_acns(ObjOut, [wait_key_down(39), noop]),
        'Action not preserved'),
    expect(ctx_cmds([], CtxNew, CtxNew), 'Commands != []')
)).

test("wait_key_down: waits for different key", (
    % --------------------------------------------------
    % Arrange
    % --------------------------------------------------
    ObjIn = object(
        id(1),
        actions([wait_key_down(37)])
    ),
    % Create context with key 39 down event (not 37)
    ctx_with_inputevents_inputheld(
        [event(key(39), down)], [], Ctx0),
    ctx_set_attr_val(1/type, static, Ctx0, Ctx),
    % --------------------------------------------------
    % Act
    % --------------------------------------------------
    obj_acns(ObjIn, ActionsIn),
    obj_id(ObjIn, ID),
    execute_action(
        action(wait_key_down(37)),
        actions_old(ActionsIn),
        obj_id(ID),
        result(Status, actions_new(ActionsOut)),
        Ctx,
        _
    ),
    obj_acns_obj(ObjIn, ActionsOut, ObjOut),
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
        actions([wait_key_down(37)])
    ),
    % Create context with multiple key events
    ctx_with_inputevents_inputheld([
        event(key(65), down),
        event(key(37), down),
        event(key(39), down)
    ], [], Ctx0),
    ctx_set_attr_val(1/type, static, Ctx0, Ctx),
    % --------------------------------------------------
    % Act
    % --------------------------------------------------
    obj_acns(ObjIn, ActionsIn),
    obj_id(ObjIn, ID),
    execute_action(
        action(wait_key_down(37)),
        actions_old(ActionsIn),
        obj_id(ID),
        result(Status, actions_new(ActionsOut)),
        Ctx,
        _
    ),
    obj_acns_obj(ObjIn, ActionsOut, ObjOut),
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
        actions([wait_key_down(39)])
    ),
    % Create context with key 39 up event (not down)
    ctx_with_inputevents_inputheld(
        [event(key(39), up)], [], Ctx0),
    ctx_set_attr_val(1/type, static, Ctx0, Ctx),
    % --------------------------------------------------
    % Act
    % --------------------------------------------------
    obj_acns(ObjIn, ActionsIn),
    obj_id(ObjIn, ID),
    execute_action(
        action(wait_key_down(39)),
        actions_old(ActionsIn),
        obj_id(ID),
        result(Status, actions_new(ActionsOut)),
        Ctx,
        _
    ),
    obj_acns_obj(ObjIn, ActionsOut, ObjOut),
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
        actions([
            loop([wait_key_down(39), set_attr(x, 10)])
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
    obj_acns(ObjIn, ActionsIn),
    obj_id(ObjIn, ID),
    tick_object(
        actions_old(ActionsIn),
        obj_id(ID),
        result(Status1, actions_new(ActionsOut)),
        Ctx,
        Ctx1
    ),
    obj_acns_obj(ObjIn, ActionsOut, Obj1),
    % --------------------------------------------------
    % Assert
    % --------------------------------------------------
    % Should yield waiting for key
    expect(Status1 = yielded, 'Should yield'),
    expect(ctx_attr_val(1/x, 0, Ctx1, Ctx1),
        'x should still be 0'),
    % Loop expands body and adds itself back
    expect(obj_acns(Obj1, [
        wait_key_down(39),
        set_attr(x, 10),
        loop([wait_key_down(39), set_attr(x, 10)])
    ]), 'Loop should expand and remain')
)).


