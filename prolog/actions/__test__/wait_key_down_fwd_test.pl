:- module(wait_key_down_fwd_test, []).

#include "./build/prologinator.pl"
#include "./prolog/util/test_util.pl"

% ==========================================================
% Tests: wait_key_down Action (Forward)
% ==========================================================

test("wait_key_down: completes when key pressed", (
    % --------------------------------------------------
    % Arrange
    % --------------------------------------------------
    ActionsIn = [wait_key_down(39), wait(0)],
    % Create context with key 39 down event
    ctx_with_inputevents_inputheld(
        [event(key(39), down)], [], Ctx0),
    ctx_set_attr_val(1/type, static, Ctx0, Ctx),
    % --------------------------------------------------
    % Act
    % --------------------------------------------------
    execute_action(
        actions_old(ActionsIn),
        obj_id(1),
        result(Status, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    % --------------------------------------------------
    % Assert
    % --------------------------------------------------
    expect(Status = completed, 'Status != completed'),
    expect(ActionsOut = [wait(0)],
        'Actions != [wait(0)]'),
    expect(ctx_spawnCmds([], CtxNew, CtxNew),
           'SpawnCmds != []')
)).

test("wait_key_down: yields when key not pressed", (
    % --------------------------------------------------
    % Arrange
    % --------------------------------------------------
    ActionsIn = [wait_key_down(39), wait(0)],
    % Create context with no key events
    empty_ctx(Ctx0),
    ctx_set_attr_val(1/type, static, Ctx0, Ctx),
    % --------------------------------------------------
    % Act
    % --------------------------------------------------
    execute_action(
        actions_old(ActionsIn),
        obj_id(1),
        result(Status, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    % --------------------------------------------------
    % Assert
    % --------------------------------------------------
    expect(Status = yielded, 'Status != yielded'),
    expect(ActionsOut = [wait_key_down(39), wait(0)],
        'Action not preserved'),
    expect(ctx_spawnCmds([], CtxNew, CtxNew),
           'SpawnCmds != []')
)).

test("wait_key_down: waits for different key", (
    % --------------------------------------------------
    % Arrange
    % --------------------------------------------------
    ActionsIn = [wait_key_down(37)],
    % Create context with key 39 down event (not 37)
    ctx_with_inputevents_inputheld(
        [event(key(39), down)], [], Ctx0),
    ctx_set_attr_val(1/type, static, Ctx0, Ctx),
    % --------------------------------------------------
    % Act
    % --------------------------------------------------
    execute_action(
        actions_old(ActionsIn),
        obj_id(1),
        result(Status, actions_new(ActionsOut)),
        Ctx,
        _
    ),
    % --------------------------------------------------
    % Assert
    % --------------------------------------------------
    expect(Status = yielded,
        'Should yield when different key pressed'),
    expect(ActionsOut = [wait_key_down(37)],
        'Action should remain in queue')
)).

test("wait_key_down: multiple key events", (
    % --------------------------------------------------
    % Arrange
    % --------------------------------------------------
    ActionsIn = [wait_key_down(37)],
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
    execute_action(
        actions_old(ActionsIn),
        obj_id(1),
        result(Status, actions_new(ActionsOut)),
        Ctx,
        _
    ),
    % --------------------------------------------------
    % Assert
    % --------------------------------------------------
    expect(Status = completed,
        'Should complete when target key pressed'),
    expect(ActionsOut = [],
        'Action should be removed')
)).

test("wait_key_down: ignores key up events", (
    % --------------------------------------------------
    % Arrange
    % --------------------------------------------------
    ActionsIn = [wait_key_down(39)],
    % Create context with key 39 up event (not down)
    ctx_with_inputevents_inputheld(
        [event(key(39), up)], [], Ctx0),
    ctx_set_attr_val(1/type, static, Ctx0, Ctx),
    % --------------------------------------------------
    % Act
    % --------------------------------------------------
    execute_action(
        actions_old(ActionsIn),
        obj_id(1),
        result(Status, actions_new(ActionsOut)),
        Ctx,
        _
    ),
    % --------------------------------------------------
    % Assert
    % --------------------------------------------------
    expect(Status = yielded,
        'Should yield when only up event present'),
    expect(ActionsOut = [wait_key_down(39)],
        'Action should remain in queue')
)).

test("wait_key_down: in loop pattern", (
    % --------------------------------------------------
    % Arrange
    % --------------------------------------------------
    ActionsIn = [
        loop([wait_key_down(39), set_attr(x, 10)])
    ],
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
        actions_old(ActionsIn),
        obj_id(1),
        result(Status1, actions_new(ActionsOut)),
        Ctx,
        Ctx1
    ),
    % --------------------------------------------------
    % Assert
    % --------------------------------------------------
    % Should yield waiting for key
    expect(Status1 = yielded, 'Should yield'),
    expect(ctx_attr_val(1/x, 0, Ctx1, Ctx1),
        'x should still be 0'),
    % Loop expands body and adds itself back
    expect(
      ActionsOut = [
        loop(
          [wait_key_down(39), set_attr(x, 10)],
          [wait_key_down(39), set_attr(x, 10)]
        )
      ],
      'Loop should preserve internal state'
    )
)).


