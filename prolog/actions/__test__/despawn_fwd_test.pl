:- module(despawn_fwd_test, []).

:- use_module('../../../build/prologinator').
:- use_module('../../../prolog/util/test_util').

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
% Tests: despawn
% ==========================================================

test("despawn: despawns object and prevents remaining \
actions from executing", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    Actions = [
        despawn,
        wait(5),
        move_to(10, 10, 3),
        set_attr(hp, 100)
    ],
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(type, static), attr(x, 0), attr(y, 0)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx0),
    % Add actions to actionstore
    ctx_actionstore(ActionStore0, Ctx0, Ctx0),
    put_assoc(1, ActionStore0, [Actions], ActionStore),
    ctx_set_actionstore(ActionStore, Ctx0, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(despawn),
        actions_old(Actions),
        obj_id(1),
        result(Status, actions_new(_ActionsOut)),
        Ctx,
        CtxNew
    ),
    ctx_spawnCmds(SpawnCmds, CtxNew, CtxNew),
    ctx_frame(Frame, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    expect(Status = despawned, "Status != despawned"),
    expect(SpawnCmds = [], "SpawnCmds != []"),
    expect(Frame = 0, "Frame != 0")
)).

test("despawn: prevents game_over state change from \
executing after despawn", (
    Actions = [
        despawn,
        trigger_state_change(game_over(won)),
        wait(5)
    ],
    empty_ctx(Ctx0),
    ctx_set_attr_val(1/type, static, Ctx0, Ctx1),
    % Add actions to actionstore
    ctx_actionstore(ActionStore0, Ctx1, Ctx1),
    put_assoc(1, ActionStore0, [Actions], ActionStore),
    ctx_set_actionstore(ActionStore, Ctx1, Ctx),
    execute_action(
        action(despawn),
        actions_old(Actions),
        obj_id(1),
        result(Status, actions_new(_ActionsOut)),
        Ctx,
        CtxNew
    ),
    % Object must be despawned
    Status = despawned,
    % Despawn hint must be recorded
    % Status must remain playing (game_over did NOT execute)
    ctx_status(playing, CtxNew, CtxNew),
    % No commands
    ctx_spawnCmds([], CtxNew, CtxNew)
)).

