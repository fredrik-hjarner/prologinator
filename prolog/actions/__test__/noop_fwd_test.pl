:- module(noop_fwd_test, []).

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
% Tests: noop
% ==========================================================

test("noop: removes self from action queue", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ActionsIn = [noop, wait(1)],
    empty_ctx(Ctx0),
    ctx_set_attr_val(0/type, static, Ctx0, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(noop),
        actions_old(ActionsIn),
        obj_id(0),
        result(ActionStatus, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    ctx_spawnCmds(SpawnCmds, CtxNew, CtxNew),
    ctx_frame(Frame, CtxNew, CtxNew),
    ctx_status(Status, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (ActionStatus = completed
    ; expect(false, "ActionStatus != completed")),
    ActionsOut = [wait(1)],
    expect(SpawnCmds = [], "SpawnCmds != []"),
    expect(Frame = 0, "Frame != 0"),
    expect(Status = playing, "Status != playing")
)).

