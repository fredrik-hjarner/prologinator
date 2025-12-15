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
    ObjIn = object(
        id(0), type(static),
        actions([noop, wait(1)]), collisions([])
    ),
    empty_ctx(Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(noop),
        obj_old(ObjIn),
        result(ActionStatus, ObjOut)
    ),
    ctx_cmds(Commands, CtxNew),
    ctx_frame(Frame, CtxNew),
    ctx_status(Status, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (ActionStatus = completed
    ; err_write("ActionStatus != completed")),
    ObjOut = object(
        id(0), type(static),
        actions([wait(1)]), collisions([])
    ),
    (Commands = [] ; err_write("Commands != []")),
    (Frame = 0 ; err_write("Frame != 0")),
    (Status = playing ; err_write("Status != playing"))
)).

