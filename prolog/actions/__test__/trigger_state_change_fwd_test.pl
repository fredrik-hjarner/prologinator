:- module(trigger_state_change_fwd_test, []).

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

% --------------------------------------------------------
% Tests: trigger_state_change
% --------------------------------------------------------

test("trigger_state_change: forward test - updates status \
to won", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    Action = trigger_state_change(game_over(won)),
    ObjIn = object(
        id(1),
        type(static),
        actions([trigger_state_change(game_over(won))])
    ),
    empty_ctx(CtxIn),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(Action),
        obj_old(ObjIn),
        result(completed, ObjOut),
        CtxIn,
        CtxOut
    ),
    ctx_cmds(Commands, CtxOut, CtxOut),
    ctx_status(Status, CtxOut, CtxOut),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ObjOut = object(
        id(1),
        type(static),
        actions([])
    ),
    (Status = won ; err_write("Status != won")),
    (Commands = [] ; err_write("Commands != []"))
)).

test("trigger_state_change: forward test - updates status \
to lost", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    Action = trigger_state_change(game_over(lost)),
    ObjIn = object(
        id(1),
        type(static),
        actions([trigger_state_change(game_over(lost))])
    ),
    empty_ctx(CtxIn),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(Action),
        obj_old(ObjIn),
        result(completed, ObjOut),
        CtxIn,
        CtxOut
    ),
    ctx_cmds(Commands, CtxOut, CtxOut),
    ctx_status(Status, CtxOut, CtxOut),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ObjOut = object(
        id(1),
        type(static),
        actions([])
    ),
    (Status = lost ; err_write("Status != lost")),
    (Commands = [] ; err_write("Commands != []"))
)).

test("trigger_state_change: forward test - won cannot \
override lost", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    Action = trigger_state_change(game_over(won)),
    ObjIn = object(
        id(1),
        type(static),
        actions([trigger_state_change(game_over(won))])
    ),
    empty_ctx(CtxTemp),
    ctx_set_status(lost, CtxTemp, CtxIn),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(Action),
        obj_old(ObjIn),
        result(completed, ObjOut),
        CtxIn,
        CtxOut
    ),
    ctx_cmds(Commands, CtxOut, CtxOut),
    ctx_status(Status, CtxOut, CtxOut),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ObjOut = object(
        id(1),
        type(static),
        actions([])
    ),
    (Status = lost ; err_write("Status != lost")),
    (Commands = [] ; err_write("Commands != []"))
)).

