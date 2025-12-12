:- module(trigger_state_change_fwd_test, []).
:- use_module('../../execute_action', [execute_action/5]).
:- use_module('../../types/accessors').
:- use_module('../../types/adv_accessors').
:- use_module('../../types/constructors').
:- use_module('../../util/util', [
    err_write/1, err_format/2
]).
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
        actions([trigger_state_change(game_over(won))]),
        collisions([])
    ),
    empty_ctx(CtxIn),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        ctx_old(CtxIn),
        ctx_new(CtxOut),
        action(Action),
        obj_old(ObjIn),
        obj_new(ObjOut)
    ),
    ctx_cmds(CtxOut, Commands),
    ctx_status(CtxOut, Status),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ObjOut = [object(
        id(1),
        type(static),
        actions([]),
        collisions([])
    )],
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
        actions([trigger_state_change(game_over(lost))]),
        collisions([])
    ),
    empty_ctx(CtxIn),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        ctx_old(CtxIn),
        ctx_new(CtxOut),
        action(Action),
        obj_old(ObjIn),
        obj_new(ObjOut)
    ),
    ctx_cmds(CtxOut, Commands),
    ctx_status(CtxOut, Status),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ObjOut = [object(
        id(1),
        type(static),
        actions([]),
        collisions([])
    )],
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
        actions([trigger_state_change(game_over(won))]),
        collisions([])
    ),
    empty_ctx(CtxTemp),
    ctx_status_ctx(CtxTemp, lost, CtxIn),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        ctx_old(CtxIn),
        ctx_new(CtxOut),
        action(Action),
        obj_old(ObjIn),
        obj_new(ObjOut)
    ),
    ctx_cmds(CtxOut, Commands),
    ctx_status(CtxOut, Status),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ObjOut = [object(
        id(1),
        type(static),
        actions([]),
        collisions([])
    )],
    (Status = lost ; err_write("Status != lost")),
    (Commands = [] ; err_write("Commands != []"))
)).

