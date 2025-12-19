:- module(move_to_fwd_test, []).

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
% Tests: move_to
% ----------------------------------------------------------

test("move_to: positive direction, multiple frames \
remaining", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    Action = move_to(10, 20, 3),
    ActionsIn = [move_to(10, 20, 3)],
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(type, static), attr(x, 0), attr(y, 0)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(Action),
        actions_old(ActionsIn),
        obj_id(1),
        result(Status, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    ctx_spawnCmds(SpawnCmds, CtxNew, CtxNew),
    ctx_attr_val(1/x, NewX, CtxNew, CtxNew),
    ctx_attr_val(1/y, NewY, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    expect(Status = yielded, "Status != yielded"),
    ( ActionsOut = [move_to(10, 20, 2)|_]
     ; expect(false, "ActionsOut mismatch") ),
    expect(NewX = 3, "NewX != 3"),
    expect(NewY = 6, "NewY != 6"),
    expect(SpawnCmds = [], "SpawnCmds != []")
)).

test("move_to: negative direction, multiple frames \
remaining", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    Action = move_to(0, 0, 3),
    ActionsIn = [move_to(0, 0, 3)],
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(type, static),
               attr(x, 10), attr(y, 20)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(Action),
        actions_old(ActionsIn),
        obj_id(1),
        result(Status, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    ctx_spawnCmds(SpawnCmds, CtxNew, CtxNew),
    ctx_attr_val(1/x, NewX, CtxNew, CtxNew),
    ctx_attr_val(1/y, NewY, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    expect(Status = yielded, "Status != yielded"),
    ActionsOut = [move_to(0, 0, 2)|_],
    expect(NewX = 7, "NewX != 7"),
    expect(NewY = 14, "NewY != 14"),
    expect(SpawnCmds = [], "SpawnCmds != []")
)).

test("move_to: single frame, arrives at target", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    Action = move_to(5, 5, 1),
    ActionsIn = [move_to(5, 5, 1)],
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(type, static), attr(x, 0), attr(y, 0)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(Action),
        actions_old(ActionsIn),
        obj_id(1),
        result(Status, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    ctx_spawnCmds(SpawnCmds, CtxNew, CtxNew),
    ctx_attr_val(1/x, X, CtxNew, CtxNew),
    ctx_attr_val(1/y, Y, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    expect(Status = yielded, "Status != yielded"),
    expect(X = 5, "X != 5"),
    expect(Y = 5, "Y != 5"),
    expect(SpawnCmds = [], "SpawnCmds != []")
)).

test("move_to: already at target, stays at position and \
continues with remaining frames", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    Action = move_to(10, 20, 3),
    ActionsIn = [move_to(10, 20, 3)],
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(type, static),
               attr(x, 10), attr(y, 20)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(Action),
        actions_old(ActionsIn),
        obj_id(1),
        result(Status, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    ctx_spawnCmds(SpawnCmds, CtxNew, CtxNew),
    ctx_attr_val(1/x, X, CtxNew, CtxNew),
    ctx_attr_val(1/y, Y, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    expect(Status = yielded, "Status != yielded"),
    ActionsOut = [move_to(10, 20, 2)|_],
    expect(X = 10, "X != 10"),
    expect(Y = 20, "Y != 20"),
    expect(SpawnCmds = [], "SpawnCmds != []")
)).

test("move_to: negative target coordinates", (
    Action = move_to(-5, -10, 2),
    ActionsIn = [move_to(-5, -10, 2)],
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(type, static), attr(x, 0), attr(y, 0)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    execute_action(
        action(Action),
        actions_old(ActionsIn),
        obj_id(1),
        result(Status, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    ctx_spawnCmds(SpawnCmds, CtxNew, CtxNew),
    ctx_attr_val(1/x, NewX, CtxNew, CtxNew),
    ctx_attr_val(1/y, NewY, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    expect(Status = yielded, "Status != yielded"),
    ActionsOut = [move_to(-5, -10, 1)|_],
    expect(NewX = -2, "NewX != -2"),
    expect(NewY = -5, "NewY != -5"),
    expect(SpawnCmds = [], "SpawnCmds != []")
)).

