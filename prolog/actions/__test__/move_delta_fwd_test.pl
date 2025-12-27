:- module(move_delta_fwd_test, []).

#include "./build/prologinator.pl"
#include "./prolog/util/test_util.pl"

% ==========================================================
% Forward Tests (all inputs ground, normal use case)
% ==========================================================

% ==========================================================
% Tests: move_delta
% ==========================================================

test("move_delta: single frame moves and completes", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ActionsIn = [move_delta(1, 5, -3)],
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
        actions_old(ActionsIn),
        obj_id(1),
        result(yielded, actions_new(_ActionsOut)),
        Ctx,
        CtxNew
    ),
    ctx_attr_val(1/x, X, CtxNew, CtxNew),
    ctx_attr_val(1/y, Y, CtxNew, CtxNew),
    ctx_spawnCmds(SpawnCmds, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    expect(X = 15, 'X != 15'),
    expect(Y = 17, 'Y != 17'),
    expect(SpawnCmds = [], 'SpawnCmds != []')
)).

test("move_delta: multiple frames continues", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ActionsIn = [move_delta(3, 10, 5)],
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(type, static), attr(x, 0), attr(y, 0)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        actions_old(ActionsIn),
        obj_id(1),
        result(yielded, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    ctx_attr_val(1/x, X, CtxNew, CtxNew),
    ctx_attr_val(1/y, Y, CtxNew, CtxNew),
    ctx_spawnCmds(SpawnCmds, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    expect(X = 10, "X != 10"),
    expect(Y = 5, "Y != 5"),
    (ActionsOut = [move_delta(2, 10, 5)]
     ;
     expect(false, "Actions wrong")),
    expect(SpawnCmds = [], "SpawnCmds != []")
)).

test("move_delta: negative deltas work", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ActionsIn = [move_delta(2, -10, -5)],
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 50), attr(y, 50)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        actions_old(ActionsIn),
        obj_id(1),
        result(yielded, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    ctx_attr_val(1/x, X, CtxNew, CtxNew),
    ctx_attr_val(1/y, Y, CtxNew, CtxNew),
    ctx_spawnCmds(SpawnCmds, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    expect(X = 40, "X != 40"),
    expect(Y = 45, "Y != 45"),
    (ActionsOut = [move_delta(1, -10, -5)]
     ;
     expect(false, "Actions wrong")),
    expect(SpawnCmds = [], "SpawnCmds != []")
)).

test("move_delta: preserves other attributes", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ActionsIn = [move_delta(1, 5, -3)],
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(x, 10), attr(y, 20),
               attr(hp, 100), attr(speed, 5)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        actions_old(ActionsIn),
        obj_id(1),
        result(yielded, actions_new(_ActionsOut)),
        Ctx,
        CtxNew
    ),
    ctx_attr_val(1/x, X, CtxNew, CtxNew),
    ctx_attr_val(1/y, Y, CtxNew, CtxNew),
    ctx_attr_val(1/hp, HP, CtxNew, CtxNew),
    ctx_attr_val(1/speed, Speed, CtxNew, CtxNew),
    ctx_spawnCmds(SpawnCmds, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    expect(X = 15, "X != 15"),
    expect(Y = 17, "Y != 17"),
    expect(HP = 100, "HP != 100"),
    expect(Speed = 5, "Speed != 5"),
    expect(SpawnCmds = [], "SpawnCmds != []")
)).

