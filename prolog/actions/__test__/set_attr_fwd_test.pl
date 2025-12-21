:- module(set_attr_fwd_test, []).

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
% Tests: set_attr
% ==========================================================

test("set_attr: set new attribute", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ActionsIn = [set_attr(hp, 100)],
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(type, enemy), attr(x, 5), attr(y, 10)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        actions_old(ActionsIn),
        obj_id(1),
        result(completed, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    ctx_attr_val(1/hp, HP, CtxNew, CtxNew),
    ctx_attr_val(1/x, X, CtxNew, CtxNew),
    ctx_attr_val(1/y, Y, CtxNew, CtxNew),
    ctx_spawnCmds(SpawnCmds, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    expect(HP = 100, "HP != 100"),
    expect(X = 5, "X != 5"),
    expect(Y = 10, "Y != 10"),
    expect(SpawnCmds = [], "SpawnCmds != []")
)).

test("set_attr: replace existing attribute", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ActionsIn = [set_attr(hp, 50)],
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(type, enemy), attr(hp, 100), attr(x, 5),
               attr(y, 10)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        actions_old(ActionsIn),
        obj_id(1),
        result(completed, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    ctx_attr_val(1/hp, HP, CtxNew, CtxNew),
    ctx_attr_val(1/x, X, CtxNew, CtxNew),
    ctx_attr_val(1/y, Y, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    expect(HP = 50, "HP != 50"),
    expect(X = 5, "X != 5"),
    expect(Y = 10, "Y != 10")
)).

test("set_attr: multiple sets overwrite, no \
duplicates", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ActionsIn = [
        set_attr(hp, 100),
        set_attr(hp, 75),
        set_attr(hp, 50)
    ],
    empty_ctx(Ctx0),
    ctx_set_attr_val(1/type, enemy, Ctx0, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        actions_old(ActionsIn),
        obj_id(1),
        result(completed, actions_new(ActionsOut)),
        Ctx,
        Ctx1
    ),
    execute_action(
        actions_old(ActionsOut),
        obj_id(1),
        result(completed, actions_new(ActionsOut2)),
        Ctx1,
        Ctx2
    ),
    execute_action(
        actions_old(ActionsOut2),
        obj_id(1),
        result(completed, actions_new(_ActionsOut3)),
        Ctx2,
        CtxNew
    ),
    ctx_attr_val(1/hp, HP, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    expect(HP = 50, "HP != 50")
)).

