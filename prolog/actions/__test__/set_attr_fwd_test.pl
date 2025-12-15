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
    ObjIn = object(
        id(1), type(enemy),
        actions([set_attr(hp, 100)])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 5), attr(y, 10)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(set_attr(hp, 100)),
        obj_old(ObjIn),
        result(completed, ObjOut),
        Ctx,
        CtxNew
    ),
    ctx_attr_val(1/hp, HP, CtxNew, CtxNew),
    ctx_attr_val(1/x, X, CtxNew, CtxNew),
    ctx_attr_val(1/y, Y, CtxNew, CtxNew),
    ctx_cmds(Commands, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ObjOut = object(
        id(1), type(enemy),
        actions([])
    ),
    (HP = 100 ; err_write("HP != 100")),
    (X = 5 ; err_write("X != 5")),
    (Y = 10 ; err_write("Y != 10")),
    (Commands = [] ; err_write("Commands != []"))
)).

test("set_attr: replace existing attribute", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ObjIn = object(
        id(1), type(enemy),
        actions([set_attr(hp, 50)])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(hp, 100), attr(x, 5), attr(y, 10)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(set_attr(hp, 50)),
        obj_old(ObjIn),
        result(completed, ObjOut),
        Ctx,
        CtxNew
    ),
    ctx_attr_val(1/hp, HP, CtxNew, CtxNew),
    ctx_attr_val(1/x, X, CtxNew, CtxNew),
    ctx_attr_val(1/y, Y, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ObjOut = object(
        id(1), type(enemy),
        actions([])
    ),
    (HP = 50 ; err_write("HP != 50")),
    (X = 5 ; err_write("X != 5")),
    (Y = 10 ; err_write("Y != 10"))
)).

test("set_attr: multiple sets overwrite, no \
duplicates", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ObjIn = object(
        id(1), type(enemy),
        actions([
            set_attr(hp, 100),
            set_attr(hp, 75),
            set_attr(hp, 50)
        ])
    ),
    empty_ctx(Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(set_attr(hp, 100)),
        obj_old(ObjIn),
        result(completed, Obj1),
        Ctx,
        Ctx1
    ),
    execute_action(
        action(set_attr(hp, 75)),
        obj_old(Obj1),
        result(completed, Obj2),
        Ctx1,
        Ctx2
    ),
    execute_action(
        action(set_attr(hp, 50)),
        obj_old(Obj2),
        result(completed, _ObjOut),
        Ctx2,
        CtxNew
    ),
    ctx_attr_val(1/hp, HP, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (HP = 50 ; err_write("HP != 50"))
)).

