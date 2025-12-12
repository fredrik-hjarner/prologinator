:- module(set_attr_fwd_test, []).
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

% ==========================================================
% Tests: set_attr
% ==========================================================

test("set_attr: set new attribute", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ObjIn = object(
        id(1), type(enemy),
        actions([set_attr(hp, 100)]), collisions([])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 5), attr(y, 10)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(set_attr(hp, 100)),
        obj_old(ObjIn),
        obj_new([ObjOut])
    ),
    ctx_attr_val(CtxNew, 1/hp, HP),
    ctx_attr_val(CtxNew, 1/x, X),
    ctx_attr_val(CtxNew, 1/y, Y),
    ctx_cmds(CtxNew, Commands),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ObjOut = object(
        id(1), type(enemy),
        actions([]), collisions([])
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
        actions([set_attr(hp, 50)]), collisions([])
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
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(set_attr(hp, 50)),
        obj_old(ObjIn),
        obj_new([ObjOut])
    ),
    ctx_attr_val(CtxNew, 1/hp, HP),
    ctx_attr_val(CtxNew, 1/x, X),
    ctx_attr_val(CtxNew, 1/y, Y),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ObjOut = object(
        id(1), type(enemy),
        actions([]), collisions([])
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
        ]), collisions([])
    ),
    empty_ctx(Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(Ctx1),
        action(set_attr(hp, 100)),
        obj_old(ObjIn),
        obj_new([Obj1])
    ),
    execute_action(
        ctx_old(Ctx1),
        ctx_new(Ctx2),
        action(set_attr(hp, 75)),
        obj_old(Obj1),
        obj_new([Obj2])
    ),
    execute_action(
        ctx_old(Ctx2),
        ctx_new(CtxNew),
        action(set_attr(hp, 50)),
        obj_old(Obj2),
        obj_new([_ObjOut])
    ),
    ctx_attr_val(CtxNew, 1/hp, HP),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (HP = 50 ; err_write("HP != 50"))
)).

