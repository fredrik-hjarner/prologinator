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
        id(1),
        actions([set_attr(hp, 100)])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(type, enemy), attr(x, 5), attr(y, 10)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    obj_acns(ObjIn, ActionsIn),
    obj_id(ObjIn, ID),
    execute_action(
        action(set_attr(hp, 100)),
        actions_old(ActionsIn),
        obj_id(ID),
        result(completed, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    obj_acns_obj(ObjIn, ActionsOut, ObjOut),
    ctx_attr_val(1/hp, HP, CtxNew, CtxNew),
    ctx_attr_val(1/x, X, CtxNew, CtxNew),
    ctx_attr_val(1/y, Y, CtxNew, CtxNew),
    ctx_cmds(Commands, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ObjOut = object(
        id(1),
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
        id(1),
        actions([set_attr(hp, 50)])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(type, enemy), attr(hp, 100), attr(x, 5),
               attr(y, 10)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    obj_acns(ObjIn, ActionsIn),
    obj_id(ObjIn, ID),
    execute_action(
        action(set_attr(hp, 50)),
        actions_old(ActionsIn),
        obj_id(ID),
        result(completed, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    obj_acns_obj(ObjIn, ActionsOut, ObjOut),
    ctx_attr_val(1/hp, HP, CtxNew, CtxNew),
    ctx_attr_val(1/x, X, CtxNew, CtxNew),
    ctx_attr_val(1/y, Y, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ObjOut = object(
        id(1),
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
        id(1),
        actions([
            set_attr(hp, 100),
            set_attr(hp, 75),
            set_attr(hp, 50)
        ])
    ),
    empty_ctx(Ctx0),
    ctx_set_attr_val(1/type, enemy, Ctx0, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    obj_acns(ObjIn, ActionsIn),
    obj_id(ObjIn, ID),
    execute_action(
        action(set_attr(hp, 100)),
        actions_old(ActionsIn),
        obj_id(ID),
        result(completed, actions_new(ActionsOut)),
        Ctx,
        Ctx1
    ),
    obj_acns_obj(ObjIn, ActionsOut, Obj1),
    obj_acns(Obj1, ActionsIn2),
    obj_id(Obj1, ID2),
    execute_action(
        action(set_attr(hp, 75)),
        actions_old(ActionsIn2),
        obj_id(ID2),
        result(completed, actions_new(ActionsOut2)),
        Ctx1,
        Ctx2
    ),
    obj_acns_obj(Obj1, ActionsOut2, Obj2),
    obj_acns(Obj2, ActionsIn3),
    obj_id(Obj2, ID3),
    execute_action(
        action(set_attr(hp, 50)),
        actions_old(ActionsIn3),
        obj_id(ID3),
        result(completed, actions_new(ActionsOut3)),
        Ctx2,
        CtxNew
    ),
    obj_acns_obj(Obj2, ActionsOut3, _ObjOut),
    ctx_attr_val(1/hp, HP, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (HP = 50 ; err_write("HP != 50"))
)).

