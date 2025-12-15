:- module(move_delta_fwd_test, []).

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
% Tests: move_delta
% ==========================================================

test("move_delta: single frame moves and completes", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ObjIn = object(
        id(1),
        type(static),
        actions([move_delta(1, 5, -3)]),
        collisions([])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 10), attr(y, 20)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(move_delta(1, 5, -3)),
        obj_old(ObjIn),
        result(yielded, ObjOut),
        Ctx,
        CtxNew
    ),
    ctx_attr_val(CtxNew, 1/x, X),
    ctx_attr_val(CtxNew, 1/y, Y),
    ctx_cmds(Commands, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    expect(ObjOut = object(
        id(1),
        type(static),
        actions([]),
        collisions([])
    ), 'ObjOut != object(id(1), type(static), actions(...'),
    expect(X = 15, 'X != 15'),
    expect(Y = 17, 'Y != 17'),
    expect(Commands = [], 'Commands != []')
)).

test("move_delta: multiple frames continues", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ObjIn = object(
        id(1),
        type(static),
        actions([move_delta(3, 10, 5)]),
        collisions([])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 0), attr(y, 0)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(move_delta(3, 10, 5)),
        obj_old(ObjIn),
        result(yielded, ObjOut),
        Ctx,
        CtxNew
    ),
    ctx_attr_val(CtxNew, 1/x, X),
    ctx_attr_val(CtxNew, 1/y, Y),
    obj_acns(ObjOut, Actions),
    ctx_cmds(Commands, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (X = 10 ; err_write("X != 10")),
    (Y = 5 ; err_write("Y != 5")),
    (Actions = [move_delta(2, 10, 5)]
     ;
     err_write("Actions wrong")),
    (Commands = [] ; err_write("Commands != []"))
)).

test("move_delta: negative deltas work", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ObjIn = object(
        id(1),
        type(static),
        actions([move_delta(2, -10, -5)]),
        collisions([])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 50), attr(y, 50)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(move_delta(2, -10, -5)),
        obj_old(ObjIn),
        result(yielded, ObjOut),
        Ctx,
        CtxNew
    ),
    ctx_attr_val(CtxNew, 1/x, X),
    ctx_attr_val(CtxNew, 1/y, Y),
    obj_acns(ObjOut, Actions),
    ctx_cmds(Commands, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (X = 40 ; err_write("X != 40")),
    (Y = 45 ; err_write("Y != 45")),
    (Actions = [move_delta(1, -10, -5)]
     ;
     err_write("Actions wrong")),
    (Commands = [] ; err_write("Commands != []"))
)).

test("move_delta: preserves other attributes", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ObjIn = object(
        id(1),
        type(static),
        actions([move_delta(1, 5, -3)]),
        collisions([])
    ),
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
        action(move_delta(1, 5, -3)),
        obj_old(ObjIn),
        result(yielded, ObjOut),
        Ctx,
        CtxNew
    ),
    ctx_attr_val(CtxNew, 1/x, X),
    ctx_attr_val(CtxNew, 1/y, Y),
    ctx_attr_val(CtxNew, 1/hp, HP),
    ctx_attr_val(CtxNew, 1/speed, Speed),
    ctx_cmds(Commands, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ObjOut = object(
        id(1),
        type(static),
        actions([]),
        collisions([])
    ),
    (X = 15 ; err_write("X != 15")),
    (Y = 17 ; err_write("Y != 17")),
    (HP = 100 ; err_write("HP != 100")),
    (Speed = 5 ; err_write("Speed != 5")),
    (Commands = [] ; err_write("Commands != []"))
)).

