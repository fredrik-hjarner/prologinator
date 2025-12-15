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
    ObjIn = object(
        id(1),
        type(static),
        actions([move_to(10, 20, 3)]),
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
        action(Action),
        obj_old(ObjIn),
        result(Status, ObjOut),
        Ctx,
        CtxNew
    ),
    ctx_cmds(Commands, CtxNew, CtxNew),
    ctx_attr_val(1/x, NewX, CtxNew, CtxNew),
    ctx_attr_val(1/y, NewY, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (Status = yielded ; err_write("Status != yielded")),
    ( ObjOut = object(
        id(1),
        type(static),
        actions([move_to(10, 20, 2)|_]),
        collisions([])
    ) ; err_write("ObjOut mismatch") ),
    (NewX = 3 ; err_write("NewX != 3")),
    (NewY = 6 ; err_write("NewY != 6")),
    (Commands = [] ; err_write("Commands != []"))
)).

test("move_to: negative direction, multiple frames \
remaining", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    Action = move_to(0, 0, 3),
    ObjIn = object(
        id(1),
        type(static),
        actions([move_to(0, 0, 3)]),
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
        action(Action),
        obj_old(ObjIn),
        result(Status, ObjOut),
        Ctx,
        CtxNew
    ),
    ctx_cmds(Commands, CtxNew, CtxNew),
    ctx_attr_val(1/x, NewX, CtxNew, CtxNew),
    ctx_attr_val(1/y, NewY, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (Status = yielded ; err_write("Status != yielded")),
    ObjOut = object(
        id(1),
        type(static),
        actions([move_to(0, 0, 2)|_]),
        collisions([])
    ),
    (NewX = 7 ; err_write("NewX != 7")),
    (NewY = 14 ; err_write("NewY != 14")),
    (Commands = [] ; err_write("Commands != []"))
)).

test("move_to: single frame, arrives at target", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    Action = move_to(5, 5, 1),
    ObjIn = object(
        id(1),
        type(static),
        actions([move_to(5, 5, 1)]),
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
        action(Action),
        obj_old(ObjIn),
        result(Status, ObjOut),
        Ctx,
        CtxNew
    ),
    ctx_cmds(Commands, CtxNew, CtxNew),
    ctx_attr_val(1/x, X, CtxNew, CtxNew),
    ctx_attr_val(1/y, Y, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (Status = yielded ; err_write("Status != yielded")),
    ObjOut = object(
        id(1),
        type(static),
        actions([]),
        collisions([])
    ),
    (X = 5 ; err_write("X != 5")),
    (Y = 5 ; err_write("Y != 5")),
    (Commands = [] ; err_write("Commands != []"))
)).

test("move_to: already at target, stays at position and \
continues with remaining frames", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    Action = move_to(10, 20, 3),
    ObjIn = object(
        id(1),
        type(static),
        actions([move_to(10, 20, 3)]),
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
        action(Action),
        obj_old(ObjIn),
        result(Status, ObjOut),
        Ctx,
        CtxNew
    ),
    ctx_cmds(Commands, CtxNew, CtxNew),
    ctx_attr_val(1/x, X, CtxNew, CtxNew),
    ctx_attr_val(1/y, Y, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (Status = yielded ; err_write("Status != yielded")),
    ObjOut = object(
        id(1),
        type(static),
        actions([move_to(10, 20, 2)|_]),
        collisions([])
    ),
    (X = 10 ; err_write("X != 10")),
    (Y = 20 ; err_write("Y != 20")),
    (Commands = [] ; err_write("Commands != []"))
)).

test("move_to: negative target coordinates", (
    Action = move_to(-5, -10, 2),
    ObjIn = object(
        id(1),
        type(static),
        actions([move_to(-5, -10, 2)]),
        collisions([])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 0), attr(y, 0)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    execute_action(
        action(Action),
        obj_old(ObjIn),
        result(Status, ObjOut),
        Ctx,
        CtxNew
    ),
    ctx_cmds(Commands, CtxNew, CtxNew),
    ctx_attr_val(1/x, NewX, CtxNew, CtxNew),
    ctx_attr_val(1/y, NewY, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (Status = yielded ; err_write("Status != yielded")),
    ObjOut = object(
        id(1),
        type(static),
        actions([move_to(-5, -10, 1)|_]),
        collisions([])
    ),
    (NewX = -2 ; err_write("NewX != -2")),
    (NewY = -5 ; err_write("NewY != -5")),
    (Commands = [] ; err_write("Commands != []"))
)).

