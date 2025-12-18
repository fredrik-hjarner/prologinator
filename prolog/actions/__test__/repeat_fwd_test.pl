:- module(repeat_fwd_test, []).

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
% Tests: repeat
% ==========================================================

test("repeat: expands actions once and decrements", (
    ObjIn = object(
        id(1),
        actions([
            repeat(3, [noop, set_attr(count, 1)]),
            despawn
        ])
    ),
    empty_ctx(Ctx0),
    ctx_set_attr_val(1/type, static, Ctx0, Ctx),
    execute_action(
        action(repeat(3, [noop, set_attr(count, 1)])),
        obj_old(ObjIn),
        result(completed, ObjOut),
        Ctx,
        CtxNew
    ),
    % Should expand to: [noop, set_attr(count, 1),
    %   repeat(2, [noop, set_attr(count, 1)]), despawn]
    obj_acns(ObjOut, Actions),
    Actions = [
        noop,
        set_attr(count, 1),
        repeat(2, [noop, set_attr(count, 1)]),
        despawn
    ],
    ctx_cmds([], CtxNew, CtxNew)
)).

test("repeat: last repetition doesn't add repeat", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ObjIn = object(
        id(1),
        actions([repeat(1, [noop]), despawn])
    ),
    empty_ctx(Ctx0),
    ctx_set_attr_val(1/type, static, Ctx0, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(repeat(1, [noop])),
        obj_old(ObjIn),
        result(completed, ObjOut),
        Ctx,
        CtxNew
    ),
    obj_acns(ObjOut, Actions),
    ctx_cmds(Commands, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (Actions = [noop, despawn]
     ;
     err_write("Actions wrong")),
    (Commands = [] ; err_write("Commands != []"))
)).

test("repeat: multiple actions in repeat list", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ObjIn = object(
        id(1),
        actions([
            repeat(2, [
                noop,
                set_attr(a, 1),
                set_attr(b, 2)
            ]),
            despawn
        ])
    ),
    empty_ctx(Ctx0),
    ctx_set_attr_val(1/type, static, Ctx0, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(repeat(2, [
            noop,
            set_attr(a, 1),
            set_attr(b, 2)
        ])),
        obj_old(ObjIn),
        result(completed, ObjOut),
        Ctx,
        CtxNew
    ),
    obj_acns(ObjOut, Actions),
    ctx_cmds(Commands, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (Actions = [
        noop,
        set_attr(a, 1),
        set_attr(b, 2),
        repeat(1, [noop, set_attr(a, 1), set_attr(b, 2)]),
        despawn
    ] ; err_write("Actions wrong")),
    (Commands = [] ; err_write("Commands != []"))
)).

