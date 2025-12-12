:- module(repeat_fwd_test, []).
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
% Tests: repeat
% ==========================================================

test("repeat: expands actions once and decrements", (
    ObjIn = object(
        id(1),
        type(static),
        actions([
            repeat(3, [noop, set_attr(count, 1)]),
            despawn
        ]),
        collisions([])
    ),
    empty_ctx(Ctx),
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(repeat(3, [noop, set_attr(count, 1)])),
        obj_old(ObjIn),
        obj_new([ObjOut])
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
    ctx_cmds(CtxNew, [])
)).

test("repeat: last repetition doesn't add repeat", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ObjIn = object(
        id(1),
        type(static),
        actions([repeat(1, [noop]), despawn]),
        collisions([])
    ),
    empty_ctx(Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(repeat(1, [noop])),
        obj_old(ObjIn),
        obj_new([ObjOut])
    ),
    obj_acns(ObjOut, Actions),
    ctx_cmds(CtxNew, Commands),
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
        type(static),
        actions([
            repeat(2, [
                noop,
                set_attr(a, 1),
                set_attr(b, 2)
            ]),
            despawn
        ]),
        collisions([])
    ),
    empty_ctx(Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(repeat(2, [
            noop,
            set_attr(a, 1),
            set_attr(b, 2)
        ])),
        obj_old(ObjIn),
        obj_new([ObjOut])
    ),
    obj_acns(ObjOut, Actions),
    ctx_cmds(CtxNew, Commands),
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

