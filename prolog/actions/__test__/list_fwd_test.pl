:- module(list_fwd_test, []).
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
% Tests: list
% ==========================================================

test("list: expands actions into queue", (
    ObjIn = object(
        id(0), type(static),
        actions([
            list([wait(1), move_to(5, 5, 2)]),
            wait(3)
        ]),
        collisions([])
    ),
    empty_ctx(Ctx),
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(list([wait(1), move_to(5, 5, 2)])),
        obj_old(ObjIn),
        result(completed, ObjOut)
    ),
    ObjOut = object(
        id(0), type(static),
        actions([wait(1), move_to(5, 5, 2), wait(3)]),
        collisions([])
    ),
    ctx_cmds(CtxNew, []),
    ctx_frame(CtxNew, 0),
    ctx_status(CtxNew, playing)
)).

test("list: empty list removes itself", (
    ObjIn = object(
        id(0), type(static),
        actions([list([]), wait(1)]),
        collisions([])
    ),
    empty_ctx(Ctx),
    execute_action(
        ctx_old(Ctx),
        ctx_new(_),
        action(list([])),
        obj_old(ObjIn),
        result(completed, ObjOut)
    ),
    ObjOut = object(
        id(0), type(static),
        actions([wait(1)]),
        collisions([])
    )
)).

