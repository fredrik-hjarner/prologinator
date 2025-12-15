:- module(list_fwd_test, []).

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
% Tests: list
% ==========================================================

test("list: executes actions and yields when child \
yields", (
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
        action(list([wait(1), move_to(5, 5, 2)])),
        obj_old(ObjIn),
        result(Status, ObjOut),
        Ctx,
        CtxNew
    ),
    % list now executes actions immediately using
    % tick_object
    % wait(1) yields and removes itself, so list yields
    % with remaining actions: [move_to(5, 5, 2)]
    Status = yielded,
    obj_acns(ObjOut, Actions),
    % Remaining actions should be wrapped back in list
    Actions = [
        list([move_to(5, 5, 2)]), wait(3)
    ],
    ctx_cmds([], CtxNew),
    ctx_frame(0, CtxNew),
    ctx_status(playing, CtxNew)
)).

test("list: empty list removes itself", (
    ObjIn = object(
        id(0), type(static),
        actions([list([]), wait(1)]),
        collisions([])
    ),
    empty_ctx(Ctx),
    execute_action(
        action(list([])),
        obj_old(ObjIn),
        result(completed, ObjOut),
        Ctx,
        _
    ),
    ObjOut = object(
        id(0), type(static),
        actions([wait(1)]),
        collisions([])
    )
)).

