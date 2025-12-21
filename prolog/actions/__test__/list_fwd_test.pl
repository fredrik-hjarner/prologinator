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
    ActionsIn = [
        list([wait(1), move_to(5, 5, 2)]),
        wait(3)
    ],
    empty_ctx(Ctx0),
    ctx_set_attr_val(0/type, static, Ctx0, Ctx),
    execute_action(
        actions_old(ActionsIn),
        obj_id(0),
        result(Status, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    % list now executes actions immediately using
    % tick_object
    % wait(1) yields and removes itself, so list yields
    % with remaining actions: [move_to(5, 5, 2)]
    Status = yielded,
    % Remaining actions should be wrapped back in list
    ActionsOut = [
        list([move_to(5, 5, 2)]), wait(3)
    ],
    ctx_spawnCmds([], CtxNew, CtxNew),
    ctx_frame(0, CtxNew, CtxNew),
    ctx_status(playing, CtxNew, CtxNew)
)).

test("list: empty list removes itself", (
    ActionsIn = [list([]), wait(1)],
    empty_ctx(Ctx0),
    ctx_set_attr_val(0/type, static, Ctx0, Ctx),
    execute_action(
        actions_old(ActionsIn),
        obj_id(0),
        result(completed, actions_new(ActionsOut)),
        Ctx,
        _
    ),
    ActionsOut = [wait(1)]
)).

