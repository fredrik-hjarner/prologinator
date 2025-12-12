:- module(parallel_race_fwd_test, []).

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
% Tests: parallel_race
% ==========================================================

test("parallel_race: stops on child completion", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    Obj = object(
        id(0), type(tower),
        actions([
            parallel_race([
                wait(5),
                noop,
                wait(10)
            ]),
            wait(3)
        ]),
        collisions([])
    ),
    empty_ctx(CtxTemp),
    ctx_objs_ctx(CtxTemp, [Obj], Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(_),
        action(parallel_race(
            [wait(5), noop, wait(10)]
        )),
        obj_old(Obj),
        result(completed, NewObj)
    ),
    obj_acns(NewObj, Actions),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (Actions = [wait(3)|_] ; err_write("Actions wrong"))
)).

test("parallel_race: continues if no child \
done", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    Obj = object(
        id(0), type(tower),
        actions([
            parallel_race([
                wait(5), wait(10)
            ]),
            wait(3)
        ]),
        collisions([])
    ),
    empty_ctx(CtxTemp),
    ctx_objs_ctx(CtxTemp, [Obj], Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(_),
        action(parallel_race(
            [wait(5), wait(10)]
        )),
        obj_old(Obj),
        result(completed, NewObj)
    ),
    obj_acns(NewObj, Actions),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (Actions = [parallel_race_running([wait(4), wait(9)])|_]
     ;
     err_write("Actions wrong"))
)).

test("parallel_race: despawns parent when child \
despawns", (
    Obj = object(
        id(1),
        type(static),
        actions([
            parallel_race([
                despawn,
                wait(10)
            ]),
            trigger_state_change(game_over(won)),
            wait(5)
        ]),
        collisions([])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 0), attr(y, 0)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(parallel_race([despawn, wait(10)])),
        obj_old(Obj),
        result(ActionStatus, _ObjOut)
    ),
    ctx_status(CtxNew, Status),
    ctx_cmds(CtxNew, Commands),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (ActionStatus = despawned
    ; err_write("ActionStatus != despawned")),
    (Status = playing ; err_write("Status != playing")),
    (Commands = [] ; err_write("Commands != []"))
)).

