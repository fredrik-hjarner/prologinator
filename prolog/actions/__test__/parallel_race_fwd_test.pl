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
    ctx_set_objs([Obj], CtxTemp, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(parallel_race(
            [wait(5), noop, wait(10)]
        )),
        obj_old(Obj),
        result(completed, NewObj),
        Ctx,
        _
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
    ctx_set_objs([Obj], CtxTemp, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(parallel_race(
            [wait(5), wait(10)]
        )),
        obj_old(Obj),
        result(Status, NewObj),
        Ctx,
        _
    ),
    obj_acns(NewObj, Actions),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (Status = yielded
     ; err_write("Status != yielded")),
    (Actions = [parallel_race([wait(4), wait(9)])|_]
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
        action(parallel_race([despawn, wait(10)])),
        obj_old(Obj),
        result(ActionStatus, _ObjOut),
        Ctx,
        CtxNew
    ),
    ctx_status(Status, CtxNew, CtxNew),
    ctx_cmds(Commands, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (ActionStatus = despawned
    ; err_write("ActionStatus != despawned")),
    (Status = playing ; err_write("Status != playing")),
    (Commands = [] ; err_write("Commands != []"))
)).

% TODO: Complete and fix this test.
% % Test: parallel_all despawns when a child despawns
% test("parallel_race: despawns when child despawns", (
%     % ----------------------------------------------------
%     % Arrange
%     % ----------------------------------------------------
%     ParAcn = parallel_all([
%         loop([
%             wait(1), incr(a, 1)
%         ]),
%         loop([
%             wait(2), incr(b, 1)
%         ]),
%         wait(10) % 1*10 = 2*5
%     ]),
%     Actions = list([
%         ParAcn,
%         despawn
%     ]),
%     ObjIn = object(
%         id(1),
%         type(static),
%         actions([Actions]),
%         collisions([])
%     ),
%     empty_attr_store(EmptyAttrs0),
%     put_assoc(
%         1,EmptyAttrs0,[attr(a, 0), attr(b, 0)], EmptyAttrs
%     ),
%     ctx_with_attrs(EmptyAttrs, Ctx),
%     % ----------------------------------------------------
%     % Act - Frame 0 -> 1
%     % ----------------------------------------------------
%     execute_action(
%         ctx_old(Ctx), ctx_new(CtxNew),
%         action(Actions),
%         obj_old(ObjIn),
%         result(Status, ObjOut)
%     ),
%     % ----------------------------------------------------
%     % Assert - frame 1
%     % ----------------------------------------------------
%     expect(Status = yielded),
%     % a should be 0, b should be 0
%     expect(ctx_attr_val(1/a, 0, CtxNew)),
%     expect(ctx_attr_val(1/b, 0, CtxNew)),
%     % ----------------------------------------------------
%     % Act - Frame 1 -> 9 (8 more ticks)
%     % ----------------------------------------------------
%     tick_n(8, CtxNew, ObjOut, CtxFinal, ObjFinal),
%     % ----------------------------------------------------
%     % Assert - frame 9
%     % ----------------------------------------------------
%     % state should be ehm despawned
%     obj_acns(ObjFinal, Frame9Actions),
%     write_term(Frame9Actions, []), nl,
%     % ----------------------------------------------------
%     % Act - Frame 9 -> 10 (9 more ticks)
%     % ----------------------------------------------------
%     tick_n(1, CtxFinal, ObjFinal, Ctx10, Obj10),
%     % ----------------------------------------------------
%     % Assert - frame 10
%     % ----------------------------------------------------
%     % state should be ehm despawned
%     obj_acns(Obj10, Frame10Actions),
%     write_term(Frame10Actions, []), nl
%     % expect(ctx_status(CtxFinal, despawned))
% )).