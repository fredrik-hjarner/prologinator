:- module(despawn_fwd_test, []).
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
% Tests: despawn
% ==========================================================

test("despawn: despawns object and prevents remaining \
actions from executing", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ObjIn = object(
        id(1),
        type(static),
        actions([
            despawn,
            wait(5),
            move_to(10, 10, 3),
            set_attr(hp, 100)
        ]),
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
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(despawn),
        obj_old(ObjIn),
        result(Status, _ObjOut)
    ),
    ctx_cmds(CtxNew, Commands),
    ctx_frame(CtxNew, Frame),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (Status = despawned ; err_write("Status != despawned")),
    (Commands = [] ; err_write("Commands != []")),
    (Frame = 0 ; err_write("Frame != 0"))
)).

test("despawn: prevents game_over state change from \
executing after despawn", (
    ObjIn = object(
        id(1),
        type(static),
        actions([
            despawn,
            trigger_state_change(game_over(won)),
            wait(5)
        ]),
        collisions([])
    ),
    empty_ctx(Ctx),
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(despawn),
        obj_old(ObjIn),
        result(Status, _ObjOut)
    ),
    % Object must be despawned
    Status = despawned,
    % Despawn hint must be recorded
    % Status must remain playing (game_over did NOT execute)
    ctx_status(CtxNew, playing),
    % No commands
    ctx_cmds(CtxNew, [])
)).

