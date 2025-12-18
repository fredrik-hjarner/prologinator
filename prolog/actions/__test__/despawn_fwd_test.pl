:- module(despawn_fwd_test, []).

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
% Tests: despawn
% ==========================================================

test("despawn: despawns object and prevents remaining \
actions from executing", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ObjIn = object(
        id(1),
        actions([
            despawn,
            wait(5),
            move_to(10, 10, 3),
            set_attr(hp, 100)
        ])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(type, static), attr(x, 0), attr(y, 0)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    obj_acns(ObjIn, ActionsIn),
    obj_id(ObjIn, ID),
    execute_action(
        action(despawn),
        actions_old(ActionsIn),
        obj_id(ID),
        result(Status, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    obj_acns_obj(ObjIn, ActionsOut, _ObjOut),
    ctx_cmds(Commands, CtxNew, CtxNew),
    ctx_frame(Frame, CtxNew, CtxNew),
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
        actions([
            despawn,
            trigger_state_change(game_over(won)),
            wait(5)
        ])
    ),
    empty_ctx(Ctx0),
    ctx_set_attr_val(1/type, static, Ctx0, Ctx),
    obj_acns(ObjIn, ActionsIn),
    obj_id(ObjIn, ID),
    execute_action(
        action(despawn),
        actions_old(ActionsIn),
        obj_id(ID),
        result(Status, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    obj_acns_obj(ObjIn, ActionsOut, _ObjOut),
    % Object must be despawned
    Status = despawned,
    % Despawn hint must be recorded
    % Status must remain playing (game_over did NOT execute)
    ctx_status(playing, CtxNew, CtxNew),
    % No commands
    ctx_cmds([], CtxNew, CtxNew)
)).

