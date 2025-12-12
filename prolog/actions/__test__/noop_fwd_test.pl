:- module(noop_fwd_test, []).
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
% Tests: noop
% ==========================================================

test("noop: removes self from action queue", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ObjIn = object(
        id(0), type(static),
        actions([noop, wait(1)]), collisions([])
    ),
    empty_ctx(Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(noop),
        obj_old(ObjIn),
        obj_new([ObjOut])
    ),
    ctx_cmds(CtxNew, Commands),
    ctx_frame(CtxNew, Frame),
    ctx_status(CtxNew, Status),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ObjOut = object(
        id(0), type(static),
        actions([wait(1)]), collisions([])
    ),
    (Commands = [] ; err_write("Commands != []")),
    (Frame = 0 ; err_write("Frame != 0")),
    (Status = playing ; err_write("Status != playing"))
)).

