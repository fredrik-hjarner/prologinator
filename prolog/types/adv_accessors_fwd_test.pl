:- module(adv_accessors_fwd_test, []).
:- use_module('./adv_accessors', [
    ctx_attr_val/3,
    ctx_attr_val_ctx/4
]).
:- use_module('./accessors').
:- use_module('../util/util', [err_write/1]).
:- use_module(library(assoc), [
    empty_assoc/1,
    put_assoc/4
]).

% ==========================================================
% Forward Tests for adv_accessors
% All ObjectIDs are ground
% ==========================================================

% ==========================================================
% Mode: ctx_attr_val(+Ctx, +ObjectID/+Key, -Value)
% ==========================================================

test("ctx_attr_val: read existing attribute", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    empty_assoc(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 5), attr(y, 10)], Attrs),
    Ctx = ctx(state(
        frame(0),
        objects([]),
        attrs(Attrs),
        status(playing),
        next_id(1),
        commands([]),
        rev_hints([])
    )),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    ctx_attr_val(Ctx, 1/x, X),
    ctx_attr_val(Ctx, 1/y, Y),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (X = 5; err_write("X != 5")),
    (Y = 10; err_write("Y != 10"))
)).

test("ctx_attr_val: fails when object doesn't exist", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    empty_assoc(EmptyAttrs),
    Ctx = ctx(state(
        frame(0),
        objects([]),
        attrs(EmptyAttrs),
        status(playing),
        next_id(1),
        commands([]),
        rev_hints([])
    )),
    % ------------------------------------------------------
    % Act & Assert
    % ------------------------------------------------------
    \+ ctx_attr_val(Ctx, 1/x, _)
)).

test("ctx_attr_val: fails when attribute doesn't exist", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    empty_assoc(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 5)], Attrs),
    Ctx = ctx(state(
        frame(0),
        objects([]),
        attrs(Attrs),
        status(playing),
        next_id(1),
        commands([]),
        rev_hints([])
    )),
    % ------------------------------------------------------
    % Act & Assert
    % ------------------------------------------------------
    \+ ctx_attr_val(Ctx, 1/y, _)
)).

test("ctx_attr_val: enumerate Value from ObjectID and Key", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    empty_assoc(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 5), attr(y, 10)], Attrs),
    Ctx = ctx(state(
        frame(0),
        objects([]),
        attrs(Attrs),
        status(playing),
        next_id(1),
        commands([]),
        rev_hints([])
    )),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    findall(Val, ctx_attr_val(Ctx, 1/x, Val), Vals),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    member(5, Vals),
    \+ member(10, Vals)
)).

% ==========================================================
% Mode: ctx_attr_val_ctx(+CtxIn, +ObjectID/+Key, +Value, -CtxOut)
% ==========================================================

test("ctx_attr_val_ctx: create new attribute on new object", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    empty_assoc(EmptyAttrs),
    CtxIn = ctx(state(
        frame(0),
        objects([]),
        attrs(EmptyAttrs),
        status(playing),
        next_id(1),
        commands([]),
        rev_hints([])
    )),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    ctx_attr_val_ctx(CtxIn, 1/x, 5, CtxOut),
    ctx_attr_val(CtxOut, 1/x, X),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (X = 5 ; err_write("X!=5"))
)).

test("ctx_attr_val_ctx: add attribute to existing object", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    empty_assoc(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 5)], AttrsIn),
    CtxIn = ctx(state(
        frame(0),
        objects([]),
        attrs(AttrsIn),
        status(playing),
        next_id(1),
        commands([]),
        rev_hints([])
    )),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    ctx_attr_val_ctx(CtxIn, 1/y, 10, CtxOut),
    ctx_attr_val(CtxOut, 1/x, X),
    ctx_attr_val(CtxOut, 1/y, Y),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (X = 5 ; err_write("X!=5")),
    (Y = 10 ; err_write("Y!=10"))
)).

test("ctx_attr_val_ctx: replace existing attribute", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    empty_assoc(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 5), attr(y, 10)], AttrsIn),
    CtxIn = ctx(state(
        frame(0),
        objects([]),
        attrs(AttrsIn),
        status(playing),
        next_id(1),
        commands([]),
        rev_hints([])
    )),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    ctx_attr_val_ctx(CtxIn, 1/x, 20, CtxOut),
    ctx_attr_val(CtxOut, 1/x, X),
    ctx_attr_val(CtxOut, 1/y, Y),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (X = 20 ; err_write("X!=20")),
    (Y = 10 ; err_write("Y!=10"))
)).

test("ctx_attr_val_ctx: multiple updates preserve other attributes", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    empty_assoc(EmptyAttrs),
    Ctx0 = ctx(state(
        frame(0),
        objects([]),
        attrs(EmptyAttrs),
        status(playing),
        next_id(1),
        commands([]),
        rev_hints([])
    )),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    ctx_attr_val_ctx(Ctx0, 1/x, 5, Ctx1),
    ctx_attr_val_ctx(Ctx1, 1/y, 10, Ctx2),
    ctx_attr_val_ctx(Ctx2, 1/hp, 100, CtxOut),
    ctx_attr_val(CtxOut, 1/x, X),
    ctx_attr_val(CtxOut, 1/y, Y),
    ctx_attr_val(CtxOut, 1/hp, HP),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (X = 5 ; err_write("X!=5")),
    (Y = 10 ; err_write("Y!=10")),
    (HP = 100 ; err_write("HP!=100"))
)).

test("ctx_attr_val_ctx: multiple objects independent", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    empty_assoc(EmptyAttrs),
    Ctx0 = ctx(state(
        frame(0),
        objects([]),
        attrs(EmptyAttrs),
        status(playing),
        next_id(1),
        commands([]),
        rev_hints([])
    )),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    ctx_attr_val_ctx(Ctx0, 1/x, 5, Ctx1),
    ctx_attr_val_ctx(Ctx1, 2/x, 10, CtxOut),
    ctx_attr_val(CtxOut, 1/x, X1),
    ctx_attr_val(CtxOut, 2/x, X2),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (X1 = 5 ; err_write("X1!=5")),
    (X2 = 10 ; err_write("X2!=10"))
)).

