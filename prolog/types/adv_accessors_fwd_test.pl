:- module(adv_accessors_fwd_test, []).

:- use_module('../../build/prologinator').
:- use_module('../../prolog/util/test_util').

:- use_module(library(assoc), [
    empty_assoc/1,
    put_assoc/4
]).
:- use_module(library(lists), [member/2]).

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
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
        [attr(x, 5), attr(y, 10)], Attrs),
    ctx_with_attrs(Attrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    ctx_attr_val(Ctx, 1/x, X),
    ctx_attr_val(Ctx, 1/y, Y),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    expect(X = 5),
    expect(Y = 10)
)).

test("ctx_attr_val: fails when object doesn't exist", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    empty_attr_store(EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act & Assert
    % ------------------------------------------------------
    \+ ctx_attr_val(Ctx, 1/x, _)
)).

test("ctx_attr_val: fails when attribute doesn't exist", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 5)], Attrs),
    ctx_with_attrs(Attrs, Ctx),
    % ------------------------------------------------------
    % Act & Assert
    % ------------------------------------------------------
    \+ ctx_attr_val(Ctx, 1/y, _)
)).

test("ctx_attr_val: enumerate Value from ObjectID and \
Key", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
        [attr(x, 5), attr(y, 10)], Attrs),
    ctx_with_attrs(Attrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    findall(Val, ctx_attr_val(Ctx, 1/x, Val), Vals),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    expect(member(5, Vals)),
    expect(\+ member(10, Vals))
)).

% ==========================================================
% Mode: ctx_set_attr_val(+ObjectID/+Key, +Value,
%   +CtxIn, -CtxOut)
% ==========================================================

test("ctx_set_attr_val: create new attribute on new \
object", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    empty_attr_store(EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, CtxIn),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    ctx_set_attr_val(1/x, 5, CtxIn, CtxOut),
    ctx_attr_val(CtxOut, 1/x, X),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    expect(X = 5)
)).

test("ctx_set_attr_val: add attribute to existing object", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 5)], AttrsIn),
    ctx_with_attrs(AttrsIn, CtxIn),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    ctx_set_attr_val(1/y, 10, CtxIn, CtxOut),
    ctx_attr_val(CtxOut, 1/x, X),
    ctx_attr_val(CtxOut, 1/y, Y),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    expect(X = 5),
    expect(Y = 10)
)).

test("ctx_set_attr_val: replace existing attribute", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
        [attr(x, 5), attr(y, 10)], AttrsIn),
    ctx_with_attrs(AttrsIn, CtxIn),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    ctx_set_attr_val(1/x, 20, CtxIn, CtxOut),
    ctx_attr_val(CtxOut, 1/x, X),
    ctx_attr_val(CtxOut, 1/y, Y),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    expect(X = 20),
    expect(Y = 10)
)).

test("ctx_set_attr_val: multiple updates preserve \
other attributes", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    empty_attr_store(EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx0),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    ctx_set_attr_val(1/x, 5, Ctx0, Ctx1),
    ctx_set_attr_val(1/y, 10, Ctx1, Ctx2),
    ctx_set_attr_val(1/hp, 100, Ctx2, CtxOut),
    ctx_attr_val(CtxOut, 1/x, X),
    ctx_attr_val(CtxOut, 1/y, Y),
    ctx_attr_val(CtxOut, 1/hp, HP),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    expect(X = 5),
    expect(Y = 10),
    expect(HP = 100)
)).

test("ctx_set_attr_val: multiple objects independent", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    empty_attr_store(EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx0),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    ctx_set_attr_val(1/x, 5, Ctx0, Ctx1),
    ctx_set_attr_val(2/x, 10, Ctx1, CtxOut),
    ctx_attr_val(CtxOut, 1/x, X1),
    ctx_attr_val(CtxOut, 2/x, X2),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    expect(X1 = 5),
    expect(X2 = 10)
)).

