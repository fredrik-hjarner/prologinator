:- module(adv_accessors_bwd_test, []).

:- use_module('../../build/prologinator').
:- use_module('../../prolog/util/test_util').

:- use_module(library(assoc), [
    put_assoc/4
]).
:- use_module(library(lists), [member/2]).

% ==========================================================
% Backward Tests for adv_accessors
% ObjectID can be non-ground (bidirectional)
% ==========================================================

% ==========================================================
% Mode: ctx_attr_val(+Ctx, -ObjectID/-Key, -Value, -Value)
% ==========================================================
% NOTE: Untested modes with non-ground Ctx:
%   - ctx_attr_val(-Ctx, +ObjectID/+Key, +Value, +Value)
%   - ctx_attr_val(-Ctx, -ObjectID/+Key, +Value, +Value)
%   - ctx_attr_val(-Ctx, +ObjectID/-Key, +Value, +Value)
%   - ctx_attr_val(-Ctx, -ObjectID/-Key, +Value, +Value)
%   - ctx_attr_val(-Ctx, +ObjectID/+Key, -Value, -Value)
%   - ctx_attr_val(-Ctx, -ObjectID/+Key, -Value, -Value)
%   - ctx_attr_val(-Ctx, +ObjectID/-Key, -Value, -Value)
%   - ctx_attr_val(-Ctx, -ObjectID/-Key, -Value, -Value)
%   (All modes where Ctx is non-ground are untested)

test("ctx_attr_val: can enumerate ObjectID from attribute \
value", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 5)], Attrs1),
    put_assoc(2, Attrs1, [attr(x, 10)], Attrs2),
    put_assoc(3, Attrs2, [attr(x, 5)], Attrs),
    ctx_with_attrs(Attrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    findall(ID, ctx_attr_val(ID/x, 5, Ctx, Ctx), IDs),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    member(1, IDs),
    member(3, IDs),
    \+ member(2, IDs)
)).

test("ctx_attr_val: can enumerate ObjectID and Key from \
value", (
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
    findall(
        ID-Key, ctx_attr_val(ID/Key, 5, Ctx, Ctx), Pairs
    ),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    member(1-x, Pairs),
    \+ member(1-y, Pairs)
)).

test("ctx_attr_val: can enumerate Key from ObjectID and \
value", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
        [attr(x, 5), attr(y, 10), attr(hp, 100)], Attrs),
    ctx_with_attrs(Attrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    findall(Key, ctx_attr_val(1/Key, 10, Ctx, Ctx), Keys),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    member(y, Keys),
    \+ member(x, Keys),
    \+ member(hp, Keys)
)).

test("ctx_attr_val: bidirectional - can infer ObjectID \
from known Key and Value", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 5)], Attrs1),
    put_assoc(2, Attrs1, [attr(x, 10)], Attrs),
    ctx_with_attrs(Attrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    ctx_attr_val(ID/x, 5, Ctx, Ctx),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    expect(ID = 1)
)).

test("ctx_attr_val: enumerate ObjectID and Value from \
Key", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 5)], Attrs1),
    put_assoc(2, Attrs1, [attr(x, 10)], Attrs),
    ctx_with_attrs(Attrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    findall(
        ID-Val, ctx_attr_val(ID/x, Val, Ctx, Ctx), Pairs
    ),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    member(1-5, Pairs),
    member(2-10, Pairs)
)).

test("ctx_attr_val: enumerate Key and Value from \
ObjectID", (
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
    findall(
        Key-Val, ctx_attr_val(1/Key, Val, Ctx, Ctx), Pairs
    ),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    member(x-5, Pairs),
    member(y-10, Pairs)
)).

test("ctx_attr_val: enumerate all from Value", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 5)], Attrs1),
    put_assoc(2, Attrs1, [attr(y, 5)], Attrs),
    ctx_with_attrs(Attrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    findall(
        ID-Key, ctx_attr_val(ID/Key, 5, Ctx, Ctx), Pairs
    ),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    member(1-x, Pairs),
    member(2-y, Pairs)
)).

% ==========================================================
% Mode: ctx_set_attr_val(-ObjectID/-Key, +Value,
%   +CtxIn, -CtxOut)
% ==========================================================
% NOTE: Untested modes with non-ground CtxIn:
%   - ctx_set_attr_val(+ObjectID/+Key, +Value,
%       -CtxIn, -CtxOut)
%   - ctx_set_attr_val(-ObjectID/+Key, +Value,
%       -CtxIn, -CtxOut)
%   - ctx_set_attr_val(+ObjectID/-Key, +Value,
%       -CtxIn, -CtxOut)
%   - ctx_set_attr_val(-ObjectID/-Key, +Value,
%       -CtxIn, -CtxOut)
%   (All modes where CtxIn is non-ground are untested)

test("ctx_set_attr_val: can use non-ground Key for \
writes (attr/2 format enables bidirectionality)", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    empty_attr_store(EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, CtxIn),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    Key = x,
    ctx_set_attr_val(1/Key, 5, CtxIn, CtxOut),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ctx_attr_val(1/x, 5, CtxOut, CtxOut)
)).

test("ctx_set_attr_val: can use non-ground ObjectID for \
writes", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    empty_attr_store(EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, CtxIn),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    ID = 1,
    ctx_set_attr_val(ID/x, 5, CtxIn, CtxOut),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ctx_attr_val(1/x, 5, CtxOut, CtxOut)
)).

test("ctx_set_attr_val: can use non-ground ObjectID \
and Key for writes", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    empty_attr_store(EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, CtxIn),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    ID = 1,
    Key = x,
    ctx_set_attr_val(ID/Key, 5, CtxIn, CtxOut),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ctx_attr_val(1/x, 5, CtxOut, CtxOut)
)).

