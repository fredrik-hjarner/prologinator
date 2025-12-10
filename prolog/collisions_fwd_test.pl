:- module(collisions_fwd_test, []).
:- use_module('./collisions', [detect_collisions/2]).
:- use_module('./types/accessors').
:- use_module('./types/constructors', [empty_ctx/1]).
:- use_module(library(assoc), [
    empty_assoc/1,
    put_assoc/4
]).
:- use_module(library(lists), [length/2, member/2]).

% ==========================================================
% Forward Tests for Collision Detection
% ==========================================================

% --------------------------------------------------------
% Tests: No collisions
% --------------------------------------------------------

test("collisions: no collisions when objects are at \
different positions", (
    empty_assoc(EmptyAttrs0),
    put_assoc(0, EmptyAttrs0, [attr(x, 5), attr(y, 5)],
              Attrs1),
    put_assoc(1, Attrs1, [attr(x, 10), attr(y, 10)],
              EmptyAttrs),
    State = state(
        frame(0),
        objects([
            object(id(0), type(enemy), actions([]),
                   collisions([])),
            object(id(1), type(proj), actions([]),
                   collisions([]))
        ]),
        attrs(EmptyAttrs),
        status(playing),
        next_id(2),
        commands([])
    ),
    CtxIn = ctx(State),
    detect_collisions(
        ctx_old(CtxIn),
        ctx_new(CtxOut)
    ),
    ctx_objs(CtxOut, NewObjects),
    NewObjects = [
        object(id(0), type(enemy), actions([]),
               collisions([])),
        object(id(1), type(proj), actions([]),
               collisions([]))
    ]  % No objects removed
)).

% --------------------------------------------------------
% Tests: Enemy-Projectile collision
% --------------------------------------------------------

test("collisions: enemy and projectile at same position \
are both removed", (
    empty_assoc(EmptyAttrs0),
    put_assoc(0, EmptyAttrs0, [attr(x, 10), attr(y, 10)],
              Attrs1),
    put_assoc(1, Attrs1, [attr(x, 10), attr(y, 10)],
              EmptyAttrs),
    Objects = [
        object(id(0), type(enemy), actions([]),
               collisions([])),
        object(id(1), type(proj), actions([]),
               collisions([]))
    ],
    State = state(
        frame(0),
        objects(Objects),
        attrs(EmptyAttrs),
        status(playing),
        next_id(2),
        commands([])
    ),
    CtxIn = ctx(State),
    detect_collisions(
        ctx_old(CtxIn),
        ctx_new(CtxOut)
    ),
    ctx_objs(CtxOut, NewObjects),
    NewObjects = []  % Both objects removed
)).

test("collisions: enemy and projectile collision removes \
only colliding objects", (
    empty_assoc(EmptyAttrs0),
    put_assoc(0, EmptyAttrs0, [attr(x, 10), attr(y, 10)],
              Attrs1),
    put_assoc(1, Attrs1, [attr(x, 10), attr(y, 10)],
              Attrs2),
    put_assoc(2, Attrs2, [attr(x, 5), attr(y, 5)],
              EmptyAttrs),
    Objects = [
        object(id(0), type(enemy), actions([]),
               collisions([])),
        object(id(1), type(proj), actions([]),
               collisions([])),
        object(id(2), type(enemy), actions([]),
               collisions([]))
    ],
    State = state(
        frame(0),
        objects(Objects),
        attrs(EmptyAttrs),
        status(playing),
        next_id(3),
        commands([])
    ),
    CtxIn = ctx(State),
    detect_collisions(
        ctx_old(CtxIn),
        ctx_new(CtxOut)
    ),
    ctx_objs(CtxOut, NewObjects),
    NewObjects = [object(id(2), type(enemy), actions([]),
                         collisions([]))]  % Only non-colliding object remains
)).

% --------------------------------------------------------
% Tests: No collision between non-projectile types
% --------------------------------------------------------

test("collisions: enemy and enemy at same position do \
not collide", (
    empty_assoc(EmptyAttrs0),
    put_assoc(0, EmptyAttrs0, [attr(x, 10), attr(y, 10)],
              Attrs1),
    put_assoc(1, Attrs1, [attr(x, 10), attr(y, 10)],
              EmptyAttrs),
    Objects = [
        object(id(0), type(enemy), actions([]),
               collisions([])),
        object(id(1), type(enemy), actions([]),
               collisions([]))
    ],
    State = state(
        frame(0),
        objects(Objects),
        attrs(EmptyAttrs),
        status(playing),
        next_id(2),
        commands([])
    ),
    CtxIn = ctx(State),
    detect_collisions(
        ctx_old(CtxIn),
        ctx_new(CtxOut)
    ),
    ctx_objs(CtxOut, NewObjects),
    NewObjects = Objects  % No objects removed
)).

test("collisions: projectile and projectile at same \
position do not collide", (
    empty_assoc(EmptyAttrs0),
    put_assoc(0, EmptyAttrs0, [attr(x, 10), attr(y, 10)],
              Attrs1),
    put_assoc(1, Attrs1, [attr(x, 10), attr(y, 10)],
              EmptyAttrs),
    Objects = [
        object(id(0), type(proj), actions([]),
               collisions([])),
        object(id(1), type(proj), actions([]),
               collisions([]))
    ],
    State = state(
        frame(0),
        objects(Objects),
        attrs(EmptyAttrs),
        status(playing),
        next_id(2),
        commands([])
    ),
    CtxIn = ctx(State),
    detect_collisions(
        ctx_old(CtxIn),
        ctx_new(CtxOut)
    ),
    ctx_objs(CtxOut, NewObjects),
    NewObjects = Objects  % No objects removed
)).

% --------------------------------------------------------
% Tests: Objects without position attributes
% --------------------------------------------------------

test("collisions: objects without x/y attributes are \
ignored", (
    empty_assoc(EmptyAttrs),
    Objects = [
        object(id(0), type(enemy), actions([]),
               collisions([])),
        object(id(1), type(proj), actions([]),
               collisions([]))
    ],
    State = state(
        frame(0),
        objects(Objects),
        attrs(EmptyAttrs),
        status(playing),
        next_id(2),
        commands([])
    ),
    CtxIn = ctx(State),
    detect_collisions(
        ctx_old(CtxIn),
        ctx_new(CtxOut)
    ),
    ctx_objs(CtxOut, NewObjects),
    NewObjects = Objects  % No objects removed (no positions)
)).

test("collisions: object with only x attribute is \
ignored", (
    empty_assoc(EmptyAttrs0),
    put_assoc(0, EmptyAttrs0, [attr(x, 10)],
              EmptyAttrs),
    Objects = [
        object(id(0), type(enemy), actions([]),
               collisions([]))
    ],
    State = state(
        frame(0),
        objects(Objects),
        attrs(EmptyAttrs),
        status(playing),
        next_id(1),
        commands([])
    ),
    CtxIn = ctx(State),
    detect_collisions(
        ctx_old(CtxIn),
        ctx_new(CtxOut)
    ),
    ctx_objs(CtxOut, NewObjects),
    NewObjects = Objects  % No objects removed
)).

:- discontiguous(test/2).

