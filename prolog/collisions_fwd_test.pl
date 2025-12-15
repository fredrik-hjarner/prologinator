:- module(collisions_fwd_test, []).

:- use_module('../build/prologinator').
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
    empty_attr_store(EmptyAttrs0),
    put_assoc(0, EmptyAttrs0, [attr(x, 5), attr(y, 5)],
              Attrs1),
    put_assoc(1, Attrs1, [attr(x, 10), attr(y, 10)],
              Attrs),
    ctx_with_attrs(Attrs, Ctx0),
    ctx_set_objs([
        object(id(0), type(enemy), actions([]),
               collisions([])),
        object(id(1), type(proj), actions([]),
               collisions([]))
    ], Ctx0, Ctx1),
    ctx_set_nextid(2, Ctx1, CtxIn),
    detect_collisions(
        ctx_old(CtxIn),
        ctx_new(CtxOut)
    ),
    ctx_objs(NewObjects, CtxOut),
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
get collision_id attributes", (
    empty_attr_store(EmptyAttrs0),
    put_assoc(0, EmptyAttrs0, [attr(x, 10), attr(y, 10)],
              Attrs1),
    put_assoc(1, Attrs1, [attr(x, 10), attr(y, 10)],
              Attrs),
    ctx_with_attrs(Attrs, Ctx0),
    Objects = [
        object(id(0), type(enemy), actions([]),
               collisions([])),
        object(id(1), type(proj), actions([]),
               collisions([]))
    ],
    ctx_set_objs(Objects, Ctx0, Ctx1),
    ctx_set_nextid(2, Ctx1, CtxIn),
    detect_collisions(
        ctx_old(CtxIn),
        ctx_new(CtxOut)
    ),
    % Objects remain
    ctx_objs(NewObjects, CtxOut),
    NewObjects = [
        object(id(0), type(enemy), actions([]),
               collisions([])),
        object(id(1), type(proj), actions([]),
               collisions([]))
    ],
    % But collision_id attributes are set
    ctx_attr_val(CtxOut, 0/collision_id, 1),
    ctx_attr_val(CtxOut, 1/collision_id, 0)
)).

test("collisions: enemy and projectile collision sets \
collision_id only on colliding objects", (
    empty_attr_store(EmptyAttrs0),
    put_assoc(0, EmptyAttrs0, [attr(x, 10), attr(y, 10)],
              Attrs1),
    put_assoc(1, Attrs1, [attr(x, 10), attr(y, 10)],
              Attrs2),
    put_assoc(2, Attrs2, [attr(x, 5), attr(y, 5)],
              Attrs),
    ctx_with_attrs(Attrs, Ctx0),
    Objects = [
        object(id(0), type(enemy), actions([]),
               collisions([])),
        object(id(1), type(proj), actions([]),
               collisions([])),
        object(id(2), type(enemy), actions([]),
               collisions([]))
    ],
    ctx_set_objs(Objects, Ctx0, Ctx1),
    ctx_set_nextid(3, Ctx1, CtxIn),
    detect_collisions(
        ctx_old(CtxIn),
        ctx_new(CtxOut)
    ),
    % All objects remain
    ctx_objs(NewObjects, CtxOut),
    NewObjects = [
        object(id(0), type(enemy), actions([]),
               collisions([])),
        object(id(1), type(proj), actions([]),
               collisions([])),
        object(id(2), type(enemy), actions([]),
               collisions([]))
    ],
    % Collision IDs set only for colliding objects (0 and 1)
    ctx_attr_val(CtxOut, 0/collision_id, 1),
    ctx_attr_val(CtxOut, 1/collision_id, 0),
    % Object 2 has no collision_id (fails if it exists)
    \+ ctx_attr_val(CtxOut, 2/collision_id, _)
)).

% --------------------------------------------------------
% Tests: Collision detection is type-agnostic
% All positional overlaps get collision_id attributes
% --------------------------------------------------------

test("collisions: enemy and enemy at same position get \
collision_id", (
    empty_attr_store(EmptyAttrs0),
    put_assoc(0, EmptyAttrs0, [attr(x, 10), attr(y, 10)],
              Attrs1),
    put_assoc(1, Attrs1, [attr(x, 10), attr(y, 10)],
              Attrs),
    ctx_with_attrs(Attrs, Ctx0),
    Objects = [
        object(id(0), type(enemy), actions([]),
               collisions([])),
        object(id(1), type(enemy), actions([]),
               collisions([]))
    ],
    ctx_set_objs(Objects, Ctx0, Ctx1),
    ctx_set_nextid(2, Ctx1, CtxIn),
    detect_collisions(
        ctx_old(CtxIn),
        ctx_new(CtxOut)
    ),
    % Objects remain
    ctx_objs(NewObjects, CtxOut),
    NewObjects = Objects,
    % Both get collision_id attributes
    ctx_attr_val(CtxOut, 0/collision_id, 1),
    ctx_attr_val(CtxOut, 1/collision_id, 0)
)).

test("collisions: projectile and projectile at same \
position get collision_id", (
    empty_attr_store(EmptyAttrs0),
    put_assoc(0, EmptyAttrs0, [attr(x, 10), attr(y, 10)],
              Attrs1),
    put_assoc(1, Attrs1, [attr(x, 10), attr(y, 10)],
              Attrs),
    ctx_with_attrs(Attrs, Ctx0),
    Objects = [
        object(id(0), type(proj), actions([]),
               collisions([])),
        object(id(1), type(proj), actions([]),
               collisions([]))
    ],
    ctx_set_objs(Objects, Ctx0, Ctx1),
    ctx_set_nextid(2, Ctx1, CtxIn),
    detect_collisions(
        ctx_old(CtxIn),
        ctx_new(CtxOut)
    ),
    % Objects remain
    ctx_objs(NewObjects, CtxOut),
    NewObjects = Objects,
    % Both get collision_id attributes
    ctx_attr_val(CtxOut, 0/collision_id, 1),
    ctx_attr_val(CtxOut, 1/collision_id, 0)
)).

% --------------------------------------------------------
% Tests: Objects without position attributes
% --------------------------------------------------------

test("collisions: objects without x/y attributes are \
ignored", (
    empty_attr_store(EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx0),
    Objects = [
        object(id(0), type(enemy), actions([]),
               collisions([])),
        object(id(1), type(proj), actions([]),
               collisions([]))
    ],
    ctx_set_objs(Objects, Ctx0, Ctx1),
    ctx_set_nextid(2, Ctx1, CtxIn),
    detect_collisions(
        ctx_old(CtxIn),
        ctx_new(CtxOut)
    ),
    ctx_objs(NewObjects, CtxOut),
    % No objects removed (no positions)
    NewObjects = Objects
)).

test("collisions: object with only x attribute is \
ignored", (
    empty_attr_store(EmptyAttrs0),
    put_assoc(0, EmptyAttrs0, [attr(x, 10)],
              Attrs),
    ctx_with_attrs(Attrs, Ctx0),
    Objects = [
        object(id(0), type(enemy), actions([]),
               collisions([]))
    ],
    ctx_set_objs(Objects, Ctx0, Ctx1),
    ctx_set_nextid(1, Ctx1, CtxIn),
    detect_collisions(
        ctx_old(CtxIn),
        ctx_new(CtxOut)
    ),
    ctx_objs(NewObjects, CtxOut),
    NewObjects = Objects  % No objects removed
)).

