:- module(wait_until_fwd_test, []).

:- use_module('../../..'/build/prologinator).
:- use_module('../../..'/prolog/util/test_util).

:- use_module(library(assoc), [
    empty_assoc/1,
    put_assoc/4
]).

% ==========================================================
% Tests: wait_until Action (Forward)
% ==========================================================

test("wait_until: completes when path exists", (
    % --------------------------------------------------
    % Arrange
    % --------------------------------------------------
    ObjIn = object(
        id(1),
        type(static),
        actions([wait_until(collision_id), noop]),
        collisions([])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, 
              [attr(x, 0), attr(y, 0), 
               attr(collision_id, 5)], 
              Attrs),
    ctx_with_attrs(Attrs, Ctx),
    % --------------------------------------------------
    % Act
    % --------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(wait_until(collision_id)),
        obj_old(ObjIn),
        result(Status, ObjOut)
    ),
    % --------------------------------------------------
    % Assert
    % --------------------------------------------------
    expect(Status = completed, 'Status != completed'),
    expect(obj_acns(ObjOut, [noop]),
        'Actions != [noop]'),
    expect(ctx_cmds([], CtxNew), 'Commands != []')
)).

test("wait_until: yields when path does not exist", (
    % --------------------------------------------------
    % Arrange
    % --------------------------------------------------
    ObjIn = object(
        id(1),
        type(static),
        actions([wait_until(collision_id), noop]),
        collisions([])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, 
              [attr(x, 0), attr(y, 0)], 
              Attrs),
    ctx_with_attrs(Attrs, Ctx),
    % --------------------------------------------------
    % Act
    % --------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(_CtxNew),
        action(wait_until(collision_id)),
        obj_old(ObjIn),
        result(Status, ObjOut)
    ),
    % --------------------------------------------------
    % Assert
    % --------------------------------------------------
    expect(Status = yielded, 'Status != yielded'),
    expect(obj_acns(ObjOut, 
                    [wait_until(collision_id), noop]),
        'Action not preserved')
)).

test("wait_until: works with nested paths", (
    % --------------------------------------------------
    % Arrange
    % --------------------------------------------------
    ObjIn = object(
        id(1),
        type(static),
        actions([wait_until(collision_id/collisionType), 
                 noop]),
        collisions([])
    ),
    empty_attr_store(EmptyAttrs0),
    % Object 1 has collision_id pointing to object 5
    put_assoc(1, EmptyAttrs0, 
              [attr(x, 0), attr(y, 0), 
               attr(collision_id, 5)], 
              Attrs1),
    % Object 5 has collisionType
    put_assoc(5, Attrs1, 
              [attr(x, 10), attr(y, 10), 
               attr(collisionType, enemy)], 
              Attrs),
    ctx_with_attrs(Attrs, Ctx),
    % --------------------------------------------------
    % Act
    % --------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(_CtxNew),
        action(wait_until(collision_id/collisionType)),
        obj_old(ObjIn),
        result(Status, ObjOut)
    ),
    % --------------------------------------------------
    % Assert
    % --------------------------------------------------
    expect(Status = completed, 'Status != completed'),
    expect(obj_acns(ObjOut, [noop]),
        'Actions != [noop]')
)).

test("wait_until: yields when nested path does not exist", (
    % --------------------------------------------------
    % Arrange
    % --------------------------------------------------
    ObjIn = object(
        id(1),
        type(static),
        actions([wait_until(collision_id/collisionType), 
                 noop]),
        collisions([])
    ),
    empty_attr_store(EmptyAttrs0),
    % Object 1 has collision_id pointing to object 5
    put_assoc(1, EmptyAttrs0, 
              [attr(x, 0), attr(y, 0), 
               attr(collision_id, 5)], 
              Attrs1),
    % Object 5 exists but has no collisionType
    put_assoc(5, Attrs1, 
              [attr(x, 10), attr(y, 10)], 
              Attrs),
    ctx_with_attrs(Attrs, Ctx),
    % --------------------------------------------------
    % Act
    % --------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(_CtxNew),
        action(wait_until(collision_id/collisionType)),
        obj_old(ObjIn),
        result(Status, ObjOut)
    ),
    % --------------------------------------------------
    % Assert
    % --------------------------------------------------
    expect(Status = yielded, 'Status != yielded'),
    expect(obj_acns(ObjOut, 
                    [wait_until(collision_id/
                                collisionType), 
                     noop]),
        'Action not preserved')
)).

test("wait_until: yields when intermediate path does not \
exist", (
    % --------------------------------------------------
    % Arrange
    % --------------------------------------------------
    ObjIn = object(
        id(1),
        type(static),
        actions([wait_until(collision_id/collisionType), 
                 noop]),
        collisions([])
    ),
    empty_attr_store(EmptyAttrs0),
    % Object 1 has no collision_id at all
    put_assoc(1, EmptyAttrs0, 
              [attr(x, 0), attr(y, 0)], 
              Attrs),
    ctx_with_attrs(Attrs, Ctx),
    % --------------------------------------------------
    % Act
    % --------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(_CtxNew),
        action(wait_until(collision_id/collisionType)),
        obj_old(ObjIn),
        result(Status, ObjOut)
    ),
    % --------------------------------------------------
    % Assert
    % --------------------------------------------------
    expect(Status = yielded, 'Status != yielded'),
    expect(obj_acns(ObjOut, 
                    [wait_until(collision_id/
                                collisionType), 
                     noop]),
        'Action not preserved')
)).

