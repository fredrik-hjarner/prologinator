:- module(parallel_all_fwd_test, []).

:- use_module('../../../build/prologinator').
:- use_module('../../../prolog/util/test_util').

:- use_module(library(lists), [member/2]).
:- use_module(library(assoc), [
    empty_assoc/1,
    put_assoc/4
]).
:- use_module(library(format)).

% ==========================================================
% Helper predicates
% ==========================================================

% Helper to tick object N times recursively
% tick_n(+N, +ObjIn, -ObjOut, +CtxIn, -CtxOut)
tick_n(0, Obj, Obj, Ctx, Ctx).
tick_n(N, ObjIn, ObjOut, CtxIn, CtxOut) :-
    N > 0,
    tick_object(
        obj_old(ObjIn),
        result(_, Obj1),
        CtxIn,
        Ctx1
    ),
    N1 is N - 1,
    tick_n(N1, Obj1, ObjOut, Ctx1, CtxOut).

% ==========================================================
% Forward Tests for parallel_all
% ==========================================================

% Test: parallel_all executes all children and they all
% complete.
% Simple case: set multiple attributes in parallel
test("parallel_all: all children complete - sets multiple \
attributes.", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ObjIn = object(
        id(1),
        type(static),
        actions([
            parallel_all([
                set_attr(x, 10),
                set_attr(y, 20),
                set_attr(z, 30)
            ])
        ]),
        collisions([])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(
        1,
        EmptyAttrs0,
        [attr(x, 0), attr(y, 0), attr(z, 0)],
        EmptyAttrs
    ),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(parallel_all([
            set_attr(x, 10),
            set_attr(y, 20),
            set_attr(z, 30)
        ])),
        obj_old(ObjIn),
        result(Status, ObjOut),
        Ctx,
        CtxNew
    ),
    ctx_attr_val(1/x, NewX, CtxNew),
    ctx_attr_val(1/y, NewY, CtxNew),
    ctx_attr_val(1/z, NewZ, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    Status = completed, % because no action is yielding.
    NewX = 10,
    NewY = 20,
    NewZ = 30,
    obj_acns(ObjOut, [])
)).

% Test: parallel_all yields when a child yields
test("parallel_all: yields when child yields", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ObjIn = object(
        id(1),
        type(static),
        actions([
            parallel_all([
                set_attr(x, 10),
                wait(2),  % Will yield (becomes wait(1))
                set_attr(y, 20)
            ])
        ]),
        collisions([])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(
        1, EmptyAttrs0, [attr(x, 0), attr(y, 0)], EmptyAttrs
    ),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(parallel_all([
            set_attr(x, 10),
            wait(2),  % Yields after 1 frame (becomes wait(1))
            set_attr(y, 20)
        ])),
        obj_old(ObjIn),
        result(Status, ObjOut),
        Ctx,
        CtxNew
    ),
    expect(ctx_attr_val(1/x, NewX, CtxNew)),
    expect(ctx_attr_val(1/y, NewY, CtxNew)),
    expect(obj_acns(ObjOut, Actions)),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    Status = yielded,
    % Both top level actions should still have run since
    % well... that's the parallelism.
    NewX = 10,  % First child completed
    % When wait(2) yields, it becomes wait(1) and execution
    % continues
    % with the next child. So set_attr(y, 20) should also
    % complete.
    NewY = 20,  % Third child completed (execution continue
    % after yield)
    % parallel_all should still be in queue with remaining
    % children
    % Structure: parallel_all([wait(1)]) - only the
    % yielded child remains
    Actions = [parallel_all([wait(1)])]
)).

% Test: parallel_all despawns when a child despawns
test("parallel_all: despawns when child despawns", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ObjIn = object(
        id(1),
        type(static),
        actions([
            parallel_all([
                set_attr(x, 10),
                despawn,  % This will despawn and stop all!
                set_attr(y, 20)
            ])
        ]),
        collisions([])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(
        1, EmptyAttrs0, [attr(x, 0), attr(y, 0)], EmptyAttrs
    ),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(parallel_all([
            set_attr(x, 10),
            despawn,  % Despawns immediately
            set_attr(y, 20)
        ])),
        obj_old(ObjIn),
        result(Status, _ObjOut),
        Ctx,
        CtxNew
    ),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    Status = despawned,
    % Attributes should be removed from context when
    % despawned.
    % (despawn removes attributes from the store)
    \+ ctx_attr_val(1/x, _, CtxNew)
)).

test("parallel_all: should continue until all complete", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ParAcn = parallel_all([wait(1), wait(2), wait(3)]),
    Actions = list([
        ParAcn,
        despawn
    ]),
    ObjIn = object(
        id(1),
        type(static),
        actions([Actions]),
        collisions([])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(
        1, EmptyAttrs0, [attr(a, 0), attr(b, 0)], EmptyAttrs
        ),
        ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act - Frame 0 -> 1
    % ------------------------------------------------------
    tick_object(
        obj_old(ObjIn),
        result(Status, ObjOut),
        Ctx,
        CtxNew
    ),
    % ------------------------------------------------------
    % Assert - frame 1
    % ------------------------------------------------------
    expect(Status = yielded),
    expect(obj_acns(
        ObjOut, [
            list([
                parallel_all([wait(1), wait(2)]),
                despawn
            ])
        ]
    )),
    % ------------------------------------------------------
    % Act - Frame 1 -> 2
    % ------------------------------------------------------
    tick_object(
        obj_old(ObjOut),
        result(Status2, Obj2),
        CtxNew,
        Ctx2
    ),
    % ------------------------------------------------------
    % Assert - frame 2
    % ------------------------------------------------------
    expect(Status2 = yielded),
    expect(obj_acns(
        Obj2, [
            list([
                parallel_all([wait(1)]),
                despawn
            ])
        ]
    )),
    % ------------------------------------------------------
    % Act - Frame 2 -> 3
    % ------------------------------------------------------
    tick_object(
        obj_old(Obj2),
        result(Status3, Obj3),
        Ctx2,
        Ctx3
    ),
    % ------------------------------------------------------
    % Assert - frame 3
    % ------------------------------------------------------
    expect(Status3 = despawned),
    expect(obj_acns(
        Obj3, []
    ))
)).