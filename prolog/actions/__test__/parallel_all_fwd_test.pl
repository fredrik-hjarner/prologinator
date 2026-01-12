:- module(parallel_all_fwd_test, []).

#include "./build/prologinator.pl"
#include "./prolog/util/test_util.pl"

% ==========================================================
% Helper predicates
% ==========================================================

% Helper to tick object N times recursively
% tick_n(+N, +ObjIn, -ObjOut, +CtxIn, -CtxOut)
% NOTE: This helper is deprecated - it uses obj_acns which
% returns empty list.
% Tests should use tick_action_streams directly instead.
tick_n(0, Obj, Obj, Ctx, Ctx).
tick_n(N, ObjIn, ObjOut, CtxIn, CtxOut) :-
    N > 0,
    obj_id(ObjIn, ID),
    % Get actions from actionstore
    ctx_actionstore(ActionStoreIn, CtxIn, CtxIn),
    ( gen_assoc(ID, ActionStoreIn, [ActionsIn]) ->
        phrase(
            tick_object(
                actions_old(ActionsIn),
                obj(object(id(ID))),
                result(_, actions_new(ActionsOut))
            ),
            CtxIn,
            Ctx1
        ),
        % Update actionstore
        ctx_actionstore(ActionStoreTemp, Ctx1, Ctx1),
        put_assoc(ID, ActionStoreTemp, [ActionsOut],
                  ActionStoreOut),
        ctx_set_actionstore(ActionStoreOut, Ctx1,
                            Ctx1WithActions),
        N1 is N - 1,
        tick_n(N1, ObjIn, ObjOut, Ctx1WithActions, CtxOut)
    ;
        % No actions in actionstore, just pass through
        N1 is N - 1,
        tick_n(N1, ObjIn, ObjOut, CtxIn, CtxOut)
    ).

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
    ActionsIn = [
        parallel_all([
            set_attr(x, 10),
            set_attr(y, 20),
            set_attr(z, 30)
        ])
    ],
    empty_attr_store(EmptyAttrs0),
    put_assoc(
        1,
        EmptyAttrs0,
        [attr(type, static), attr(x, 0), attr(y, 0),
         attr(z, 0)],
        EmptyAttrs
    ),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        actions_old(ActionsIn),
        obj(object(id(1))),
        result(Status, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    ctx_attr_val(1/x, NewX, CtxNew, CtxNew),
    ctx_attr_val(1/y, NewY, CtxNew, CtxNew),
    ctx_attr_val(1/z, NewZ, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    Status = completed, % because no action is yielding.
    NewX = 10,
    NewY = 20,
    NewZ = 30,
    ActionsOut = []
)).

% Test: parallel_all yields when a child yields
test("parallel_all: yields when child yields", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ActionsIn = [
        parallel_all([
            set_attr(x, 10),
            wait(2),  % Will yield (becomes wait(1))
            set_attr(y, 20)
        ])
    ],
    empty_attr_store(EmptyAttrs0),
    put_assoc(
        1, EmptyAttrs0,
        [attr(type, static), attr(x, 0), attr(y, 0)],
        EmptyAttrs
    ),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        actions_old(ActionsIn),
        obj(object(id(1))),
        result(Status, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    expect(ctx_attr_val(1/x, NewX, CtxNew, CtxNew)),
    expect(ctx_attr_val(1/y, NewY, CtxNew, CtxNew)),
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
    ActionsOut = [parallel_all([wait(1)])]
)).

% Test: parallel_all despawns when a child despawns
test("parallel_all: despawns when child despawns", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ActionsIn = [
        parallel_all([
            set_attr(x, 10),
            despawn,  % This will despawn and stop all!
            set_attr(y, 20)
        ])
    ],
    empty_attr_store(EmptyAttrs0),
    put_assoc(
        1, EmptyAttrs0,
        [attr(type, static), attr(x, 0), attr(y, 0)],
        EmptyAttrs
    ),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        actions_old(ActionsIn),
        obj(object(id(1))),
        result(Status, actions_new(_)),
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
    \+ ctx_attr_val(1/x, _, CtxNew, CtxNew)
)).

test("parallel_all: should continue until all complete", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ParAcn = parallel_all([wait(1), wait(2), wait(3)]),
    ActionsIn = [list([
        ParAcn,
        despawn
    ])],
    empty_attr_store(EmptyAttrs0),
    put_assoc(
        1, EmptyAttrs0,
        [attr(type, static), attr(a, 0), attr(b, 0)],
        EmptyAttrs
        ),
        ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act - Frame 0 -> 1
    % ------------------------------------------------------
    tick_object(
        actions_old(ActionsIn),
        obj(object(id(1))),
        result(Status, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    % ------------------------------------------------------
    % Assert - frame 1
    % ------------------------------------------------------
    expect(Status = yielded),
    expect(ActionsOut = [
        list([
            parallel_all([wait(1), wait(2)]),
            despawn
        ])
    ]),
    % ------------------------------------------------------
    % Act - Frame 1 -> 2
    % ------------------------------------------------------
    tick_object(
        actions_old(ActionsOut),
        obj(object(id(1))),
        result(Status2, actions_new(ActionsOut2)),
        CtxNew,
        Ctx2
    ),
    % ------------------------------------------------------
    % Assert - frame 2
    % ------------------------------------------------------
    expect(Status2 = yielded),
    expect(ActionsOut2 = [
        list([
            parallel_all([wait(1)]),
            despawn
        ])
    ]),
    % ------------------------------------------------------
    % Act - Frame 2 -> 3
    % ------------------------------------------------------
    tick_object(
        actions_old(ActionsOut2),
        obj(object(id(1))),
        result(Status3, actions_new(ActionsOut3)),
        Ctx2,
        Ctx3
    ),
    % ------------------------------------------------------
    % Assert - frame 3
    % ------------------------------------------------------
    expect(Status3 = despawned),
    expect(ActionsOut3 = [])
)).