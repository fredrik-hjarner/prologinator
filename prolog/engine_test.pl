:- module(engine_test, []).

#include "./build/prologinator.pl"

% ==========================================================
% Tests for tick_object/5
% ==========================================================

test("tick_object: empty action list returns unchanged \
object", (
    ObjIn = object(id(1)),
    ActionsIn = [],
    empty_ctx(Ctx0),
    ctx_set_attr_val(1/type, static, Ctx0, Ctx),
    obj_id(ObjIn, ID),
    % Set up actionstore
    ctx_actionstore(ActionStoreIn, Ctx, Ctx),
    put_assoc(ID, ActionStoreIn, [ActionsIn],
              ActionStoreWithActions),
    ctx_set_actionstore(ActionStoreWithActions, Ctx,
                        CtxWithActions),
    tick_object(
        actions_old(ActionsIn),
        obj(ObjIn),
        result(Status, actions_new(ActionsOut)),
        CtxWithActions,
        CtxNew
    ),
    ctx_spawnCmds([], CtxNew, CtxNew),
    Status = completed,
    ActionsOut = []
)).

test("tick_object: yielding action (wait) stops \
after one execution", (
    ObjIn = object(id(1)),
    ActionsIn = [wait(5)],
    empty_ctx(Ctx0),
    ctx_set_attr_val(1/type, static, Ctx0, Ctx),
    obj_id(ObjIn, ID),
    % Set up actionstore
    ctx_actionstore(ActionStoreIn, Ctx, Ctx),
    put_assoc(ID, ActionStoreIn, [ActionsIn],
              ActionStoreWithActions),
    ctx_set_actionstore(ActionStoreWithActions, Ctx,
                        CtxWithActions),
    tick_object(
        actions_old(ActionsIn),
        obj(ObjIn),
        result(Status, actions_new(ActionsOut)),
        CtxWithActions,
        CtxNew
    ),
    ctx_spawnCmds([], CtxNew, CtxNew),
    Status = yielded,
    ActionsOut = [wait(4)]
)).

test("tick_object: wait(0) is removed and execution \
continues until empty", (
    ObjIn = object(id(1)),
    ActionsIn = [wait(0)],
    empty_ctx(Ctx0),
    ctx_set_attr_val(1/type, static, Ctx0, Ctx),
    obj_id(ObjIn, ID),
    % Set up actionstore
    ctx_actionstore(ActionStoreIn, Ctx, Ctx),
    put_assoc(ID, ActionStoreIn, [ActionsIn],
              ActionStoreWithActions),
    ctx_set_actionstore(ActionStoreWithActions, Ctx,
                        CtxWithActions),
    tick_object(
        actions_old(ActionsIn),
        obj(ObjIn),
        result(Status, actions_new(ActionsOut)),
        CtxWithActions,
        CtxNew
    ),
    ctx_spawnCmds([], CtxNew, CtxNew),
    Status = completed,
    ActionsOut = []
)).

% ==========================================================
% Tests for tick/2
% ==========================================================

test("tick: increments frame and processes empty game \
state", (
    ctx_with_objs([object(id(0))], CtxIn0),
    % Set up actionstore with empty actions
    ctx_actionstore(ActionStoreIn, CtxIn0, CtxIn0),
    put_assoc(0, ActionStoreIn, [[]],
              ActionStoreWithActions),
    ctx_set_actionstore(ActionStoreWithActions, CtxIn0,
                        CtxIn),
    tick(CtxIn, CtxOut),
    ctx_frame(1, CtxOut, CtxOut),
    ctx_objs([object(id(0))], CtxOut, CtxOut)
)).

test("tick: processes object with yielding action \
(wait)", (
    empty_ctx(CtxIn0),
    ctx_set_objs([object(id(0))], CtxIn0, CtxIn1),
    % Set up actionstore
    ctx_actionstore(ActionStoreIn, CtxIn1, CtxIn1),
    put_assoc(0, ActionStoreIn, [[wait(3)]],
              ActionStoreWithActions),
    ctx_set_actionstore(ActionStoreWithActions, CtxIn1,
                        CtxIn),
    tick(CtxIn, CtxOut),
    ctx_frame(1, CtxOut, CtxOut),
    ctx_objs([object(id(0))], CtxOut, CtxOut),
    % Check actionstore
    ctx_actionstore(ActionStoreOut, CtxOut, CtxOut),
    gen_assoc(0, ActionStoreOut, [[wait(2)]])
)).

test("tick: processes spawn request and creates new \
object", (
    empty_ctx(CtxIn0),
    ctx_set_objs([object(id(0))], CtxIn0, CtxIn1),
    % Set up actionstore
    ctx_actionstore(ActionStoreIn, CtxIn1, CtxIn1),
    put_assoc(0, ActionStoreIn,
              [[spawn([set_attr(type, enemy),
                       set_attr(x, 5),
                       set_attr(y, 5)])]],
              ActionStoreWithActions),
    ctx_set_actionstore(ActionStoreWithActions, CtxIn1,
                        CtxIn),
    tick(CtxIn, CtxOut),
    ctx_frame(1, CtxOut, CtxOut),
    ctx_nextid(2, CtxOut, CtxOut),
    ctx_objs(FinalObjs, CtxOut, CtxOut),
    member(object(id(0)), FinalObjs),
    member(object(id(_NewID)), FinalObjs),
    ctx_attr_val(_NewID/type, enemy, CtxOut, CtxOut)
)).

% ==========================================================
% Collision Test: Simple enemy-projectile collision
% ==========================================================

% TODO: fix perf issue.
test("collision: simple enemy-projectile collision", (
    % Start with one enemy and one projectile that will
    % collide
    % Both move towards the same target position so they
    % collide
    empty_assoc(EmptyAttrs0),
    put_assoc(0, EmptyAttrs0,
              [attr(type, enemy), attr(x, 5), attr(y, 10)],
              Attrs1),
    put_assoc(1, Attrs1,
              [attr(type, proj), attr(x, 5), attr(y, 10)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx0),
    ctx_set_objs([
        object(id(0)),
        object(id(1))
    ], Ctx0, Ctx1),
    % Set up actionstore
    ctx_actionstore(ActionStoreIn, Ctx1, Ctx1),
    put_assoc(0, ActionStoreIn, [[move_to(10, 10, 1)]],
              ActionStore1),
    put_assoc(1, ActionStore1, [[move_to(10, 10, 1)]],
              ActionStoreWithActions),
    ctx_set_actionstore(ActionStoreWithActions, Ctx1,
                        Ctx1WithActions),
    ctx_set_nextid(2, Ctx1WithActions, InitialContext),
    % Run 1 frame - collision should happen when both reach
    % (10, 10)
    tick(InitialContext, Context1),
    ctx_frame(1, Context1, Context1),
    ctx_objs(Objs1, Context1, Context1),
    % After collision, both objects remain but have
    % collision_id attributes
    length(Objs1, ObjCount),
    ObjCount = 2,
    % Check collision_id attributes are set
    ctx_attr_val(0/collision_id, 1, Context1, Context1),
    ctx_attr_val(1/collision_id, 0, Context1, Context1)
)).

% ==========================================================
% Performance Test: Run game to frame 32 (reproduces freeze)
% ==========================================================

% TODO: fix perf issue.
test("performance: run game to frame 32 to reproduce \
freeze (first collision)", (
    % Same initial state as game.pl
    empty_assoc(EmptyAttrs0),
    put_assoc(0, EmptyAttrs0,
              [attr(type, tower), attr(x, 5), attr(y, 19)],
              Attrs1),
    put_assoc(1, Attrs1,
              [attr(type, tower), attr(x, 10), attr(y, 19)],
              Attrs2),
    put_assoc(2, Attrs2,
              [attr(type, tower), attr(x, 15), attr(y, 19)],
              Attrs3),
    put_assoc(3, Attrs3, [attr(type, static)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx0),
    ctx_set_objs([
        object(id(0)),
        object(id(1)),
        object(id(2)),
        object(id(3))
    ], Ctx0, Ctx1),
    % Set up actionstore
    ctx_actionstore(ActionStoreIn, Ctx1, Ctx1),
    put_assoc(0, ActionStoreIn,
              [[loop([wait(3),
                      spawn([set_attr(type, proj),
                             set_attr(x, 5),
                             set_attr(y, 19),
                             move_to(5, 0, 20)])])]],
              ActionStore1),
    put_assoc(1, ActionStore1,
              [[loop([wait(3),
                      spawn([set_attr(type, proj),
                             set_attr(x, 10),
                             set_attr(y, 19),
                             move_to(10, 0, 20)])])]],
              ActionStore2),
    put_assoc(2, ActionStore2,
              [[loop([wait(3),
                      spawn([set_attr(type, proj),
                             set_attr(x, 15),
                             set_attr(y, 19),
                             move_to(15, 0, 20)])])]],
              ActionStore3),
    put_assoc(3, ActionStore3,
              [[loop([wait(5),
                      spawn([set_attr(type, enemy),
                             set_attr(x, 0),
                             set_attr(y, 10),
                             move_to(19, 10, 30)])])]],
              ActionStoreWithActions),
    ctx_set_actionstore(ActionStoreWithActions, Ctx1,
                        Ctx1WithActions),
    ctx_set_nextid(4, Ctx1WithActions, InitialContext),
    % Run tick 32 times (first collision should happen
    % around here)
    run_ticks(InitialContext, 32, FinalContext),
    ctx_frame(32, FinalContext, FinalContext)
)).

% ==========================================================
% Test Helper: run tick N times
% ==========================================================

run_ticks(Ctx, 0, Ctx).
run_ticks(CtxIn, N, CtxOut) :-
    N > 0,
    tick(CtxIn, CtxNext),
    N1 is N - 1,
    run_ticks(CtxNext, N1, CtxOut).
