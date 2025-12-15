:- module(engine_test, []).

:- use_module('../build/prologinator').

:- use_module(library(lists), [member/2, length/2]).
:- use_module(library(assoc), [
    empty_assoc/1,
    put_assoc/4
]).

% ==========================================================
% Tests for tick_object/5
% ==========================================================

test("tick_object: empty action list returns unchanged \
object", (
    ObjIn = object(
        id(1),
        type(static),
        actions([]),
        collisions([])
    ),
    empty_ctx(Ctx),
    tick_object(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        obj_old(ObjIn),
        result(Status, ObjOut)
    ),
    ctx_cmds([], CtxNew),
    Status = completed,
    ObjOut = object(
        id(1),
        type(static),
        actions([]),
        collisions([])
    )
)).

test("tick_object: yielding action (wait) stops \
after one execution", (
    ObjIn = object(
        id(1),
        type(static),
        actions([wait(5)]),
        collisions([])
    ),
    empty_ctx(Ctx),
    tick_object(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        obj_old(ObjIn),
        result(Status, ObjOut)
    ),
    ctx_cmds([], CtxNew),
    Status = yielded,
    ObjOut = object(
        id(1),
        type(static),
        actions([wait(4)]),
        collisions([])
    )
)).

test("tick_object: wait(0) is removed and execution \
continues until empty", (
    ObjIn = object(
        id(1),
        type(static),
        actions([wait(0)]),
        collisions([])
    ),
    empty_ctx(Ctx),
    tick_object(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        obj_old(ObjIn),
        result(Status, ObjOut)
    ),
    ctx_cmds([], CtxNew),
    Status = completed,
    ObjOut = object(
        id(1),
        type(static),
        actions([]),
        collisions([])
    )
)).

% ==========================================================
% Tests for tick/2
% ==========================================================

test("tick: increments frame and processes empty game \
state", (
    empty_ctx(CtxIn0),
    ctx_set_objs([object(
        id(0),
        type(static),
        actions([]),
        collisions([])
    )], CtxIn0, CtxIn),
    tick(ctx_in(CtxIn), ctx_out(CtxOut)),
    ctx_frame(1, CtxOut),
    ctx_objs([object(
        id(0),
        type(static),
        actions([]),
        collisions([])
    )], CtxOut)
)).

test("tick: processes object with yielding action \
(wait)", (
    empty_ctx(CtxIn0),
    ctx_set_objs([object(
        id(0),
        type(static),
        actions([wait(3)]),
        collisions([])
    )], CtxIn0, CtxIn),
    tick(ctx_in(CtxIn), ctx_out(CtxOut)),
    ctx_frame(1, CtxOut),
    ctx_objs([object(
        id(0),
        type(static),
        actions([wait(2)]),
        collisions([])
    )], CtxOut)
)).

test("tick: processes spawn request and creates new \
object", (
    empty_ctx(CtxIn0),
    ctx_set_objs([object(
        id(0),
        type(static),
        actions([
            spawn(enemy, 5, 5, [])
        ]),
        collisions([])
    )], CtxIn0, CtxIn),
    tick(ctx_in(CtxIn), ctx_out(CtxOut)),
    ctx_frame(1, CtxOut),
    ctx_nextid(2, CtxOut),
    ctx_objs(FinalObjs, CtxOut),
    member(
        object(
            id(0),
            type(static),
            actions([]),
            collisions([])
        ),
        FinalObjs
    ),
    member(
        object(
            id(_NewID),
            type(enemy),
            actions([]),
            collisions([])
        ),
        FinalObjs
    )
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
    put_assoc(0, EmptyAttrs0, [attr(x, 5), attr(y, 10)],
              Attrs1),
    put_assoc(1, Attrs1, [attr(x, 5), attr(y, 10)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx0),
    ctx_set_objs([
        object(
            id(0), type(enemy),
            actions([
                % Moving right, arrives at (10, 10) in 1
                % frame
                move_to(10, 10, 1)
            ]), collisions([])
        ),
        object(
            id(1), type(proj),
            actions([
                % Moving to same target, arrives at
                % (10, 10) in 1 frame
                move_to(10, 10, 1)
            ]), collisions([])
        )
    ], Ctx0, Ctx1),
    ctx_set_nextid(2, Ctx1, InitialContext),
    % Run 1 frame - collision should happen when both reach
    % (10, 10)
    tick(ctx_in(InitialContext), ctx_out(Context1)),
    ctx_frame(1, Context1),
    ctx_objs(Objs1, Context1),
    % After collision, both objects remain but have
    % collision_id attributes
    length(Objs1, ObjCount),
    ObjCount = 2,
    % Check collision_id attributes are set
    ctx_attr_val(Context1, 0/collision_id, 1),
    ctx_attr_val(Context1, 1/collision_id, 0)
)).

% ==========================================================
% Performance Test: Run game to frame 32 (reproduces freeze)
% ==========================================================

% TODO: fix perf issue.
test("performance: run game to frame 32 to reproduce \
freeze (first collision)", (
    % Same initial state as game.pl
    empty_assoc(EmptyAttrs0),
    put_assoc(0, EmptyAttrs0, [attr(x, 5), attr(y, 19)],
              Attrs1),
    put_assoc(1, Attrs1, [attr(x, 10), attr(y, 19)],
              Attrs2),
    put_assoc(2, Attrs2, [attr(x, 15), attr(y, 19)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx0),
    ctx_set_objs([
        object(
            id(0), type(tower),
            actions([
                loop([
                    wait(3),
                    spawn(proj, 5, 19, [
                        move_to(5, 0, 20)
                    ])
                ])
            ]), collisions([])
        ),
        object(
            id(1), type(tower),
            actions([
                loop([
                    wait(3),
                    spawn(proj, 10, 19, [
                        move_to(10, 0, 20)
                    ])
                ])
            ]), collisions([])
        ),
        object(
            id(2), type(tower),
            actions([
                loop([
                    wait(3),
                    spawn(proj, 15, 19, [
                        move_to(15, 0, 20)
                    ])
                ])
            ]), collisions([])
        ),
        object(
            id(3), type(static),
            actions([
                loop([
                    wait(5),
                    spawn(enemy, 0, 10, [
                        move_to(19, 10, 30)
                    ])
                ])
            ]), collisions([])
        )
    ], Ctx0, Ctx1),
    ctx_set_nextid(4, Ctx1, InitialContext),
    % Run tick 32 times (first collision should happen
    % around here)
    run_ticks(
        ctx_in(InitialContext), 32, ctx_out(FinalContext)
    ),
    ctx_frame(32, FinalContext)
)).

% ==========================================================
% Test Helper: run tick N times
% ==========================================================

run_ticks(ctx_in(Ctx), 0, ctx_out(Ctx)).
run_ticks(ctx_in(CtxIn), N, ctx_out(CtxOut)) :-
    N > 0,
    tick(ctx_in(CtxIn), ctx_out(CtxNext)),
    N1 is N - 1,
    run_ticks(ctx_in(CtxNext), N1, ctx_out(CtxOut)).
