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
        actions([        ])
    ),
    empty_ctx(Ctx0),
    ctx_set_attr_val(1/type, static, Ctx0, Ctx),
    tick_object(
        obj_old(ObjIn),
        result(Status, ObjOut),
        Ctx,
        CtxNew
    ),
    ctx_cmds([], CtxNew, CtxNew),
    Status = completed,
    ObjOut = object(
        id(1),
        actions([            ])
    )
)).

test("tick_object: yielding action (wait) stops \
after one execution", (
    ObjIn = object(
        id(1),
        actions([wait(5)        ])
    ),
    empty_ctx(Ctx0),
    ctx_set_attr_val(1/type, static, Ctx0, Ctx),
    tick_object(
        obj_old(ObjIn),
        result(Status, ObjOut),
        Ctx,
        CtxNew
    ),
    ctx_cmds([], CtxNew, CtxNew),
    Status = yielded,
    ObjOut = object(
        id(1),
        actions([wait(4)            ])
    )
)).

test("tick_object: wait(0) is removed and execution \
continues until empty", (
    ObjIn = object(
        id(1),
        actions([wait(0)        ])
    ),
    empty_ctx(Ctx0),
    ctx_set_attr_val(1/type, static, Ctx0, Ctx),
    tick_object(
        obj_old(ObjIn),
        result(Status, ObjOut),
        Ctx,
        CtxNew
    ),
    ctx_cmds([], CtxNew, CtxNew),
    Status = completed,
    ObjOut = object(
        id(1),
        actions([            ])
    )
)).

% ==========================================================
% Tests for tick/2
% ==========================================================

test("tick: increments frame and processes empty game \
state", (
    ctx_with_objs([object(
        id(0),
        actions([            ])
    )], CtxIn),
    tick(CtxIn, CtxOut),
    ctx_frame(1, CtxOut, CtxOut),
    ctx_objs([object(
        id(0),
        actions([            ])
    )], CtxOut, CtxOut)
)).

test("tick: processes object with yielding action \
(wait)", (
    empty_ctx(CtxIn0),
    ctx_set_objs([object(
        id(0),
        actions([wait(3)            ])
    )], CtxIn0, CtxIn),
    tick(CtxIn, CtxOut),
    ctx_frame(1, CtxOut, CtxOut),
    ctx_objs([object(
        id(0),
        actions([wait(2)            ])
    )], CtxOut, CtxOut)
)).

test("tick: processes spawn request and creates new \
object", (
    empty_ctx(CtxIn0),
    ctx_set_objs([object(
        id(0),
        actions([
            spawn(enemy, 5, 5, [])
            ])
    )], CtxIn0, CtxIn),
    tick(CtxIn, CtxOut),
    ctx_frame(1, CtxOut, CtxOut),
    ctx_nextid(2, CtxOut, CtxOut),
    ctx_objs(FinalObjs, CtxOut, CtxOut),
    member(
        object(
            id(0),
            actions([        ])
    ),
        FinalObjs
    ),
    member(
        object(
            id(_NewID),
            actions([        ])
    ),
        FinalObjs
    ),
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
        object(
            id(0),
            actions([
                % Moving right, arrives at (10, 10) in 1
                % frame
                move_to(10, 10, 1)
            ])
        ),
        object(
            id(1),
            actions([
                % Moving to same target, arrives at
                % (10, 10) in 1 frame
                move_to(10, 10, 1)
            ])
        )
    ], Ctx0, Ctx1),
    ctx_set_nextid(2, Ctx1, InitialContext),
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
        object(
            id(0),
            actions([
                loop([
                    wait(3),
                    spawn(proj, 5, 19, [
                        move_to(5, 0, 20)
                    ])
                ])
            ])
        ),
        object(
            id(1),
            actions([
                loop([
                    wait(3),
                    spawn(proj, 10, 19, [
                        move_to(10, 0, 20)
                    ])
                ])
            ])
        ),
        object(
            id(2),
            actions([
                loop([
                    wait(3),
                    spawn(proj, 15, 19, [
                        move_to(15, 0, 20)
                    ])
                ])
            ])
        ),
        object(
            id(3),
            actions([
                loop([
                    wait(5),
                    spawn(enemy, 0, 10, [
                        move_to(19, 10, 30)
                    ])
                ])
            ])
        )
    ], Ctx0, Ctx1),
    ctx_set_nextid(4, Ctx1, InitialContext),
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
