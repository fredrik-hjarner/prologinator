:- module(engine_test, []).
% use_module/1 imports ALL the exported stuff.
:- use_module('./engine').
:- use_module('./types/accessors').
:- use_module(library(lists), [member/2, length/2]).

% ==========================================================
% Tests for yields/2
% ==========================================================

test("yields: wait with positive number should \
yield", (
    A = wait(5),
    yields(A, true)
)).

test("yields: wait with zero should not yield", (
    A = wait(0),
    yields(A, false)
)).

test("yields: move_to with positive frames should yield", (
    A = move_to(10, 20, 3),
    yields(A, true)
)).

test("yields: can generate yielding actions \
bidirectionally", (
    yields(A, true),
    ( A = wait(_)
    ; A = move_to(_, _, _)
    ; A = parallel_running(_) )
)).

% ==========================================================
% Tests for tick_object/5
% ==========================================================

test("tick_object: empty action list returns unchanged \
object", (
    ObjIn = object(
        id(1),
        type(static),
        attrs([pos(0, 0)]),
        actions([]),
        collisions([])
    ),
    Ctx = ctx(state(
        frame(0),
        objects([]),
        status(playing),
        next_id(1),
        commands([]),
        rev_hints([])
    )),
    tick_object(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        obj_old(ObjIn),
        obj_new(ObjOut)
    ),
    ctx_cmds(CtxNew, []),
    ctx_revhints(CtxNew, []),
    ObjOut = [object(
        id(1),
        type(static),
        attrs([pos(0, 0)]),
        actions([]),
        collisions([])
    )]
)).

test("tick_object: yielding action (wait) stops \
after one execution", (
    ObjIn = object(
        id(1),
        type(static),
        attrs([pos(0, 0)]),
        actions([wait(5)]),
        collisions([])
    ),
    Ctx = ctx(state(
        frame(0),
        objects([]),
        status(playing),
        next_id(1),
        commands([]),
        rev_hints([])
    )),
    tick_object(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        obj_old(ObjIn),
        obj_new(ObjOut)
    ),
    ctx_cmds(CtxNew, []),
    ctx_revhints(CtxNew, []),
    ObjOut = [object(
        id(1),
        type(static),
        attrs([pos(0, 0)]),
        actions([wait(4)]),
        collisions([])
    )]
)).

test("tick_object: wait(0) is removed and execution \
continues until empty", (
    ObjIn = object(
        id(1),
        type(static),
        attrs([pos(0, 0)]),
        actions([wait(0)]),
        collisions([])
    ),
    Ctx = ctx(state(
        frame(0),
        objects([]),
        status(playing),
        next_id(1),
        commands([]),
        rev_hints([])
    )),
    tick_object(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        obj_old(ObjIn),
        obj_new(ObjOut)
    ),
    ctx_cmds(CtxNew, []),
    ctx_revhints(CtxNew, []),
    ObjOut = [object(
        id(1),
        type(static),
        attrs([pos(0, 0)]),
        actions([]),
        collisions([])
    )]
)).

% ==========================================================
% Tests for tick/2
% ==========================================================

test("tick: increments frame and processes empty game \
state", (
    CtxIn = ctx(state(
        frame(0),
        objects([object(
            id(0),
            type(static),
            attrs([pos(0, 0)]),
            actions([]),
            collisions([])
        )]),
        status(playing),
        next_id(1),
        commands([]),
        rev_hints([])
    )),
    tick(ctx_in(CtxIn), ctx_out(CtxOut)),
    CtxOut = ctx(state(
        frame(1),
        objects([object(
            id(0),
            type(static),
            attrs([pos(0, 0)]),
            actions([]),
            collisions([])
        )]),
        status(playing),
        next_id(1),
        commands([]),
        rev_hints([])
    ))
)).

test("tick: processes object with yielding action \
(wait)", (
    CtxIn = ctx(state(
        frame(0),
        objects([object(
            id(0),
            type(static),
            attrs([pos(0, 0)]),
            actions([wait(3)]),
            collisions([])
        )]),
        status(playing),
        next_id(1),
        commands([]),
        rev_hints([])
    )),
    tick(ctx_in(CtxIn), ctx_out(CtxOut)),
    CtxOut = ctx(state(
        frame(1),
        objects([object(
            id(0),
            type(static),
            attrs([pos(0, 0)]),
            actions([wait(2)]),
            collisions([])
        )]),
        status(playing),
        next_id(1),
        commands([]),
        rev_hints([])
    ))
)).

test("tick: processes spawn request and creates new \
object", (
    CtxIn = ctx(state(
        frame(0),
        objects([object(
            id(0),
            type(static),
            attrs([pos(0, 0)]),
            actions([
                spawn(enemy, pos(5, 5), [])
            ]),
            collisions([])
        )]),
        status(playing),
        next_id(1),
        commands([]),
        rev_hints([])
    )),
    tick(ctx_in(CtxIn), ctx_out(CtxOut)),
    CtxOut = ctx(state(
        frame(1),
        objects(FinalObjs),
        status(playing),
        next_id(2),
        commands([]),
        rev_hints([])
    )),
    member(
        object(
            id(0),
            type(static),
            attrs([pos(0, 0)]),
            actions([]),
            collisions([])
        ),
        FinalObjs
    ),
    member(
        object(
            id(_NewID),
            type(enemy),
            attrs([pos(5, 5)]),
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
    InitialContext = ctx(state(
        frame(0),
        objects([
            object(
                id(0), type(enemy), attrs([pos(5, 10)]),
                actions([
                    % Moving right, arrives at (10, 10) in 1
                    % frame
                    move_to(10, 10, 1)
                ]), collisions([])
            ),
            object(
                id(1), type(proj), attrs([pos(5, 10)]),
                actions([
                    % Moving to same target, arrives at
                    % (10, 10) in 1 frame
                    move_to(10, 10, 1)
                ]), collisions([])
            )
        ]),
        status(playing),
        next_id(2),
        commands([]),
        rev_hints([])
    )),
    % Run 2 frames - collision should happen at frame 1 when
    % both reach (10, 10)
    tick(ctx_in(InitialContext), ctx_out(Context1)),
    Context1 = ctx(state(
        frame(1),
        objects(Objs1),
        status(_),
        next_id(_),
        commands(_),
        rev_hints(_)
    )),
    % After collision at frame 1, both objects should be
    % removed
    length(Objs1, ObjCount),
    ObjCount = 0
)).

% ==========================================================
% Performance Test: Run game to frame 32 (reproduces freeze)
% ==========================================================

% TODO: fix perf issue.
test("performance: run game to frame 32 to reproduce \
freeze (first collision)", (
    % Same initial state as game.pl
    InitialContext = ctx(state(
        frame(0),
        objects([
            object(
                id(0), type(tower), attrs([pos(5, 19)]),
                actions([
                    loop([
                        wait(3),
                        spawn(proj, pos(5, 19), [
                            move_to(5, 0, 20)
                        ])
                    ])
                ]), collisions([])
            ),
            object(
                id(1), type(tower), attrs([pos(10, 19)]),
                actions([
                    loop([
                        wait(3),
                        spawn(proj, pos(10, 19), [
                            move_to(10, 0, 20)
                        ])
                    ])
                ]), collisions([])
            ),
            object(
                id(2), type(tower), attrs([pos(15, 19)]),
                actions([
                    loop([
                        wait(3),
                        spawn(proj, pos(15, 19), [
                            move_to(15, 0, 20)
                        ])
                    ])
                ]), collisions([])
            ),
            object(
                id(3), type(static), attrs([]),
                actions([
                    loop([
                        wait(5),
                        spawn(enemy, pos(0, 10), [
                            move_to(19, 10, 30)
                        ])
                    ])
                ]), collisions([])
            )
        ]),
        status(playing),
        next_id(4),
        commands([]),
        rev_hints([])
    )),
    % Run tick 32 times (first collision should happen
    % around here)
    run_ticks(
        ctx_in(InitialContext), 32, ctx_out(FinalContext)
    ),
    FinalContext = ctx(state(
        frame(32),
        objects(_),
        status(_),
        next_id(_),
        commands(_),
        rev_hints(_)
    ))
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

:- discontiguous(test/2).
