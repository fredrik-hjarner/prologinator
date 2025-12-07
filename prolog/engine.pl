:- module(engine, [tick/2, tick_object/5, yields/1]).

:- use_module(library(clpz)).
:- use_module(library(lists), [
    append/3,
    select/3,
    member/2,
    maplist/5,
    list_to_set/2,
    length/2
]).
:- use_module('./third_party/exclude', [exclude/3]).
:- use_module('./execute_action', [execute_action/6]).
:- use_module('./types/constraints', [
    state_constraint/1,
    object_constraint/1,
    game_status_constraint/1,
    command_constraint/1,
    rev_hint_constraint/1,
    action_constraint/1,
    pos_constraint/1,
    attr_constraint/1,
    collision_constraint/1
]).
:- use_module('./types/accessors', [
    ctx_objs/2,
    ctx_objs_ctx/3,
    ctx_cmds/2,
    ctx_cmds_ctx/3,
    ctx_revhints/2,
    ctx_revhints_ctx/3,
    ctx_objs_cmds_revhints/4,
    ctx_objs_cmds_revhints_ctx/5,
    obj_acns/2
]).
:- use_module('./types/validation', [
    context_validation/1,
    state_validation/1,
    object_validation/1
]).
:- use_module('./util/util', [partition/4, flatten/2]).
:- use_module('./collisions', [detect_collisions/3]).

% test/2 clauses are intentionally separated by other code
% (tests are placed near the code they test)
:- discontiguous(test/2).

% ==========================================================
% Yielding Actions
% ==========================================================
% Bidirectional: works forward (check if action yields) and
% backward (generate yielding actions)
% Modes: yields(?Action) - can be called with Action bound
% or unbound
% :- pred yields(?Action) # "Checks if an action yields \
% control, or generates yielding actions.".

% yields tells whether an action "yields" or not.
% THis is how it works now: AFTER an action has been
% executed, we check if it yields. If it does yield then
% we DONT executa the next action. However it it does NOT
% yield then we execute the next action etc until EITHER
% the game object cease to exist or we come to a yielding
% action.
yields(wait_frames(N)) :- N #> 0.
yields(move_to(_, _, Frames)) :- Frames #> 0.
yields(parallel_running(_)).

% ==========================================================
% Execution Model: tick_object
% ==========================================================

% TODO: Add a tick_object wrapper that wraps a
% tick_object_impl. though refactorings are needed first...

% % forward execution
% :- pred tick_object(+ObjIn, -ObjOut, -AllCommands,
% -AllRevHints).

tick_object(
    ctx_old(_),
    obj_old(Obj),
    obj_new([Obj]),
    cmds_new([]),
    revhints_new([])
) :-
    obj_acns(Obj, []), % guard.
    !. % TODO: Can this cut hurt bidirectionality?

% Observe that ObjNew is either [object] or [].
% It's empty list when the object have been removed.
tick_object(
    ctx_old(CtxOld),
    obj_old(ObjOld),
    obj_new(ObjNew),
    cmds_new(CmdsNew),
    revhints_new(RevHintsNew)
) :-
    object_validation(ObjOld),
    obj_acns(ObjOld, [Act|_Rest]),
    execute_action(
        ctx_old(CtxOld),
        action(Act),
        obj_old(ObjOld),
        obj_new(ObjTempList),
        cmds_new(C1),
        revhints_new(R1)
    ),
    ( ObjTempList = [] ->
        ObjNew = [],
        CmdsNew = C1,
        RevHintsNew = R1
    ; yields(Act) ->
        ObjNew = ObjTempList,
        CmdsNew = C1,
        RevHintsNew = R1
    ;
        ObjTempList = [ObjTemp],
        tick_object(
            ctx_old(CtxOld),
            obj_old(ObjTemp),
            obj_new(ObjNew),
            cmds_new(C2),
            revhints_new(R2)
        ),
        append(C1, C2, CmdsNew),
        append(R1, R2, RevHintsNew)
    ).

% ==========================================================
% Main Tick Function
% ==========================================================
tick(ctx_in(CtxIn), ctx_out(CtxOut)) :-
    % game_state_constraint(CtxIn),
    context_validation(CtxIn),
    CtxIn = ctx(state(
        frame(F),
        _,
        status(Status),
        next_id(NextID),
        _,
        _
    )),
    
    % 1. Tick all objects (updates objects, commands, and
    % rev_hints in context)
    tick_all_objects(
        ctx_in(CtxIn),
        ctx_out(CtxAfterTick)
    ),
    
    % Extract objects and rev_hints from returned context
    ctx_objs(CtxAfterTick, TempObjs),
    ctx_revhints(CtxAfterTick, RevHints1),
    
    % 2. Detect collisions (collisions only produce
    % rev_hints, no commands)
    detect_collisions(TempObjs, NewObjs, RevHints2),
    
    % 3. Combine rev_hints
    append(RevHints1, RevHints2, AllRevHints),
    
    % Get commands from context
    ctx_cmds(CtxAfterTick, AllCommands),
    
    % 4. Partition commands
    partition(
        is_spawn_request,
        AllCommands,
        SpawnReqs,
        OtherCommands
    ),
    partition(
        is_state_change,
        OtherCommands,
        StateChanges,
        _
    ),
    
    % 5. Process spawns with ID assignment
    process_spawn_requests(
        SpawnReqs,
        NextID,
        SpawnedObjs,
        NewNextID
    ),
    
    % 6. Add spawned objects
    append(NewObjs, SpawnedObjs, FinalObjs),
    
    % 7. Apply state changes
    apply_state_changes(StateChanges, Status, NewStatus),
    
    F1 #= F + 1,
    
    % 8. Build final context with updated state
    CtxOut = ctx(state(
        frame(F1),
        objects(FinalObjs),
        status(NewStatus),
        next_id(NewNextID),
        commands([]),
        rev_hints(AllRevHints)
    )).
    % NOTE: placing this at the top caused cpu to go bananas
    %       so there are some performance problems with it..
    % game_state_constraint(StateOut).

% ==========================================================
% Tick Helpers
% ==========================================================

% Helper predicate for maplist - wraps tick_object
tick_object_with_ctx(Ctx, ObjOld, ObjNew, Cmds, RevHints) :-
    tick_object(
        ctx_old(Ctx),
        obj_old(ObjOld),
        obj_new(ObjNew),
        cmds_new(Cmds),
        revhints_new(RevHints)
    ).

% TODO: It got messy here with context... need to refactor.
tick_all_objects(ctx_in(CtxIn), ctx_out(CtxOut)) :-
    ctx_objs_cmds_revhints(
        CtxIn, Objects, OldCommands, OldRevHints
    ),
    maplist(
        tick_object_with_ctx(CtxIn),
        Objects, % "input"
        TempObjectLists, % collected output (list of lists)
        CommandLists, % collected "output"
        RevHintLists % collected "output"
    ),
    % flatten/2 flattens lists, append/3 combines
    flatten(CommandLists, NewCommands),
    append(OldCommands, NewCommands, AllCommands),
    % flatten/2 flattens list of lists, append/3 combines
    flatten(RevHintLists, NewRevHints),
    append(OldRevHints, NewRevHints, AllRevHints),
    % Flatten list of lists and filter out empty lists
    flatten(TempObjectLists, FinalObjects),
    ctx_objs_cmds_revhints_ctx(
        CtxIn,
        FinalObjects, AllCommands, AllRevHints,
        CtxOut
    ).

% ==========================================================
% Spawn Processing (from Addendum 3)
% ==========================================================
is_spawn_request(spawn_request(_, _, _)).

process_spawn_requests([], ID, [], ID).
process_spawn_requests(
    [spawn_request(Type, Pos, Acts)|Rest],
    IDIn,
    [object(
        id(ObjID),
        type(Type),
        attrs([Pos]),
        actions(Acts),
        collisions([])
    )|RestObjs],
    IDOut
) :-
    % Generate ID: just use the integer directly
    ObjID = IDIn,
    
    IDIn1 #= IDIn + 1,
    
    process_spawn_requests(Rest, IDIn1, RestObjs, IDOut).

% ==========================================================
% State Change Processing
% ==========================================================
is_state_change(state_change(_)).

apply_state_changes([], Status, Status).
apply_state_changes(
    [state_change(game_over(lost))|_],
    _, lost
) :- !.
apply_state_changes(
    [state_change(game_over(won))|Rest],
    Status, FinalStatus
) :-
    ( Status = lost ->
        FinalStatus = lost
    ;
        apply_state_changes(Rest, won, FinalStatus)
    ).
apply_state_changes(
    [_|Rest], Status, FinalStatus
) :-
    apply_state_changes(Rest, Status, FinalStatus).

% ==========================================================
% ==========================================================
% TESTS
% ==========================================================
% ==========================================================

% ==========================================================
% Tests for yields/1
% ==========================================================

test("yields: wait_frames with positive number should \
yield", (
    A = wait_frames(5),
    yields(A)
)).

test("yields: wait_frames with zero should not yield", (
    A = wait_frames(0),
    \+ yields(A)
)).

test("yields: move_to with positive frames should yield", (
    A = move_to(10, 20, 3),
    yields(A)
)).

test("yields: can generate yielding actions \
bidirectionally", (
    yields(A),
    ( A = wait_frames(_)
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
        [],
        status(playing),
        next_id(1),
        [],
        []
    )),
    tick_object(
        ctx_old(Ctx),
        obj_old(ObjIn),
        obj_new(ObjOut),
        cmds_new(Commands),
        revhints_new(RevHints)
    ),
    ObjOut = [object(
        id(1),
        type(static),
        attrs([pos(0, 0)]),
        actions([]),
        collisions([])
    )],
    Commands = [],
    RevHints = []
)).

test("tick_object: yielding action (wait_frames) stops \
after one execution", (
    ObjIn = object(
        id(1),
        type(static),
        attrs([pos(0, 0)]),
        actions([wait_frames(5)]),
        collisions([])
    ),
    Ctx = ctx(state(
        frame(0),
        [],
        status(playing),
        next_id(1),
        [],
        []
    )),
    tick_object(
        ctx_old(Ctx),
        obj_old(ObjIn),
        obj_new(ObjOut),
        cmds_new(Commands),
        revhints_new(RevHints)
    ),
    ObjOut = [object(
        id(1),
        type(static),
        attrs([pos(0, 0)]),
        actions([wait_frames(4)]),
        collisions([])
    )],
    Commands = [],
    RevHints = []
)).

test("tick_object: wait_frames(0) is removed and execution \
continues until empty", (
    ObjIn = object(
        id(1),
        type(static),
        attrs([pos(0, 0)]),
        actions([wait_frames(0)]),
        collisions([])
    ),
    Ctx = ctx(state(
        frame(0),
        [],
        status(playing),
        next_id(1),
        [],
        []
    )),
    tick_object(
        ctx_old(Ctx),
        obj_old(ObjIn),
        obj_new(ObjOut),
        cmds_new(Commands),
        revhints_new(RevHints)
    ),
    ObjOut = [object(
        id(1),
        type(static),
        attrs([pos(0, 0)]),
        actions([]),
        collisions([])
    )],
    Commands = [],
    RevHints = []
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
(wait_frames)", (
    CtxIn = ctx(state(
        frame(0),
        objects([object(
            id(0),
            type(static),
            attrs([pos(0, 0)]),
            actions([wait_frames(3)]),
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
            actions([wait_frames(2)]),
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
                        wait_frames(3),
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
                        wait_frames(3),
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
                        wait_frames(3),
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
                        wait_frames(5),
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
