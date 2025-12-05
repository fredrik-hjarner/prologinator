% Tower Defense Game Engine
% Based on design v2 + all addendums

:- module(engine, [
    tick/2,
    tick_object/4,
    yields/1
]).

:- use_module(library(clpz)).
:- use_module(library(lists), [
    append/2,
    append/3,
    select/3,
    member/2,
    maplist/5,
    list_to_set/2,
    length/2
]).
:- use_module('./third_party/exclude', [exclude/3]).
:- use_module('./execute_action', [execute_action/5]).
:- use_module('./types/constraints', [
    game_state_constraint/1,
    game_object_constraint/1,
    game_status_constraint/1,
    command_constraint/1,
    rev_hint_constraint/1,
    action_constraint/1,
    pos_constraint/1,
    attr_constraint/1,
    collision_constraint/1
]).
:- use_module('./types/validation', [
    game_state_validation/1,
    game_object_validation/1
]).

% test/2 clauses are intentionally separated by other code
% (tests are placed near the code they test)
:- discontiguous(test/2).

% ==========================================================
% State Structure (from Addendum 3 - FINAL version)
% ==========================================================
% game_state(
%   frame(N),
%   objects([...]),
%   game_status(playing|won|lost),
%   score(Score),
%   next_id(NextID),
%   commands([...]),
%   reverse_hints([...])
% )

% ==========================================================
% GameObject Structure
% ==========================================================
% game_object(
%   id(ID),
%   attrs([...]),
%   actions([...]),
%   collisions([...])
% )

% ==========================================================
% Yielding Actions (from Addendum 1 + 3)
% ==========================================================
% Bidirectional: works forward (check if action yields) and
% backward (generate yielding actions)
% Modes: yields(?Action) - can be called with Action bound
% or unbound
% :- pred yields(?Action) # "Checks if an action yields \
% control, or generates yielding actions.".

yields(wait_frames(N)) :- N #> 0.
yields(move_to(_, _, Frames)) :- Frames #> 0.
yields(parallel_running(_)).

% ==========================================================
% Tests
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
% Execution Model: tick_object (from Addendum 1)
% ==========================================================

% TODO: Add a tick_object wrapper that wraps a
% tick_object_impl. though refactorings are needed first...

% % forward execution
% :- pred tick_object(+ObjIn, -ObjOut, -AllCommands,
% -AllRevHints).

tick_object(
    game_object(
        id(ID), type(Type), attrs(Attrs), actions([]),
        collisions(Colls)
    ),
    game_object(
        id(ID), type(Type), attrs(Attrs), actions([]),
        collisions(Colls)
    ),
    [],
    []
) :- !. % TODO: Can this cut hurt bidirectionality?

tick_object(ObjIn, ObjOut, AllCommands, AllRevHints) :-
    game_object_validation(ObjIn),
    ObjIn = game_object(
        _ID, _Type, _A, actions([Act|_Rest]), collisions(_C)
    ),
    execute_action(Act, ObjIn, ObjTemp, C1, R1),
    ( ObjTemp = despawned ->
        ObjOut = despawned,
        AllCommands = C1,
        AllRevHints = R1
    ; yields(Act) ->
        ObjOut = ObjTemp,
        AllCommands = C1,
        AllRevHints = R1
    ;
        tick_object(ObjTemp, ObjOut, C2, R2),
        append(C1, C2, AllCommands),
        append(R1, R2, AllRevHints)
    ).

% ==========================================================
% Tests for tick_object
% ==========================================================

test("tick_object: empty action list returns unchanged \
object", (
    ObjIn = game_object(
        id(1),
        type(static),
        attrs([pos(0, 0)]),
        actions([]),
        collisions([])
    ),
    tick_object(ObjIn, ObjOut, Commands, RevHints),
    ObjOut = game_object(
        id(1),
        type(static),
        attrs([pos(0, 0)]),
        actions([]),
        collisions([])
    ),
    Commands = [],
    RevHints = []
)).

test("tick_object: yielding action (wait_frames) stops \
after one execution", (
    ObjIn = game_object(
        id(1),
        type(static),
        attrs([pos(0, 0)]),
        actions([wait_frames(5)]),
        collisions([])
    ),
    tick_object(ObjIn, ObjOut, Commands, RevHints),
    ObjOut = game_object(
        id(1),
        type(static),
        attrs([pos(0, 0)]),
        actions([wait_frames(4)]),
        collisions([])
    ),
    Commands = [],
    RevHints = []
)).

test("tick_object: wait_frames(0) is removed and execution \
continues until empty", (
    ObjIn = game_object(
        id(1),
        type(static),
        attrs([pos(0, 0)]),
        actions([wait_frames(0)]),
        collisions([])
    ),
    tick_object(ObjIn, ObjOut, Commands, RevHints),
    ObjOut = game_object(
        id(1),
        type(static),
        attrs([pos(0, 0)]),
        actions([]),
        collisions([])
    ),
    Commands = [],
    RevHints = []
)).

% ==========================================================
% Tests for tick
% ==========================================================

test("tick: increments frame and processes empty game \
state", (
    StateIn = game_state(
        frame(0),
        objects([game_object(
            id(0),
            type(static),
            attrs([pos(0, 0)]),
            actions([]),
            collisions([])
        )]),
        status(playing),
        score(0),
        next_id(1),
        commands([]),
        rev_hints([])
    ),
    tick(StateIn, StateOut),
    StateOut = game_state(
        frame(1),
        objects([game_object(
            id(0),
            type(static),
            attrs([pos(0, 0)]),
            actions([]),
            collisions([])
        )]),
        status(playing),
        score(0),
        next_id(1),
        commands([]),
        rev_hints([])
    )
)).

test("tick: processes object with yielding action \
(wait_frames)", (
    StateIn = game_state(
        frame(0),
        objects([game_object(
            id(0),
            type(static),
            attrs([pos(0, 0)]),
            actions([wait_frames(3)]),
            collisions([])
        )]),
        status(playing),
        score(0),
        next_id(1),
        commands([]),
        rev_hints([])
    ),
    tick(StateIn, StateOut),
    StateOut = game_state(
        frame(1),
        objects([game_object(
            id(0),
            type(static),
            attrs([pos(0, 0)]),
            actions([wait_frames(2)]),
            collisions([])
        )]),
        status(playing),
        score(0),
        next_id(1),
        commands([]),
        rev_hints([])
    )
)).

test("tick: processes spawn request and creates new \
object", (
    StateIn = game_state(
        frame(0),
        objects([game_object(
            id(0),
            type(static),
            attrs([pos(0, 0)]),
            actions([
                spawn(enemy, pos(5, 5), [])
            ]),
            collisions([])
        )]),
        status(playing),
        score(0),
        next_id(1),
        commands([]),
        rev_hints([])
    ),
    tick(StateIn, StateOut),
    StateOut = game_state(
        frame(1),
        objects(FinalObjs),
        status(playing),
        score(0),
        next_id(2),
        commands([]),
        rev_hints([])
    ),
    member(
        game_object(
            id(0),
            type(static),
            attrs([pos(0, 0)]),
            actions([]),
            collisions([])
        ),
        FinalObjs
    ),
    member(
        game_object(
            id(_NewID),
            type(enemy),
            attrs([pos(5, 5)]),
            actions([]),
            collisions([])
        ),
        FinalObjs
    )
)).

test("tick: processes state change (score increase)", (
    StateIn = game_state(
        frame(0),
        objects([game_object(
            id(0),
            type(static),
            attrs([pos(0, 0)]),
            actions([trigger_state_change(score(10))]),
            collisions([])
        )]),
        status(playing),
        score(0),
        next_id(1),
        commands([]),
        rev_hints([])
    ),
    tick(StateIn, StateOut),
    StateOut = game_state(
        frame(1),
        objects([game_object(
            id(0),
            type(static),
            attrs([pos(0, 0)]),
            actions([]),
            collisions([])
        )]),
        status(playing),
        score(NewScore),
        next_id(1),
        commands([]),
        rev_hints([])
    ),
    NewScore = 10
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
    InitialState = game_state(
        frame(0),
        objects([
            game_object(
                id(0), type(enemy), attrs([pos(5, 10)]),
                actions([
                    % Moving right, arrives at (10, 10) in 1
                    % frame
                    move_to(10, 10, 1)
                ]), collisions([])
            ),
            game_object(
                id(1), type(proj), attrs([pos(5, 10)]),
                actions([
                    % Moving to same target, arrives at
                    % (10, 10) in 1 frame
                    move_to(10, 10, 1)
                ]), collisions([])
            )
        ]),
        status(playing),
        score(0),
        next_id(2),
        commands([]),
        rev_hints([])
    ),
    % Run 2 frames - collision should happen at frame 1 when
    % both reach (10, 10)
    tick(InitialState, State1),
    State1 = game_state(
        frame(1),
        objects(Objs1),
        status(_),
        score(_),
        next_id(_),
        commands(_),
        rev_hints(_)
    ),
    % After collision at frame 1, both objects should be
    % removed
    length(Objs1, ObjCount),
    ObjCount = 0
)).

% ==========================================================
% Performance Test: Run game to frame 31 (reproduces freeze)
% ==========================================================

% TODO: fix perf issue.
test("performance: run game to frame 32 to reproduce \
freeze (first collision)", (
    % Same initial state as game.pl
    InitialState = game_state(
        frame(0),
        objects([
            game_object(
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
            game_object(
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
            game_object(
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
            game_object(
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
        score(0),
        next_id(4),
        commands([]),
        rev_hints([])
    ),
    % Run tick 32 times (first collision should happen
    % around here)
    run_ticks(InitialState, 32, FinalState),
    FinalState = game_state(
        frame(32),
        objects(_),
        status(_),
        score(_),
        next_id(_),
        commands(_),
        rev_hints(_)
    )
)).

% Helper: run tick N times
run_ticks(State, 0, State).
run_ticks(StateIn, N, StateOut) :-
    N > 0,
    tick(StateIn, StateNext),
    N1 is N - 1,
    run_ticks(StateNext, N1, StateOut).

% ==========================================================
% Main Tick Function (from Addendum 3 - FINAL version)
% ==========================================================
tick(StateIn, StateOut) :-
    % game_state_constraint(StateIn),
    game_state_validation(StateIn),
    StateIn = game_state(
        frame(F),
        objects(Objs),
        status(Status),
        score(Score),
        next_id(NextID),
        commands(_OldCommands),
        rev_hints(_OldRevHints)
    ),
    
    % 1. Tick all objects
    tick_all_objects(Objs, TempObjs, Commands1, RevHints1),
    
    % 2. Detect collisions (collisions only produce
    % rev_hints, no commands)
    detect_collisions(TempObjs, NewObjs, RevHints2),
    
    % 3. Combine commands and rev_hints
    AllCommands = Commands1,
    append(RevHints1, RevHints2, AllRevHints),
    
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
    apply_state_changes(StateChanges, Status, Score,
                        NewStatus, NewScore),
    
    F1 #= F + 1,
    
    % 8. Build new state
    StateOut = game_state(
        frame(F1),
        objects(FinalObjs),
        status(NewStatus),
        score(NewScore),
        next_id(NewNextID),
        commands([]),
        rev_hints(AllRevHints)
    ).
    % NOTE: placing this at the top caused cpu to go bananas
    %       so there are some performance problems with it..
    % game_state_constraint(StateOut).

% ==========================================================
% Tick Helpers
% ==========================================================
tick_all_objects(
    Objects, FinalObjects, AllCommands, AllRevHints
) :-
    maplist(
        tick_object, % predicate
        Objects, % "input"
        TempObjects, % collected "output"
        CommandLists, % collected "output"
        RevHintLists % collected "output"
    ),
    % append/2 flattens the list of lists
    append(CommandLists, AllCommands),
    % append/2 flattens the list of lists
    append(RevHintLists, AllRevHints),
    exclude(is_despawned, TempObjects, FinalObjects).

is_despawned(despawned).

% ==========================================================
% Spawn Processing (from Addendum 3)
% ==========================================================
is_spawn_request(spawn_request(_, _, _)).

process_spawn_requests([], ID, [], ID).
process_spawn_requests(
    [spawn_request(Type, Pos, Acts)|Rest],
    IDIn,
    [game_object(
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

apply_state_changes([], Status, Score, Status, Score).
apply_state_changes(
    [state_change(game_over(lost))|_],
    _, Score, lost, Score
) :- !.
apply_state_changes(
    [state_change(game_over(won))|Rest],
    Status, Score, FinalStatus, FinalScore
) :-
    ( Status = lost ->
        FinalStatus = lost, FinalScore = Score
    ;
        apply_state_changes(
            Rest, won, Score, FinalStatus, FinalScore
        )
    ).
apply_state_changes(
    [state_change(score(Delta))|Rest],
    Status, Score, FinalStatus, FinalScore
) :-
    NewScore #= Score + Delta,
    apply_state_changes(
        Rest, Status, NewScore, FinalStatus, FinalScore
    ).
apply_state_changes(
    [_|Rest], Status, Score,FinalStatus, FinalScore
) :-
    apply_state_changes(
        Rest, Status, Score, FinalStatus, FinalScore
    ).

% ==========================================================
% Collision Detection (simplified - grid-based)
% ==========================================================
% Note: CiaoPP cannot verify the findall call, but the code
% is correct
% :- trust pred detect_collisions(+list(game_object),
% -list(game_object), -list(hint)).

detect_collisions(Objects, NewObjects, RevHints) :-
    findall(
        collision(ID1, ID2),
        (
            member(
                game_object(
                    id(ID1), _, attrs(A1), _, collisions(_)
                ),
                Objects
            ),
            member(
                game_object(
                    id(ID2), _, attrs(A2), _, collisions(_)
                ),
                Objects
            ),
            ID1 @< ID2,
            member(pos(X1, Y1), A1),
            member(pos(X2, Y2), A2),
            collides_at(X1, Y1, X2, Y2)
        ),
        Collisions
    ),
    handle_collisions(
        Objects, Collisions, NewObjects, RevHints
    ).

collides_at(X, Y, X, Y).

handle_collisions(
    Objects,
    Collisions,
    NewObjects,
    RevHints
) :-
    findall(
        (ProjID, EnemyID),
        (
            member(collision(ID1, ID2), Collisions),
            member(
                game_object(
                    id(ID1), type(proj), _, _, collisions(_)
                ),
                Objects
            ),
            member(
                game_object(
                    id(ID2),
                    type(enemy),
                    _,
                    _,
                    collisions(_)
                ),
                Objects
            ),
            ProjID = ID1, EnemyID = ID2
        ;
            member(collision(ID1, ID2), Collisions),
            member(
                game_object(
                    id(ID1),
                    type(enemy),
                    _,
                    _,
                    collisions(_)
                ),
                Objects
            ),
            member(
                game_object(
                    id(ID2),
                    type(proj),
                    _,
                    _,
                    collisions(_)
                ),
                Objects
            ),
            ProjID = ID2, EnemyID = ID1
        ),
        ToRemove
    ),
    findall(ID, member((ID, _), ToRemove), P1),
    findall(ID, member((_, ID), ToRemove), P2),
    append(P1, P2, AllIDs),
    list_to_set(AllIDs, UniqueIDs),
    % TODO: I dont like that what happens at collision is
    % hard-coded like this.
    %       should be dynamic.
    remove_with_rev_hints(
        Objects, UniqueIDs, NewObjects, RevHints
    ).

% :- pred remove_with_rev_hints(+Objects,
% +IDsOfObjectsToRemove, -NewObjects, -RevHints).

remove_with_rev_hints([], _, [], []).
remove_with_rev_hints(
    [game_object(
        id(ID), _, attrs(AttrList), _, collisions(_)
    )|Rest],
    ToRemove,
    NewObjs,
    [despawned(ID, AttrList)|RestRevHints]
) :-
    member(ID, ToRemove),
    !,
    remove_with_rev_hints(
        Rest, ToRemove, NewObjs, RestRevHints
    ).
remove_with_rev_hints(
    [Obj|Rest], ToRemove, [Obj|NewObjs], RevHints
) :-
    remove_with_rev_hints(
        Rest, ToRemove, NewObjs, RevHints
    ).

% ==========================================================
% Partition helper (if not available in Ciao)
% ==========================================================
%  TODO:
partition(_Pred, [], [], []).
partition(Pred, [X|Xs], Yes, No) :-
    ( call(Pred, X) ->
        Yes = [X|YesRest],
        partition(Pred, Xs, YesRest, No)
    ;
        No = [X|NoRest],
        partition(Pred, Xs, Yes, NoRest)
    ).

