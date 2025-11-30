% Tower Defense Game Engine
% Based on design v2 + all addendums

:- module(engine, [
    tick/2,
    tick_object/3,
    yields/1
], [clpfd, assertions, unittestdecls, modes, regtypes]).

:- use_module(library(lists), [append/3, select/3, member/2]).
:- use_module(library(llists), [append/2]).
:- use_module(library(hiordlib), [maplist/4]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(engine(hiord_rt), [call/2]).
:- use_module(engine(atomic_basic), [atom_number/2]).
:- use_module('./third_party/exclude', [exclude/3]).
:- use_module('./third_party/list_to_set', [list_to_set/2]).
:- use_module('./execute_action', [execute_action/4]).
:- use_module('./types', [
    game_state/1,
    game_object/1,
    game_status/1,
    keyframe/1,
    hint/1,
    action/1,
    pos/1,
    attr/1,
    collision/1
]).

% ============================================================================
% State Structure (from Addendum 3 - FINAL version)
% ============================================================================
% game_state(
%   frame(N),
%   objects([...]),
%   game_status(playing|won|lost),
%   score(Score),
%   next_id(NextID),
%   last_keyframe(Keyframe),
%   reverse_hints([...])
% )

% ============================================================================
% GameObject Structure
% ============================================================================
% game_object(
%   id(ID),
%   attrs([...]),
%   actions([...]),
%   collisions([...])
% )

% ============================================================================
% Yielding Actions (from Addendum 1 + 3)
% ============================================================================
% Bidirectional: works forward (check if action yields) and backward (generate yielding actions)
% Modes: yields(?Action) - can be called with Action bound or unbound
:- pred yields(?Action) # "Checks if an action yields control, or generates yielding actions.".

yields(wait_frames(N)) :- N #> 0.
yields(move_to(_, _, Frames)) :- Frames #> 0.
yields(parallel_running(_)).

% ============================================================================
% Tests
% ============================================================================
:- test yields(A) : (A = wait_frames(5))
    => true
    # "wait_frames with positive number should yield".

:- test yields(A) : (A = wait_frames(0))
    + fails
    # "wait_frames with zero should not yield".

:- test yields(A) : (A = move_to(10, 20, 3))
    => true
    # "move_to with positive frames should yield".

:- test yields(A) : (true)
    => (A = wait_frames(_) ; A = move_to(_, _, _) ; A = parallel_running(_))
    # "yields/1 can generate yielding actions bidirectionally".

% ============================================================================
% Execution Model: tick_object (from Addendum 1)
% ============================================================================
tick_object(
    game_object(ID, Type, attrs(Attrs), [], Colls),
    game_object(ID, Type, attrs(Attrs), [], Colls),
    []
) :- !.

tick_object(ObjIn, ObjOut, AllHints) :-
    ObjIn = game_object(_ID, _Type, _A, [Act|_Rest], _C),
    execute_action(Act, ObjIn, ObjTemp, H1),
    ( ObjTemp = despawned ->
        ObjOut = despawned,
        AllHints = H1
    ; yields(Act) ->
        ObjOut = ObjTemp,
        AllHints = H1
    ;
        tick_object(ObjTemp, ObjOut, H2),
        append(H1, H2, AllHints)
    ).

% ============================================================================
% Tests for tick_object
% ============================================================================

:- test tick_object(ObjIn, ObjOut, Hints) : (
        ObjIn = game_object(
            id1,
            static,
            attrs([pos(0, 0)]),
            [],
            []
        )
    ) => (
        ObjOut = game_object(id1, static, attrs([pos(0, 0)]), [], []),
        Hints = []
    )
    # "tick_object: empty action list returns unchanged object".

:- test tick_object(ObjIn, ObjOut, Hints) : (
        ObjIn = game_object(
            id1,
            static,
            attrs([pos(0, 0)]),
            [wait_frames(5)],
            []
        )
    ) => (
        ObjOut = game_object(id1, static, attrs([pos(0, 0)]), [wait_frames(4)], []),
        Hints = []
    )
    # "tick_object: yielding action (wait_frames) stops after one execution".

:- test tick_object(ObjIn, ObjOut, Hints) : (
        ObjIn = game_object(
            id1,
            static,
            attrs([pos(0, 0)]),
            [wait_frames(0)],
            []
        )
    ) => (
        ObjOut = game_object(id1, static, attrs([pos(0, 0)]), [], []),
        Hints = []
    )
    # "tick_object: wait_frames(0) is removed and execution continues until empty".

% ============================================================================
% Tests for tick
% ============================================================================

:- test tick(StateIn, StateOut) : (
        StateIn = game_state(
            0,
            [game_object(id1, static, attrs([pos(0, 0)]), [], [])],
            playing,
            0,
            1,
            keyframe(0, []),
            []
        )
    ) => (
        StateOut = game_state(
            1,
            [game_object(id1, static, attrs([pos(0, 0)]), [], [])],
            playing,
            0,
            1,
            _,
            []
        )
    )
    # "tick: increments frame and processes empty game state".

:- test tick(StateIn, StateOut) : (
        StateIn = game_state(
            0,
            [game_object(id1, static, attrs([pos(0, 0)]), [wait_frames(3)], [])],
            playing,
            0,
            1,
            keyframe(0, []),
            []
        )
    ) => (
        StateOut = game_state(
            1,
            [game_object(id1, static, attrs([pos(0, 0)]), [wait_frames(2)], [])],
            playing,
            0,
            1,
            _,
            []
        )
    )
    # "tick: processes object with yielding action (wait_frames)".

:- test tick(StateIn, StateOut) : (
        StateIn = game_state(
            0,
            [game_object(id1, static, attrs([pos(0, 0)]), [spawn(enemy, pos(5, 5), [])], [])],
            playing,
            0,
            1,
            keyframe(0, []),
            []
        )
    ) => (
        StateOut = game_state(
            1,
            FinalObjs,
            playing,
            0,
            2,
            _,
            []
        ),
        member(game_object(id1, static, attrs([pos(0, 0)]), [], []), FinalObjs),
        member(game_object(_NewID, enemy, attrs([pos(5, 5)]), [], []), FinalObjs)
    )
    # "tick: processes spawn request and creates new object".

:- test tick(StateIn, StateOut) : (
        StateIn = game_state(
            0,
            [game_object(id1, static, attrs([pos(0, 0)]), [trigger_state_change(score(10))], [])],
            playing,
            0,
            1,
            keyframe(0, []),
            []
        )
    ) => (
        StateOut = game_state(
            1,
            [game_object(id1, static, attrs([pos(0, 0)]), [], [])],
            playing,
            NewScore,
            1,
            _,
            []
        ),
        NewScore = 10
    )
    # "tick: processes state change (score increase)".

% ============================================================================
% Main Tick Function (from Addendum 3 - FINAL version)
% ============================================================================
tick(StateIn, StateOut) :-
    StateIn = game_state(
        F, Objs, Status, Score, NextID,
        LastKF, _OldHints
    ),
    
    % 1. Tick all objects
    tick_all_objects(Objs, TempObjs, Hints1),
    
    % 2. Detect collisions
    detect_collisions(TempObjs, NewObjs, Hints2),
    
    % 3. Combine hints
    append(Hints1, Hints2, AllHints),
    
    % 4. Partition hints
    partition(is_spawn_request, AllHints, SpawnReqs, OtherHints),
    partition(is_state_change, OtherHints, StateChanges, ReverseHints),
    
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
    
    % 8. Create keyframe if needed
    ( should_create_keyframe(F1) ->
        NewKF = keyframe(F1, FinalObjs)
    ;
        NewKF = LastKF
    ),
    
    % 9. Build new state
    StateOut = game_state(
        F1, FinalObjs, NewStatus, NewScore,
        NewNextID, NewKF, ReverseHints
    ).

% ============================================================================
% Tick Helpers
% ============================================================================
tick_all_objects(Objects, FinalObjects, AllHints) :-
    maplist(tick_object, Objects, TempObjects, HintLists),
    append(HintLists, AllHints),
    exclude(is_despawned, TempObjects, FinalObjects).

is_despawned(despawned).

% ============================================================================
% Spawn Processing (from Addendum 3)
% ============================================================================
is_spawn_request(spawn_request(_, _, _)).

process_spawn_requests([], ID, [], ID).
process_spawn_requests(
    [spawn_request(Type, Pos, Acts)|Rest],
    IDIn,
    [game_object(ObjID, Type, attrs([Pos]), Acts, [])|RestObjs],
    IDOut
) :-
    % Generate ID: type_counter
    atom_concat(Type, '_', Prefix),
    atom_number(IDInAtom, IDIn),
    atom_concat(Prefix, IDInAtom, ObjID),
    
    IDIn1 #= IDIn + 1,
    
    process_spawn_requests(Rest, IDIn1, RestObjs, IDOut).

% ============================================================================
% State Change Processing
% ============================================================================
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
        apply_state_changes(Rest, won, Score, FinalStatus, FinalScore)
    ).
apply_state_changes(
    [state_change(score(Delta))|Rest],
    Status, Score, FinalStatus, FinalScore
) :-
    NewScore #= Score + Delta,
    apply_state_changes(Rest, Status, NewScore, FinalStatus, FinalScore).
apply_state_changes([_|Rest], Status, Score, FinalStatus, FinalScore) :-
    apply_state_changes(Rest, Status, Score, FinalStatus, FinalScore).

% ============================================================================
% Collision Detection (simplified - grid-based)
% ============================================================================
% Note: CiaoPP cannot verify the findall call, but the code is correct
% :- trust pred detect_collisions(+list(game_object), -list(game_object), -list(hint)).

detect_collisions(Objects, NewObjects, Hints) :-
    findall(
        collision(ID1, ID2),
        (
            member(game_object(ID1, _, attrs(A1), _, _), Objects),
            member(game_object(ID2, _, attrs(A2), _, _), Objects),
            ID1 @< ID2,
            member(pos(X1, Y1), A1),
            member(pos(X2, Y2), A2),
            collides_at(X1, Y1, X2, Y2)
        ),
        Collisions
    ),
    handle_collisions(Objects, Collisions, NewObjects, Hints).

collides_at(X, Y, X, Y).

handle_collisions(Objects, Collisions, NewObjects, Hints) :-
    findall(
        (ProjID, EnemyID),
        (
            member(collision(ID1, ID2), Collisions),
            member(game_object(ID1, proj, _, _, _), Objects),
            member(game_object(ID2, enemy, _, _, _), Objects),
            ProjID = ID1, EnemyID = ID2
        ;
            member(collision(ID1, ID2), Collisions),
            member(game_object(ID1, enemy, _, _, _), Objects),
            member(game_object(ID2, proj, _, _, _), Objects),
            ProjID = ID2, EnemyID = ID1
        ),
        ToRemove
    ),
    findall(ID, member((ID, _), ToRemove), P1),
    findall(ID, member((_, ID), ToRemove), P2),
    append(P1, P2, AllIDs),
    list_to_set(AllIDs, UniqueIDs),
    remove_with_hints(Objects, UniqueIDs, NewObjects, Hints).

remove_with_hints([], _, [], []).
remove_with_hints(
    [game_object(ID, _, Attrs, _, _)|Rest],
    ToRemove,
    NewObjs,
    [despawned(ID, Attrs)|RestHints]
) :-
    member(ID, ToRemove),
    !,
    remove_with_hints(Rest, ToRemove, NewObjs, RestHints).
remove_with_hints([Obj|Rest], ToRemove, [Obj|NewObjs], Hints) :-
    remove_with_hints(Rest, ToRemove, NewObjs, Hints).

% ============================================================================
% Keyframes
% ============================================================================
should_create_keyframe(F) :-
    F mod 10 #= 0.

% ============================================================================
% Partition helper (if not available in Ciao)
% ============================================================================
partition(_Pred, [], [], []).
partition(Pred, [X|Xs], Yes, No) :-
    ( call(Pred, X) ->
        Yes = [X|YesRest],
        partition(Pred, Xs, YesRest, No)
    ;
        No = [X|NoRest],
        partition(Pred, Xs, Yes, NoRest)
    ).

