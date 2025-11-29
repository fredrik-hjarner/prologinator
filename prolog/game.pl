% Tower Defense Game Engine
% Based on design v2 + all addendums

:- module(game, [
    tick/2,
    execute_action/4,
    tick_object/3,
    yields/1
], [clpfd]).

:- use_module(library(lists), [append/3, select/3, member/2]).
:- use_module(library(llists), [append/2]).
:- use_module(library(hiordlib), [maplist/4]).
:- use_module(library(aggregates), [findall/3]).
:- use_module(engine(hiord_rt), [call/2]).
:- use_module(engine(atomic_basic), [atom_number/2]).
:- use_module('./third_party/exclude', [exclude/3]).
:- use_module('./third_party/list_to_set', [list_to_set/2]).

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
% Basic Actions: wait_frames
% ============================================================================
execute_action(
    wait_frames(N),
    game_object(ID, Attrs, [_|Rest], Colls),
    game_object(ID, Attrs, NewActions, Colls),
    []
) :-
    ( N #> 1 ->
        N1 #= N - 1,
        NewActions = [wait_frames(N1)|Rest]
    ;
        % N = 1, wait is done
        NewActions = Rest
    ).

% ============================================================================
% Basic Actions: move_to
% ============================================================================
execute_action(
    move_to(TargetX, TargetY, Frames),
    game_object(ID, Attrs, [_|Rest], Colls),
    game_object(ID, NewAttrs, NewActions, Colls),
    []
) :-
    select(pos(CurrentX, CurrentY), Attrs, RestAttrs),
    
    % Compute step
    DX #= (TargetX - CurrentX) // Frames,
    DY #= (TargetY - CurrentY) // Frames,
    
    NewX #= CurrentX + DX,
    NewY #= CurrentY + DY,
    
    NewAttrs = [pos(NewX, NewY)|RestAttrs],
    
    ( Frames #> 1 ->
        Frames1 #= Frames - 1,
        NewActions = [move_to(TargetX, TargetY, Frames1)|Rest]
    ;
        NewActions = Rest  % Arrived
    ).

% ============================================================================
% Basic Actions: despawn
% ============================================================================
execute_action(
    despawn,
    game_object(ID, Attrs, _, Colls),
    despawned,
    [despawned(ID, Attrs)]
) :- !.

% ============================================================================
% Compound Actions: spawn (from Addendum 3 - FINAL version)
% ============================================================================
execute_action(
    spawn(Type, Pos, Actions),
    game_object(ID, Attrs, [_|Rest], Colls),
    game_object(ID, Attrs, Rest, Colls),
    [spawn_request(Type, Pos, Actions)]
).

% ============================================================================
% Compound Actions: loop
% ============================================================================
execute_action(
    loop(Actions),
    game_object(ID, Attrs, [_|Rest], Colls),
    game_object(ID, Attrs, NewActions, Colls),
    []
) :-
    append(Actions, [loop(Actions)], Expanded),
    append(Expanded, Rest, NewActions).

% ============================================================================
% Compound Actions: trigger_state_change
% ============================================================================
execute_action(
    trigger_state_change(Change),
    game_object(ID, Attrs, [_|Rest], Colls),
    game_object(ID, Attrs, Rest, Colls),
    [state_change(Change)]
).

% ============================================================================
% Compound Actions: parallel (from Addendum 2 + 3 - FINAL version)
% ============================================================================
execute_action(
    parallel(ChildActions),
    game_object(ID, AttrsIn, [_|Rest], Colls),
    Result,
    AllHints
) :-
    tick_parallel_children(
        ChildActions, ID, AttrsIn, Colls,
        AttrsOut, UpdatedChildren, AllHints
    ),
    ( member(caused_despawn, UpdatedChildren) ->
        Result = despawned
    ; all_children_done(UpdatedChildren) ->
        Result = game_object(ID, AttrsOut, Rest, Colls)
    ;
        NewActions = [parallel_running(UpdatedChildren)|Rest],
        Result = game_object(ID, AttrsOut, NewActions, Colls)
    ).

execute_action(
    parallel_running(Children),
    Obj, NewObj, Hints
) :-
    execute_action(parallel(Children), Obj, NewObj, Hints).

tick_parallel_children(
    [], _ID, Attrs, _Colls,
    Attrs, [], []
).
tick_parallel_children(
    [Child|RestChildren],
    ID, AttrsIn, Colls,
    AttrsOut, [UpdatedChild|RestUpdated],
    AllHints
) :-
    tick_one_child(
        Child, ID, AttrsIn, Colls,
        Attrs1, UpdatedChild, Hints1
    ),
    tick_parallel_children(
        RestChildren, ID, Attrs1, Colls,
        AttrsOut, RestUpdated, Hints2
    ),
    append(Hints1, Hints2, AllHints).

tick_one_child(done, _, A, _, A, done, []) :- !.

tick_one_child(
    caused_despawn,
    _, A, _, A, caused_despawn, []
) :- !.

tick_one_child(Child, ID, AIn, C, AOut, U, H) :-
    execute_action(
        Child,
        game_object(ID, AIn, [Child], C),
        Result,
        H
    ),
    ( Result = despawned ->
        U = caused_despawn,
        AOut = AIn
    ;
        Result = game_object(_, AOut, NewActs, _),
        ( NewActs = [] -> U = done ; [U|_] = NewActs )
    ).

all_children_done([]).
all_children_done([done|R]) :-
    all_children_done(R).
all_children_done([caused_despawn|_]) :-
    !, fail.

% ============================================================================
% Yielding Actions (from Addendum 1 + 3)
% ============================================================================
yields(wait_frames(N)) :- N #> 0.
yields(move_to(_, _, Frames)) :- Frames #> 0.
yields(parallel_running(_)).

% ============================================================================
% Execution Model: execute_until_yield (from Addendum 1)
% ============================================================================
tick_object(Obj, NewObj, Hints) :-
    execute_until_yield(Obj, NewObj, Hints).

execute_until_yield(
    game_object(ID, Attrs, [], Colls),
    game_object(ID, Attrs, [], Colls),
    []
) :- !.

execute_until_yield(ObjIn, ObjOut, AllHints) :-
    ObjIn = game_object(ID, A, [Act|Rest], C),
    execute_action(Act, ObjIn, ObjTemp, H1),
    ( ObjTemp = despawned ->
        ObjOut = despawned,
        AllHints = H1
    ; yields(Act) ->
        ObjOut = ObjTemp,
        AllHints = H1
    ;
        execute_until_yield(ObjTemp, ObjOut, H2),
        append(H1, H2, AllHints)
    ).

% ============================================================================
% Main Tick Function (from Addendum 3 - FINAL version)
% ============================================================================
tick(StateIn, StateOut) :-
    StateIn = game_state(
        F, Objs, Status, Score, NextID,
        LastKF, OldHints
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
    [game_object(ObjID, attrs([Pos]), Acts, [])|RestObjs],
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
detect_collisions(Objects, NewObjects, Hints) :-
    findall(
        collision(ID1, ID2),
        (
            member(game_object(ID1, attrs(A1), _, _), Objects),
            member(game_object(ID2, attrs(A2), _, _), Objects),
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
            ((is_proj(ID1), is_enemy(ID2), ProjID=ID1, EnemyID=ID2) ;
             (is_enemy(ID1), is_proj(ID2), ProjID=ID2, EnemyID=ID1))
        ),
        ToRemove
    ),
    findall(ID, member((ID, _), ToRemove), P1),
    findall(ID, member((_, ID), ToRemove), P2),
    append(P1, P2, AllIDs),
    list_to_set(AllIDs, UniqueIDs),
    remove_with_hints(Objects, UniqueIDs, NewObjects, Hints).

is_proj(ID) :- atom_concat(proj, _, ID).
is_enemy(ID) :- atom_concat(enemy, _, ID).

remove_with_hints([], _, [], []).
remove_with_hints(
    [game_object(ID, Attrs, _, _)|Rest],
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

