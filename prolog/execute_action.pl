% Action Execution Module
% Handles execution of all game actions

:- module(execute_action, [
    execute_action/4
], [clpfd, assertions, unittestdecls, modes, regtypes]).

:- use_module(library(lists), [append/3, select/3, member/2]).
:- use_module(library(llists), [append/2]).
:- use_module(library(unittest/unittest_props), [try_sols/2]).
:- use_module('./types', [
    game_object/1,
    action/1,
    hint/1
]).

% ============================================================================
% execute_action/4
% ============================================================================

% Forward execution (normal game)
:- pred execute_action(+Action, +ObjIn, -ObjOut, -Hints) 
    :: (action(Action), game_object(ObjIn)) 
    => (action(Action), game_object(ObjOut), list(hint, Hints)).

% Reverse execution (undo/replay)
:- pred execute_action(-Action, -ObjIn, +ObjOut, +Hints).

% Action inference - which must the action have been?
:- pred execute_action(-Action, +ObjIn, +ObjOut, +Hints).

% Validation (consistency check)
:- pred execute_action(+Action, +ObjIn, +ObjOut, +Hints).

% ----------------------------------------------------------------------------
% Basic Actions: wait_frames
% ----------------------------------------------------------------------------

execute_action(
    wait_frames(N),
    game_object(ID, Type, attrs(Attrs), [_|Rest], Colls),
    game_object(ID, Type, attrs(Attrs), NewActions, Colls),
    []
) :-
    ( N #> 1 ->
        N1 #= N - 1,
        NewActions = [wait_frames(N1)|Rest]
    ;
        % N = 1, wait is done
        NewActions = Rest
    ).

% ----------------------------------------------------------------------------
% Basic Actions: move_to
% ----------------------------------------------------------------------------

execute_action(
    move_to(TargetX, TargetY, Frames),
    game_object(ID, Type, attrs(Attrs), [_|Rest], Colls),
    game_object(ID, Type, attrs(NewAttrs), NewActions, Colls),
    []
) :-
    select(pos(CurrentX, CurrentY), Attrs, RestAttrs),
    compute_move_position(TargetX, TargetY, CurrentX, CurrentY, Frames, NewX, NewY),
    % Label at the boundary where we need ground values for game objects
    (ground(TargetX), ground(TargetY), ground(CurrentX), ground(CurrentY), ground(Frames) ->
        labeling([], [NewX, NewY])
    ;
        true
    ),
    NewAttrs = [pos(NewX, NewY)|RestAttrs],
    ( Frames #> 1 ->
        Frames1 #= Frames - 1,
        NewActions = [move_to(TargetX, TargetY, Frames1)|Rest]
    ;
        NewActions = Rest  % Arrived
    ).

% ----------------------------------------------------------------------------
% Basic Actions: despawn
% ----------------------------------------------------------------------------

execute_action(
    despawn,
    game_object(ID, _Type, attrs(Attrs), _, _Colls),
    despawned,
    [despawned(ID, Attrs)]
) :- !.

% ----------------------------------------------------------------------------
% Compound Actions: spawn (from Addendum 3 - FINAL version)
% ----------------------------------------------------------------------------

execute_action(
    spawn(Type, Pos, Actions),
    game_object(ID, ObjType, attrs(Attrs), [_|Rest], Colls),
    game_object(ID, ObjType, attrs(Attrs), Rest, Colls),
    [spawn_request(Type, Pos, Actions)]
).

% ----------------------------------------------------------------------------
% Compound Actions: loop
% ----------------------------------------------------------------------------

execute_action(
    loop(Actions),
    game_object(ID, Type, attrs(Attrs), [_|Rest], Colls),
    game_object(ID, Type, attrs(Attrs), NewActions, Colls),
    []
) :-
    append(Actions, [loop(Actions)], Expanded),
    append(Expanded, Rest, NewActions).

% ----------------------------------------------------------------------------
% Compound Actions: trigger_state_change
% ----------------------------------------------------------------------------

execute_action(
    trigger_state_change(Change),
    game_object(ID, Type, attrs(Attrs), [_|Rest], Colls),
    game_object(ID, Type, attrs(Attrs), Rest, Colls),
    [state_change(Change)]
).

% ----------------------------------------------------------------------------
% Compound Actions: parallel (from Addendum 2 + 3 - FINAL version)
% ----------------------------------------------------------------------------

execute_action(
    parallel(ChildActions),
    game_object(ID, Type, attrs(AttrsIn), [_|Rest], Colls),
    Result,
    AllHints
) :-
    tick_parallel_children(
        ChildActions, ID, Type, AttrsIn, Colls,
        AttrsOut, UpdatedChildren, AllHints
    ),
    ( member(caused_despawn, UpdatedChildren) ->
        Result = despawned
    ; all_children_done(UpdatedChildren) ->
        Result = game_object(ID, Type, attrs(AttrsOut), Rest, Colls)
    ;
        NewActions = [parallel_running(UpdatedChildren)|Rest],
        Result = game_object(ID, Type, attrs(AttrsOut), NewActions, Colls)
    ).

% ----------------------------------------------------------------------------
% Compound Actions: parallel_running
% ----------------------------------------------------------------------------

execute_action(
    parallel_running(Children),
    Obj, NewObj, Hints
) :-
    execute_action(parallel(Children), Obj, NewObj, Hints).

% ============================================================================
% tick_parallel_children/7
% ============================================================================

:- pred tick_parallel_children(?Children, ?ID, ?Type, ?AttrsIn, ?Colls, ?AttrsOut, ?UpdatedChildren, ?AllHints).

tick_parallel_children(
    [], _ID, _Type, Attrs, _Colls,
    Attrs, [], []
).

tick_parallel_children(
    [Child|RestChildren],
    ID, Type, AttrsIn, Colls,
    AttrsOut, [UpdatedChild|RestUpdated],
    AllHints
) :-
    tick_one_child(
        Child, ID, Type, AttrsIn, Colls,
        Attrs1, UpdatedChild, Hints1
    ),
    tick_parallel_children(
        RestChildren, ID, Type, Attrs1, Colls,
        AttrsOut, RestUpdated, Hints2
    ),
    append(Hints1, Hints2, AllHints).

% ============================================================================
% tick_one_child/7
% ============================================================================

:- pred tick_one_child(?Child, ?ID, ?Type, ?AttrsIn, ?Colls, ?AttrsOut, ?UpdatedChild, ?Hints).

tick_one_child(done, _, _, A, _, A, done, []) :- !.

tick_one_child(
    caused_despawn,
    _, _, A, _, A, caused_despawn, []
) :- !.

tick_one_child(Child, ID, Type, AIn, C, AOut, U, H) :-
    execute_action(
        Child,
        game_object(ID, Type, attrs(AIn), [Child], C),
        Result,
        H
    ),
    ( Result = despawned ->
        U = caused_despawn,
        AOut = AIn
    ;
        Result = game_object(_, _, attrs(AOut), NewActs, _),
        ( NewActs = [] -> U = done ; [U|_] = NewActs )
    ).

% ============================================================================
% all_children_done/1
% ============================================================================

:- pred all_children_done(?Children).

all_children_done([]).

all_children_done([done|R]) :-
    all_children_done(R).

all_children_done([caused_despawn|_]) :-
    !, fail.

% ============================================================================
% Helper: Integer division using CLPFD constraints (truncated division)
% Shifts to positive space, solves, then shifts back
% ============================================================================

solve_shifted(Diff, Frames, Step) :-
    Offset = 200, % A constant large enough to make the math positive
    domain([PosStep], 0, 1000),
    OffsetTerm #= Offset * Frames,
    ShiftedDiff #= Diff + OffsetTerm,
    PosStepNext #= PosStep + 1,
    PosStep * Frames      #=< ShiftedDiff,
    PosStepNext * Frames  #>  ShiftedDiff,
    Step #= PosStep - Offset.

% Helper: Compute new position after moving towards target
% Pure constraint logic - no labeling (labeling done at boundary in execute_action)
compute_move_position(TargetX, TargetY, CurrentX, CurrentY, Frames, NewX, NewY) :-
    DiffX #= TargetX - CurrentX,
    DiffY #= TargetY - CurrentY,
    solve_shifted(DiffX, Frames, DX),
    solve_shifted(DiffY, Frames, DY),
    NewX #= CurrentX + DX,
    NewY #= CurrentY + DY.

% ============================================================================
% Tests
% ============================================================================

% ----------------------------------------------------------------------------
% Tests: move_to
% ----------------------------------------------------------------------------

:- test execute_action(Action, ObjIn, ObjOut, Hints) : (
        Action = move_to(10, 20, 3),
        ObjIn = game_object(
            id1,
            static,
            attrs([pos(0, 0)]),
            [move_to(10, 20, 3)],
            []
        )
    ) => (
        ObjOut = game_object(
            id1,
            static,
            attrs([pos(NewX, NewY)|_]),
            [move_to(10, 20, 2)|_],
            []
        ),
        NewX = 3,
        NewY = 6,
        Hints = []
    )
    # "move_to: positive direction, multiple frames remaining".

:- test execute_action(Action, ObjIn, ObjOut, Hints) :
    (Action = move_to(0, 0, 3),
     ObjIn = game_object(id1, static, attrs([pos(10, 20)]), [move_to(0, 0, 3)], []))
    => (ObjOut = game_object(id1, static, attrs([pos(NewX, NewY)|_]), [move_to(0, 0, 2)|_], []),
        NewX = 7, NewY = 14,
        Hints = [])
    # "move_to: negative direction, multiple frames remaining".

:- test execute_action(Action, ObjIn, ObjOut, Hints) :
    (Action = move_to(5, 5, 1),
     ObjIn = game_object(id1, static, attrs([pos(0, 0)]), [move_to(5, 5, 1)], []))
    => (ObjOut = game_object(id1, static, attrs([pos(5, 5)|_]), [], []),
        Hints = [])
    # "move_to: single frame, arrives at target".

:- test execute_action(Action, ObjIn, ObjOut, Hints) : (
        Action = move_to(10, 20, 3),
        ObjIn = game_object(id1, static, attrs([pos(10, 20)]), [move_to(10, 20, 3)], [])
    ) => (
        ObjOut = game_object(id1, static, attrs([pos(10, 20)|_]), [move_to(10, 20, 2)|_], []),
        Hints = []
    )
    # "move_to: already at target, stays at position and continues with remaining frames".

:- test execute_action(Action, ObjIn, ObjOut, Hints) : (
        Action = move_to(-5, -10, 2),
        ObjIn = game_object(id1, static, attrs([pos(0, 0)]), [move_to(-5, -10, 2)], [])
    ) => (
        ObjOut = game_object(id1, static, attrs([pos(NewX, NewY)|_]), [move_to(-5, -10, 1)|_], []),
        NewX = -2, NewY = -5,
        Hints = []
    )
    # "move_to: negative target coordinates".

:- test execute_action(Action, ObjIn, ObjOut, Hints) :
    (Action = move_to(TargetX, TargetY, 1),
     ObjIn = game_object(id1, static, attrs([pos(0, 0)]), [move_to(TargetX, TargetY, 1)], []),
     ObjOut = game_object(id1, static, attrs([pos(5, 5)|_]), [], []),
     Hints = [])
    => (TargetX = 5, TargetY = 5)
    + try_sols(1)
    # "move_to: backward test - can infer target coordinates from final position".

% ----------------------------------------------------------------------------
% Tests: wait_frames
% ----------------------------------------------------------------------------

:- test execute_action(Action, ObjIn, ObjOut, Hints) :
    (Action = wait_frames(N),
     ObjIn = game_object(id1, static, attrs([]), [wait_frames(N)], []),
     ObjOut = game_object(id1, static, attrs([]), [wait_frames(2)], []),
     Hints = [])
    => (N = 3)
    + try_sols(1)
    # "wait_frames: backward test - can infer initial frame count from remaining frames".

% ----------------------------------------------------------------------------
% Tests: spawn
% ----------------------------------------------------------------------------

:- test execute_action(Action, ObjIn, ObjOut, Hints) :
    (Action = spawn(Type, Pos, Actions),
     ObjIn = game_object(id1, static, attrs([]), [spawn(Type, Pos, Actions)], []),
     ObjOut = game_object(id1, static, attrs([]), [], []),
     Hints = [spawn_request(enemy, pos(10, 5), [move_to(0, 0, 10)])])
    => (Type = enemy, Pos = pos(10, 5), Actions = [move_to(0, 0, 10)])
    + try_sols(1)
    # "spawn: backward test - can infer spawn parameters from spawn_request hint".

% ----------------------------------------------------------------------------
% Tests: trigger_state_change
% ----------------------------------------------------------------------------

:- test execute_action(Action, ObjIn, ObjOut, Hints) :
    (Action = trigger_state_change(Change),
     ObjIn = game_object(id1, static, attrs([]), [trigger_state_change(Change)], []),
     ObjOut = game_object(id1, static, attrs([]), [], []),
     Hints = [state_change(score(10))])
    => (Change = score(10))
    + try_sols(1)
    # "trigger_state_change: backward test - can infer change from state_change hint".

% ----------------------------------------------------------------------------
% Tests: loop
% ----------------------------------------------------------------------------

:- test execute_action(Action, ObjIn, ObjOut, Hints) :
    (Action = loop(Actions),
     ObjIn = game_object(id1, static, attrs([]), [loop(Actions)], []),
     ObjOut = game_object(id1, static, attrs([]), [move_to(5, 5, 1), loop([move_to(5, 5, 1)])], []),
     Hints = [])
    => (Actions = [move_to(5, 5, 1)])
    + try_sols(1)
    # "loop: backward test - can infer looped actions from final action list".

