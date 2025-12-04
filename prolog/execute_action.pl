% Action Execution Module
% Handles execution of all game actions

:- module(execute_action, [
    execute_action/5
]).

:- use_module(library(clpz)).
:- use_module(library(lists), [
    append/2,
    append/3,
    select/3,
    member/2
]).

% ==========================================================
% execute_action/5
% ==========================================================

% Forward execution (normal game)
% :- pred execute_action(+Action, +ObjIn, -ObjOut,
%     -Commands, -RevHints) 
%     :: (action(Action), game_object(ObjIn)) 
%     => (action(Action), game_object(ObjOut),
%     list(command, Commands), list(rev_hint, RevHints))
%     + is_det.

% Reverse execution (undo/replay)
% :- pred execute_action(-Action, -ObjIn, +ObjOut,
%     +Commands, +RevHints) + is_det.

% Action inference - which must the action have been?
% :- pred execute_action(-Action, +ObjIn, +ObjOut,
%     +Commands, +RevHints).

% Validation (consistency check)
% :- pred execute_action(+Action, +ObjIn, +ObjOut,
%     +Commands, +RevHints).

% ----------------------------------------------------------
% Basic Actions: wait_frames
% ----------------------------------------------------------

execute_action(
    wait_frames(N),
    game_object(ID, Type, attrs(Attrs), [_|Rest], Colls),
    game_object(ID, Type, attrs(Attrs), NewActions, Colls),
    [],
    []
) :-
    ( N #> 1 ->
        N1 #= N - 1,
        NewActions = [wait_frames(N1)|Rest]
    ;
        % N = 1, wait is done
        NewActions = Rest
    ).

% ----------------------------------------------------------
% Basic Actions: move_to
% ----------------------------------------------------------

execute_action(
    move_to(TargetX, TargetY, Frames),
    game_object(ID, Type, attrs(Attrs), [_|Rest], Colls),
    game_object(
        ID, Type, attrs(NewAttrs), NewActions, Colls
    ),
    [],
    []
) :-
    select(pos(CurrentX, CurrentY), Attrs, RestAttrs),
    % Compute step using integer division
    DX #= (TargetX - CurrentX) // Frames,
    DY #= (TargetY - CurrentY) // Frames,
    NewX #= CurrentX + DX,
    NewY #= CurrentY + DY,
    % Label at the boundary where we need ground values for
    % game objects
    (ground(TargetX), ground(TargetY), ground(CurrentX),
     ground(CurrentY), ground(Frames) ->
        labeling([], [NewX, NewY])
    ;
        true
    ),
    NewAttrs = [pos(NewX, NewY)|RestAttrs],
    ( Frames #> 1 ->
        Frames1 #= Frames - 1,
        NewActions = [move_to(TargetX, TargetY, Frames1)|
            Rest]
    ;
        NewActions = Rest  % Arrived
    ).

% ----------------------------------------------------------
% Basic Actions: despawn
% ----------------------------------------------------------

execute_action(
    despawn,
    game_object(ID, _Type, attrs(Attrs), _, _Colls),
    despawned,
    [],
    [despawned(ID, Attrs)]
) :- !.

% ----------------------------------------------------------
% Compound Actions: spawn (from Addendum 3 - FINAL version)
% ----------------------------------------------------------

execute_action(
    spawn(Type, Pos, Actions),
    game_object(ID, ObjType, attrs(Attrs), [_|Rest], Colls),
    game_object(ID, ObjType, attrs(Attrs), Rest, Colls),
    [spawn_request(Type, Pos, Actions)],
    []
).

% ----------------------------------------------------------
% Compound Actions: loop
% ----------------------------------------------------------

execute_action(
    loop(Actions),
    game_object(ID, Type, attrs(Attrs), [_|Rest], Colls),
    game_object(ID, Type, attrs(Attrs), NewActions, Colls),
    [],
    []
) :-
    append(Actions, [loop(Actions)], Expanded),
    append(Expanded, Rest, NewActions).

% ----------------------------------------------------------
% Compound Actions: trigger_state_change
% ----------------------------------------------------------

execute_action(
    trigger_state_change(Change),
    game_object(ID, Type, attrs(Attrs), [_|Rest], Colls),
    game_object(ID, Type, attrs(Attrs), Rest, Colls),
    [state_change(Change)],
    []
).

% ----------------------------------------------------------
% Compound Actions: parallel (from Addendum 2 + 3 -
% FINAL version)
% ----------------------------------------------------------

execute_action(
    parallel(ChildActions),
    game_object(ID, Type, attrs(AttrsIn), [_|Rest], Colls),
    Result,
    AllCommands,
    AllRevHints
) :-
    tick_parallel_children(
        ChildActions, ID, Type, AttrsIn, Colls,
        AttrsOut, UpdatedChildren, AllCommands, AllRevHints
    ),
    ( member(caused_despawn, UpdatedChildren) ->
        Result = despawned
    ; all_children_done(UpdatedChildren) ->
        Result = game_object(
            ID, Type, attrs(AttrsOut), Rest, Colls
        )
    ;
        NewActions = [parallel_running(UpdatedChildren)|
            Rest],
        Result = game_object(
            ID, Type, attrs(AttrsOut), NewActions, Colls
        )
    ).

% --------------------------------------------------------
% Compound Actions: parallel_running
% --------------------------------------------------------

execute_action(
    parallel_running(Children),
    Obj, NewObj, Commands, RevHints
) :-
    execute_action(
        parallel(Children), Obj, NewObj, Commands, RevHints
    ).

% ==========================================================
% tick_parallel_children/9
% ==========================================================

% TODO: Tighten type
% :- pred tick_parallel_children(?Children, ?ID, ?Type,
%     ?AttrsIn, ?Colls, ?AttrsOut, ?UpdatedChildren,
%     ?AllCommands, ?AllRevHints).

tick_parallel_children(
    [], _ID, _Type, Attrs, _Colls,
    Attrs, [], [], []
).

tick_parallel_children(
    [Child|RestChildren],
    ID, Type, AttrsIn, Colls,
    AttrsOut, [UpdatedChild|RestUpdated],
    AllCommands, AllRevHints
) :-
    tick_one_child(
        Child, ID, Type, AttrsIn, Colls,
        Attrs1, UpdatedChild, Commands1, RevHints1
    ),
    tick_parallel_children(
        RestChildren, ID, Type, Attrs1, Colls,
        AttrsOut, RestUpdated, Commands2, RevHints2
    ),
    append(Commands1, Commands2, AllCommands),
    append(RevHints1, RevHints2, AllRevHints).

% ==========================================================
% tick_one_child/9
% ==========================================================

% TOODO: Tighten type
% :- pred tick_one_child(?Child, ?ID, ?Type, ?AttrsIn,
%     ?Colls, ?AttrsOut, ?UpdatedChild, ?Commands,
%     ?RevHints).

tick_one_child(done, _, _, A, _, A, done, [], []) :- !.

tick_one_child(
    caused_despawn,
    _, _, A, _, A, caused_despawn, [], []
) :- !.

tick_one_child(
    Child, ID, Type, AIn, C, AOut, U, Commands, RevHints
) :-
    execute_action(
        Child,
        game_object(ID, Type, attrs(AIn), [Child], C),
        Result,
        Commands,
        RevHints
    ),
    ( Result = despawned ->
        U = caused_despawn,
        AOut = AIn
    ;
        Result = game_object(_, _, attrs(AOut), NewActs, _),
        ( NewActs = [] -> U = done ; [U|_] = NewActs )
    ).

% ==========================================================
% all_children_done/1
% ==========================================================

% :- pred all_children_done(?Children).

all_children_done([]).

all_children_done([done|R]) :-
    all_children_done(R).

all_children_done([caused_despawn|_]) :-
    !, fail.

% ==========================================================
% Tests
% ==========================================================

% --------------------------------------------------------
% Tests: move_to
% ----------------------------------------------------------

test("move_to: positive direction, multiple frames \
remaining", (
    Action = move_to(10, 20, 3),
    ObjIn = game_object(
        1,
        static,
        attrs([pos(0, 0)]),
        [move_to(10, 20, 3)],
        []
    ),
    execute_action(
        Action, ObjIn, ObjOut, Commands, RevHints
    ),
    ObjOut = game_object(
        1,
        static,
        attrs([pos(NewX, NewY)|_]),
        [move_to(10, 20, 2)|_],
        []
    ),
    NewX = 3,
    NewY = 6,
    Commands = [],
    RevHints = []
)).

test("move_to: negative direction, multiple frames \
remaining", (
    Action = move_to(0, 0, 3),
    ObjIn = game_object(
        1,
        static,
        attrs([pos(10, 20)]),
        [move_to(0, 0, 3)],
        []
    ),
    execute_action(
        Action, ObjIn, ObjOut, Commands, RevHints
    ),
    ObjOut = game_object(
        1,
        static,
        attrs([pos(NewX, NewY)|_]),
        [move_to(0, 0, 2)|_],
        []
    ),
    NewX = 7,
    NewY = 14,
    Commands = [],
    RevHints = []
)).

test("move_to: single frame, arrives at target", (
    Action = move_to(5, 5, 1),
    ObjIn = game_object(
        1,
        static,
        attrs([pos(0, 0)]),
        [move_to(5, 5, 1)],
        []
    ),
    execute_action(
        Action, ObjIn, ObjOut, Commands, RevHints
    ),
    ObjOut = game_object(
        1, static, attrs([pos(5, 5)|_]), [], []
    ),
    Commands = [],
    RevHints = []
)).

test("move_to: already at target, stays at position and \
continues with remaining frames", (
    Action = move_to(10, 20, 3),
    ObjIn = game_object(
        1,
        static,
        attrs([pos(10, 20)]),
        [move_to(10, 20, 3)],
        []
    ),
    execute_action(
        Action, ObjIn, ObjOut, Commands, RevHints
    ),
    ObjOut = game_object(
        1,
        static,
        attrs([pos(10, 20)|_]),
        [move_to(10, 20, 2)|_],
        []
    ),
    Commands = [],
    RevHints = []
)).

test("move_to: negative target coordinates", (
    Action = move_to(-5, -10, 2),
    ObjIn = game_object(
        1,
        static,
        attrs([pos(0, 0)]),
        [move_to(-5, -10, 2)],
        []
    ),
    execute_action(
        Action, ObjIn, ObjOut, Commands, RevHints
    ),
    ObjOut = game_object(
        1,
        static,
        attrs([pos(NewX, NewY)|_]),
        [move_to(-5, -10, 1)|_],
        []
    ),
    NewX = -2,
    NewY = -5,
    Commands = [],
    RevHints = []
)).

test("move_to: backward test - can infer target \
coordinates from final position", (
    % TargetX and TargetY are unknown - should be inferred
    Action = move_to(TargetX, TargetY, 1),
    ObjIn = game_object(
        1,
        static,
        attrs([pos(0, 0)]),
        [move_to(TargetX, TargetY, 1)],
        []
    ),
    ObjOut = game_object(
        1, static, attrs([pos(5, 5)|_]), [], []
    ),
    Commands = [],
    RevHints = [],
    execute_action(
        Action, ObjIn, ObjOut, Commands, RevHints
    ),
    % Verify that TargetX was correctly inferred
    TargetX = 5,
    % Verify that TargetY was correctly inferred
    TargetY = 5
)).

% --------------------------------------------------------
% Tests: wait_frames
% --------------------------------------------------------

% Scryer Prolog test format - backward test: infer N from
% output
test("wait_frames: backward test - can infer initial frame \
count from remaining frames", (
    % N is unknown - should be inferred
    Action = wait_frames(N),
    ObjIn = game_object(
        1, static, attrs([]), [wait_frames(N)], []
    ),
    ObjOut = game_object(
        1, static, attrs([]), [wait_frames(2)], []
    ),
    Commands = [],
    RevHints = [],
    execute_action(
        Action, ObjIn, ObjOut, Commands, RevHints
    ),
    N = 3  % Verify that N was correctly inferred
)).

% --------------------------------------------------------
% Tests: spawn
% --------------------------------------------------------

test("spawn: backward test - can infer spawn \
parameters from spawn_request command", (
    % Type, Pos, and Actions are unknown - should be
    % inferred
    Action = spawn(Type, Pos, Actions),
    ObjIn = game_object(
        1,
        static,
        attrs([]),
        [spawn(Type, Pos, Actions)],
        []
    ),
    ObjOut = game_object(1, static, attrs([]), [], []),
    Commands = [spawn_request(
        enemy, pos(10, 5), [move_to(0, 0, 10)]
    )],
    RevHints = [],
    execute_action(
        Action, ObjIn, ObjOut, Commands, RevHints
    ),
    % Verify that Type was correctly inferred
    Type = enemy,
    % Verify that Pos was correctly inferred
    Pos = pos(10, 5),
    % Verify that Actions were correctly inferred
    Actions = [move_to(0, 0, 10)]
)).

% --------------------------------------------------------
% Tests: trigger_state_change
% --------------------------------------------------------

test("trigger_state_change: backward test - can infer \
change from state_change command", (
    % Change is unknown - should be inferred
    Action = trigger_state_change(Change),
    ObjIn = game_object(
        1,
        static,
        attrs([]),
        [trigger_state_change(Change)],
        []
    ),
    ObjOut = game_object(1, static, attrs([]), [], []),
    Commands = [state_change(score(10))],
    RevHints = [],
    execute_action(
        Action, ObjIn, ObjOut, Commands, RevHints
    ),
    % Verify that Change was correctly inferred
    Change = score(10)
)).

% --------------------------------------------------------
% Tests: loop
% --------------------------------------------------------

test("loop: backward test - can infer looped actions from \
final action list", (
    % Actions is unknown - should be inferred
    Action = loop(Actions),
    ObjIn = game_object(
        1, static, attrs([]), [loop(Actions)], []
    ),
    ObjOut = game_object(
        1,
        static,
        attrs([]),
        [move_to(5, 5, 1), loop([move_to(5, 5, 1)])],
        []
    ),
    Commands = [],
    RevHints = [],
    execute_action(
        Action, ObjIn, ObjOut, Commands, RevHints
    ),
    % Verify that Actions were correctly inferred
    Actions = [move_to(5, 5, 1)]
)).

% ==========================================================
% Tests: despawn
% ==========================================================

test("despawn: backward test - can infer ID and attributes \
from despawned rev_hint", (
    Action = despawn,
    % ID and Attrs are unknown - should be inferred
    ObjIn = game_object(ID, static, attrs(Attrs), [], []),
    ObjOut = despawned,
    Commands = [],
    RevHints = [despawned(1, [pos(10, 20)])],
    execute_action(
        Action, ObjIn, ObjOut, Commands, RevHints
    ),
    ID = 1,  % Verify that ID was correctly inferred
    % Verify that Attrs were correctly inferred
    Attrs = [pos(10, 20)]
)).

% ==========================================================
% Tests: parallel
% ==========================================================

test("parallel: backward test - can infer original \
wait_frames values from parallel_running state", (
    % N1 and N2 are unknown - should be inferred
    Action = parallel([wait_frames(N1), wait_frames(N2)]),
    ObjIn = game_object(
        1,
        static,
        attrs([]),
        [parallel([wait_frames(N1), wait_frames(N2)])],
        []
    ),
    ObjOut = game_object(
        1,
        static,
        attrs([]),
        [parallel_running([
            wait_frames(2),
            wait_frames(3)
        ])],
        []
    ),
    Commands = [],
    RevHints = [],
    execute_action(
        Action, ObjIn, ObjOut, Commands, RevHints
    ),
    N1 = 3,  % Verify that N1 was correctly inferred
    N2 = 4   % Verify that N2 was correctly inferred
)).

