% Action Execution Module
% Handles execution of all game actions

:- module(execute_action, [
    execute_action/6
]).

:- use_module(library(clpz)).
:- use_module(library(lists), [
    append/2,
    append/3,
    select/3,
    member/2
]).
:- use_module('./types/validation', [action_validation/1]).

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

% Wrapper: validates action then delegates to implementation
execute_action(
    ctx_in(Ctx), Action, ObjIn, ObjOut, Commands, RevHints
) :-
    action_validation(Action),
    execute_action_impl(
        ctx_in(Ctx),
        Action,
        ObjIn,
        ObjOut,
        Commands,
        RevHints
    ).

% ==========================================================
% execute_action_impl/5
% ==========================================================
% Internal implementation (no validation)
% Multifile: some clauses are defined in parallel.pl

:- multifile(execute_action_impl/6).

% Load parallel.pl after this module loads to make multifile
% clauses available. This is necessary because:
% 1. parallel.pl defines execute_action:execute_action_impl
%    clauses
% 2. Tests load execute_action.pl directly, so parallel.pl
%    won't be loaded automatically
% 3. We use initialization (runs after file loads) instead
%    of use_module to avoid circular dependency (parallel.pl
%    imports execute_action)
% 4. parallel.pl uses module qualification
%    (execute_action:execute_action) to call execute_action,
%    avoiding the need to import it
:- initialization(use_module('./actions/parallel', [])).

% ----------------------------------------------------------
% Basic Actions: wait_frames
% ----------------------------------------------------------

execute_action_impl(
    ctx_in(_Ctx),
    wait_frames(N),
    object(
        id(ID),
        type(Type),
        attrs(Attrs),
        actions([_|Rest]),
        collisions(Colls)
    ),
    [object(
        id(ID),
        type(Type),
        attrs(Attrs),
        actions(NewActions),
        collisions(Colls)
    )],
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

execute_action_impl(
    ctx_in(_Ctx),
    move_to(TargetX, TargetY, Frames),
    object(
        id(ID),
        type(Type),
        attrs(Attrs),
        actions([_|Rest]),
        Colls
    ),
    [object(
        id(ID),
        type(Type),
        attrs(NewAttrs),
        actions(NewActions),
        Colls
    )],
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

execute_action_impl(
    ctx_in(_Ctx),
    despawn,
    object(
        id(ID),
        type(_Type),
        attrs(Attrs),
        actions(_),
        collisions(_Colls)
    ),
    [],
    [],
    [despawned(ID, Attrs)]
) :- !.

% ----------------------------------------------------------
% Compound Actions: spawn (from Addendum 3 - FINAL version)
% ----------------------------------------------------------

execute_action_impl(
    ctx_in(_Ctx),
    spawn(Type, Pos, Actions),
    object(
        id(ID),
        type(ObjType),
        attrs(Attrs),
        actions([_|Rest]),
        collisions(Colls)
    ),
    [object(
        id(ID),
        type(ObjType),
        attrs(Attrs),
        actions(Rest),
        collisions(Colls)
    )],
    [spawn_request(Type, Pos, Actions)],
    []
).

% ----------------------------------------------------------
% Compound Actions: loop
% ----------------------------------------------------------

execute_action_impl(
    ctx_in(_Ctx),
    loop(Actions),
    object(
        id(ID),
        type(Type),
        attrs(Attrs),
        actions([_|Rest]),
        collisions(Colls)
    ),
    [object(
        id(ID),
        type(Type),
        attrs(Attrs),
        actions(NewActions),
        collisions(Colls)
    )],
    [],
    []
) :-
    append(Actions, [loop(Actions)], Expanded),
    append(Expanded, Rest, NewActions).

% ----------------------------------------------------------
% Compound Actions: trigger_state_change
% ----------------------------------------------------------

execute_action_impl(
    ctx_in(_Ctx),
    trigger_state_change(Change),
    object(
        id(ID),
        type(Type),
        attrs(Attrs),
        actions([_|Rest]),
        Colls
    ),
    [object(
        id(ID),
        type(Type),
        attrs(Attrs),
        actions(Rest),
        Colls
    )],
    [state_change(Change)],
    []
).

% ----------------------------------------------------------
% Compound Actions: parallel
% ----------------------------------------------------------
% Note: parallel and parallel_running actions are handled
% in prolog/actions/parallel.pl via multifile clauses

% ==========================================================
% Tests
% ==========================================================

% --------------------------------------------------------
% Tests: move_to
% ----------------------------------------------------------

test("move_to: positive direction, multiple frames \
remaining", (
    Action = move_to(10, 20, 3),
    ObjIn = object(
        id(1),
        type(static),
        attrs([pos(0, 0)]),
        actions([move_to(10, 20, 3)]),
        []
    ),
    Ctx = ctx(state(
        frame(0),
        [],
        status(playing),
        score(0),
        next_id(1),
        [],
        []
    )),
    execute_action(
        ctx_in(Ctx),
        Action,
        ObjIn,
        ObjOut,
        Commands,
        RevHints
    ),
    ObjOut = [object(
        id(1),
        type(static),
        attrs([pos(NewX, NewY)|_]),
        actions([move_to(10, 20, 2)|_]),
        []
    )],
    NewX = 3,
    NewY = 6,
    Commands = [],
    RevHints = []
)).

test("move_to: negative direction, multiple frames \
remaining", (
    Action = move_to(0, 0, 3),
    ObjIn = object(
        id(1),
        type(static),
        attrs([pos(10, 20)]),
        actions([move_to(0, 0, 3)]),
        []
    ),
    Ctx = ctx(state(
        frame(0),
        [],
        status(playing),
        score(0),
        next_id(1),
        [],
        []
    )),
    execute_action(
        ctx_in(Ctx),
        Action,
        ObjIn,
        ObjOut,
        Commands,
        RevHints
    ),
    ObjOut = [object(
        id(1),
        type(static),
        attrs([pos(NewX, NewY)|_]),
        actions([move_to(0, 0, 2)|_]),
        []
    )],
    NewX = 7,
    NewY = 14,
    Commands = [],
    RevHints = []
)).

test("move_to: single frame, arrives at target", (
    Action = move_to(5, 5, 1),
    ObjIn = object(
        id(1),
        type(static),
        attrs([pos(0, 0)]),
        actions([move_to(5, 5, 1)]),
        []
    ),
    Ctx = ctx(state(
        frame(0),
        [],
        status(playing),
        score(0),
        next_id(1),
        [],
        []
    )),
    execute_action(
        ctx_in(Ctx),
        Action,
        ObjIn,
        ObjOut,
        Commands,
        RevHints
    ),
    ObjOut = [object(
        id(1),
        type(static),
        attrs([pos(5, 5)|_]),
        actions([]),
        []
    )],
    Commands = [],
    RevHints = []
)).

test("move_to: already at target, stays at position and \
continues with remaining frames", (
    Action = move_to(10, 20, 3),
    ObjIn = object(
        id(1),
        type(static),
        attrs([pos(10, 20)]),
        actions([move_to(10, 20, 3)]),
        []
    ),
    Ctx = ctx(state(
        frame(0),
        [],
        status(playing),
        score(0),
        next_id(1),
        [],
        []
    )),
    execute_action(
        ctx_in(Ctx),
        Action,
        ObjIn,
        ObjOut,
        Commands,
        RevHints
    ),
    ObjOut = [object(
        id(1),
        type(static),
        attrs([pos(10, 20)|_]),
        actions([move_to(10, 20, 2)|_]),
        []
    )],
    Commands = [],
    RevHints = []
)).

test("move_to: negative target coordinates", (
    Action = move_to(-5, -10, 2),
    ObjIn = object(
        id(1),
        type(static),
        attrs([pos(0, 0)]),
        actions([move_to(-5, -10, 2)]),
        []
    ),
    Ctx = ctx(state(
        frame(0),
        [],
        status(playing),
        score(0),
        next_id(1),
        [],
        []
    )),
    execute_action(
        ctx_in(Ctx),
        Action,
        ObjIn,
        ObjOut,
        Commands,
        RevHints
    ),
    ObjOut = [object(
        id(1),
        type(static),
        attrs([pos(NewX, NewY)|_]),
        actions([move_to(-5, -10, 1)|_]),
        []
    )],
    NewX = -2,
    NewY = -5,
    Commands = [],
    RevHints = []
)).

test("move_to: backward test - can infer target \
coordinates from final position", (
    % TargetX and TargetY are unknown - should be inferred
    Action = move_to(TargetX, TargetY, 1),
    ObjIn = object(
        id(1),
        type(static),
        attrs([pos(0, 0)]),
        actions([move_to(TargetX, TargetY, 1)]),
        []
    ),
    ObjOut = [object(
        id(1),
        type(static),
        attrs([pos(5, 5)|_]),
        actions([]),
        []
    )],
    Commands = [],
    RevHints = [],
    Ctx = ctx(state(
        frame(0),
        [],
        status(playing),
        score(0),
        next_id(1),
        [],
        []
    )),
    execute_action(
        ctx_in(Ctx),
        Action,
        ObjIn,
        ObjOut,
        Commands,
        RevHints
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
    ObjIn = object(
        id(1),
        type(static),
        attrs([]),
        actions([wait_frames(N)]),
        collisions([])
    ),
    ObjOut = [object(
        id(1),
        type(static),
        attrs([]),
        actions([wait_frames(2)]),
        collisions([])
    )],
    Commands = [],
    RevHints = [],
    Ctx = ctx(state(
        frame(0),
        [],
        status(playing),
        score(0),
        next_id(1),
        [],
        []
    )),
    execute_action(
        ctx_in(Ctx),
        Action,
        ObjIn,
        ObjOut,
        Commands,
        RevHints
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
    ObjIn = object(
        id(1),
        type(static),
        attrs([]),
        actions([
            spawn(Type, Pos, Actions)
        ]),
        collisions([])
    ),
    ObjOut = [object(
        id(1),
        type(static),
        attrs([]),
        actions([]),
        collisions([])
    )],
    Commands = [spawn_request(
        enemy, pos(10, 5), [move_to(0, 0, 10)]
    )],
    RevHints = [],
    Ctx = ctx(state(
        frame(0),
        [],
        status(playing),
        score(0),
        next_id(1),
        [],
        []
    )),
    execute_action(
        ctx_in(Ctx),
        Action,
        ObjIn,
        ObjOut,
        Commands,
        RevHints
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
    ObjIn = object(
        id(1),
        type(static),
        attrs([]),
        actions([trigger_state_change(Change)]),
        collisions([])
    ),
    ObjOut = [object(
        id(1),
        type(static),
        attrs([]),
        actions([]),
        collisions([])
    )],
    Commands = [state_change(score(10))],
    RevHints = [],
    Ctx = ctx(state(
        frame(0),
        [],
        status(playing),
        score(0),
        next_id(1),
        [],
        []
    )),
    execute_action(
        ctx_in(Ctx),
        Action,
        ObjIn,
        ObjOut,
        Commands,
        RevHints
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
    ObjIn = object(
        id(1),
        type(static),
        attrs([]),
        actions([loop(Actions)]),
        collisions([])
    ),
    ObjOut = [object(
        id(1),
        type(static),
        attrs([]),
        actions([
            move_to(5, 5, 1),
            loop([move_to(5, 5, 1)])
        ]),
        collisions([])
    )],
    Commands = [],
    RevHints = [],
    Ctx = ctx(state(
        frame(0),
        [],
        status(playing),
        score(0),
        next_id(1),
        [],
        []
    )),
    execute_action(
        ctx_in(Ctx),
        Action,
        ObjIn,
        ObjOut,
        Commands,
        RevHints
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
    ObjIn = object(
        id(ID),
        type(static),
        attrs(Attrs),
        actions([]),
        collisions([])
    ),
    ObjOut = [],
    Commands = [],
    RevHints = [despawned(1, [pos(10, 20)])],
    Ctx = ctx(state(
        frame(0),
        [],
        status(playing),
        score(0),
        next_id(1),
        [],
        []
    )),
    execute_action(
        ctx_in(Ctx),
        Action,
        ObjIn,
        ObjOut,
        Commands,
        RevHints
    ),
    ID = 1,  % Verify that ID was correctly inferred
    % Verify that Attrs were correctly inferred
    Attrs = [pos(10, 20)]
)).
