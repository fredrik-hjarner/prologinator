:- module(execute_action_test, []).
:- use_module('./execute_action', [execute_action/5]).
:- use_module('./types/accessors').
% ==========================================================
% Tests
% ==========================================================

% --------------------------------------------------------
% Tests: wait_frames
% --------------------------------------------------------

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
        objects([]),
        status(playing),
        next_id(1),
        commands([]),
        rev_hints([])
    )),
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(Action),
        obj_old(ObjIn),
        obj_new(ObjOut)
    ),
    ctx_cmds(CtxNew, []),
    ctx_revhints(CtxNew, []),
    N = 3  % Verify that N was correctly inferred
)).

% --------------------------------------------------------
% Tests: parallel
% --------------------------------------------------------

test("parallel: backward test - can infer original \
wait_frames values from parallel_running state", (
    % N1 and N2 are unknown - should be inferred
    Action = parallel([wait_frames(N1), wait_frames(N2)]),
    ObjIn = object(
        id(1),
        type(static),
        attrs([]),
        actions([
            parallel([wait_frames(N1), wait_frames(N2)])
        ]),
        collisions([])
    ),
    ObjOut = [object(
        id(1),
        type(static),
        attrs([]),
        actions([
            parallel_running([
                wait_frames(2),
                wait_frames(3)
            ])
        ]),
        collisions([])
    )],
    Commands = [],
    RevHints = [],
    Ctx = ctx(state(
        frame(0),
        objects([]),
        status(playing),
        next_id(1),
        commands([]),
        rev_hints([])
    )),
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(Action),
        obj_old(ObjIn),
        obj_new(ObjOut)
    ),
    ctx_cmds(CtxNew, Commands),
    ctx_revhints(CtxNew, RevHints),
    N1 = 3,  % Verify that N1 was correctly inferred
    N2 = 4   % Verify that N2 was correctly inferred
)).

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
        objects([]),
        status(playing),
        next_id(1),
        commands([]),
        rev_hints([])
    )),
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(Action),
        obj_old(ObjIn),
        obj_new(ObjOut)
    ),
    ctx_cmds(CtxNew, Commands),
    ctx_revhints(CtxNew, RevHints),
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
        objects([]),
        status(playing),
        next_id(1),
        commands([]),
        rev_hints([])
    )),
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(Action),
        obj_old(ObjIn),
        obj_new(ObjOut)
    ),
    ctx_cmds(CtxNew, Commands),
    ctx_revhints(CtxNew, RevHints),
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
        objects([]),
        status(playing),
        next_id(1),
        commands([]),
        rev_hints([])
    )),
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(Action),
        obj_old(ObjIn),
        obj_new(ObjOut)
    ),
    ctx_cmds(CtxNew, Commands),
    ctx_revhints(CtxNew, RevHints),
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
        objects([]),
        status(playing),
        next_id(1),
        commands([]),
        rev_hints([])
    )),
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(Action),
        obj_old(ObjIn),
        obj_new(ObjOut)
    ),
    ctx_cmds(CtxNew, Commands),
    ctx_revhints(CtxNew, RevHints),
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
        objects([]),
        status(playing),
        next_id(1),
        commands([]),
        rev_hints([])
    )),
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(Action),
        obj_old(ObjIn),
        obj_new(ObjOut)
    ),
    ctx_cmds(CtxNew, Commands),
    ctx_revhints(CtxNew, RevHints),
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
        objects([]),
        status(playing),
        next_id(1),
        commands([]),
        rev_hints([])
    )),
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(Action),
        obj_old(ObjIn),
        obj_new(ObjOut)
    ),
    ctx_cmds(CtxNew, Commands),
    ctx_revhints(CtxNew, RevHints),
    % Verify that TargetX was correctly inferred
    TargetX = 5,
    % Verify that TargetY was correctly inferred
    TargetY = 5
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
        objects([]),
        status(playing),
        next_id(1),
        commands([]),
        rev_hints([])
    )),
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(Action),
        obj_old(ObjIn),
        obj_new(ObjOut)
    ),
    ctx_cmds(CtxNew, Commands),
    ctx_revhints(CtxNew, RevHints),
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
    Commands = [state_change(game_over(won))],
    RevHints = [],
    Ctx = ctx(state(
        frame(0),
        objects([]),
        status(playing),
        next_id(1),
        commands([]),
        rev_hints([])
    )),
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(Action),
        obj_old(ObjIn),
        obj_new(ObjOut)
    ),
    ctx_cmds(CtxNew, Commands),
    ctx_revhints(CtxNew, RevHints),
    % Verify that Change was correctly inferred
    Change = game_over(won)
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
        objects([]),
        status(playing),
        next_id(1),
        commands([]),
        rev_hints([])
    )),
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(Action),
        obj_old(ObjIn),
        obj_new(ObjOut)
    ),
    ctx_cmds(CtxNew, Commands),
    ctx_revhints(CtxNew, RevHints),
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
        objects([]),
        status(playing),
        next_id(1),
        commands([]),
        rev_hints([])
    )),
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(Action),
        obj_old(ObjIn),
        obj_new(ObjOut)
    ),
    ctx_cmds(CtxNew, Commands),
    ctx_revhints(CtxNew, RevHints),
    ID = 1,  % Verify that ID was correctly inferred
    % Verify that Attrs were correctly inferred
    Attrs = [pos(10, 20)]
)).
