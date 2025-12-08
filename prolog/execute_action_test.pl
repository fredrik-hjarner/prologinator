:- module(execute_action_test, []).
:- use_module('./execute_action', [execute_action/5]).
:- use_module('./types/accessors').
:- use_module('./util/util', [err_write/1, err_format/2]).
:- use_module(library(lists), [member/2]).
% ==========================================================
% Tests
% ==========================================================

% --------------------------------------------------------
% Tests: wait
% --------------------------------------------------------

test("wait: backward test - can infer initial frame \
count from remaining frames", (
    % N is unknown - should be inferred
    Action = wait(N),
    ObjIn = object(
        id(1),
        type(static),
        attrs([]),
        actions([wait(N)]),
        collisions([])
    ),
    ObjOut = [object(
        id(1),
        type(static),
        attrs([]),
        actions([wait(2)]),
        collisions([])
    )],
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

test("parallel_all: backward test - can infer original \
wait values from parallel_all_running state", (
    % N1 and N2 are unknown - should be inferred
    Action = parallel_all([wait(N1), wait(N2)]),
    ObjIn = object(
        id(1),
        type(static),
        attrs([]),
        actions([
            parallel_all([wait(N1), wait(N2)])
        ]),
        collisions([])
    ),
    ObjOut = [object(
        id(1),
        type(static),
        attrs([]),
        actions([
            parallel_all_running([
                wait(2),
                wait(3)
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
        attrs([x(0), y(0)]),
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
        attrs([x(NewX), y(NewY)|_]),
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
        attrs([x(10), y(20)]),
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
        attrs([x(NewX), y(NewY)|_]),
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
        attrs([x(0), y(0)]),
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
        attrs([x(5), y(5)|_]),
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
        attrs([x(10), y(20)]),
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
        attrs([x(10), y(20)|_]),
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
        attrs([x(0), y(0)]),
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
        attrs([x(NewX), y(NewY)|_]),
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
        attrs([x(0), y(0)]),
        actions([move_to(TargetX, TargetY, 1)]),
        []
    ),
    ObjOut = [object(
        id(1),
        type(static),
        attrs([x(5), y(5)|_]),
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
parameters from spawned object", (
    % Type, X, Y, and Actions are unknown - should be
    % inferred
    Action = spawn(Type, X, Y, Actions),
    ObjIn = object(
        id(1),
        type(static),
        attrs([]),
        actions([
            spawn(Type, X, Y, Actions)
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
    Commands = [],
    RevHints = [],
    % For backward execution, context already has the
    % spawned object
    CtxOut = ctx(state(
        frame(0),
        objects([
            object(
                id(1), type(enemy), attrs([x(10), y(5)]),
                actions([move_to(0, 0, 10)]),
                collisions([])
            )
        ]),
        status(playing),
        next_id(2),
        commands([]),
        rev_hints([])
    )),
    CtxIn = ctx(state(
        frame(0),
        objects([]),
        status(playing),
        next_id(1),
        commands([]),
        rev_hints([])
    )),
    execute_action(
        ctx_old(CtxIn),
        ctx_new(CtxOut),
        action(Action),
        obj_old(ObjIn),
        obj_new(ObjOut)
    ),
    ctx_cmds(CtxOut, Commands),
    ctx_revhints(CtxOut, RevHints),
    % Verify that Type was correctly inferred
    Type = enemy,
    % Verify that X was correctly inferred
    X = 10,
    % Verify that Y was correctly inferred
    Y = 5,
    % Verify that Actions were correctly inferred
    Actions = [move_to(0, 0, 10)]
)).

% --------------------------------------------------------
% Tests: trigger_state_change
% --------------------------------------------------------

test("trigger_state_change: forward test - updates status \
to won", (
    Action = trigger_state_change(game_over(won)),
    ObjIn = object(
        id(1),
        type(static),
        attrs([]),
        actions([trigger_state_change(game_over(won))]),
        collisions([])
    ),
    CtxIn = ctx(state(
        frame(0),
        objects([]),
        status(playing),
        next_id(1),
        commands([]),
        rev_hints([])
    )),
    execute_action(
        ctx_old(CtxIn),
        ctx_new(CtxOut),
        action(Action),
        obj_old(ObjIn),
        obj_new(ObjOut)
    ),
    ctx_cmds(CtxOut, Commands),
    ctx_revhints(CtxOut, RevHints),
    ObjOut = [object(
        id(1),
        type(static),
        attrs([]),
        actions([]),
        collisions([])
    )],
    ctx_status(CtxOut, won),
    Commands = [],
    RevHints = []
)).

test("trigger_state_change: forward test - updates status \
to lost", (
    Action = trigger_state_change(game_over(lost)),
    ObjIn = object(
        id(1),
        type(static),
        attrs([]),
        actions([trigger_state_change(game_over(lost))]),
        collisions([])
    ),
    CtxIn = ctx(state(
        frame(0),
        objects([]),
        status(playing),
        next_id(1),
        commands([]),
        rev_hints([])
    )),
    execute_action(
        ctx_old(CtxIn),
        ctx_new(CtxOut),
        action(Action),
        obj_old(ObjIn),
        obj_new(ObjOut)
    ),
    ctx_cmds(CtxOut, Commands),
    ctx_revhints(CtxOut, RevHints),
    ObjOut = [object(
        id(1),
        type(static),
        attrs([]),
        actions([]),
        collisions([])
    )],
    ctx_status(CtxOut, lost),
    Commands = [],
    RevHints = []
)).

test("trigger_state_change: forward test - won cannot \
override lost", (
    Action = trigger_state_change(game_over(won)),
    ObjIn = object(
        id(1),
        type(static),
        attrs([]),
        actions([trigger_state_change(game_over(won))]),
        collisions([])
    ),
    CtxIn = ctx(state(
        frame(0),
        objects([]),
        status(lost),
        next_id(1),
        commands([]),
        rev_hints([])
    )),
    execute_action(
        ctx_old(CtxIn),
        ctx_new(CtxOut),
        action(Action),
        obj_old(ObjIn),
        obj_new(ObjOut)
    ),
    ctx_cmds(CtxOut, Commands),
    ctx_revhints(CtxOut, RevHints),
    ObjOut = [object(
        id(1),
        type(static),
        attrs([]),
        actions([]),
        collisions([])
    )],
    ctx_status(CtxOut, lost),
    Commands = [],
    RevHints = []
)).

test("trigger_state_change: backward test - can infer \
change from updated status", (
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
    Commands = [],
    RevHints = [],
    % For backward execution, context already has the
    % updated status
    CtxIn = ctx(state(
        frame(0),
        objects([]),
        status(playing),
        next_id(1),
        commands([]),
        rev_hints([])
    )),
    CtxOut = ctx(state(
        frame(0),
        objects([]),
        status(won),
        next_id(1),
        commands([]),
        rev_hints([])
    )),
    execute_action(
        ctx_old(CtxIn),
        ctx_new(CtxOut),
        action(Action),
        obj_old(ObjIn),
        obj_new(ObjOut)
    ),
    ctx_cmds(CtxOut, Commands),
    ctx_revhints(CtxOut, RevHints),
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
% Tests: noop
% ==========================================================

test("noop: removes self from action queue", (
    ObjIn = object(
        id(0), type(static), attrs([pos(0, 0)]),
        actions([noop, wait(1)]), collisions([])
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
        action(noop),
        obj_old(ObjIn),
        obj_new([ObjOut])
    ),
    ObjOut = object(
        id(0), type(static), attrs([pos(0, 0)]),
        actions([wait(1)]), collisions([])
    ),
    ctx_revhints(CtxNew, []),
    ctx_cmds(CtxNew, []),
    ctx_frame(CtxNew, 0),
    ctx_status(CtxNew, playing)
)).

% ==========================================================
% Tests: list
% ==========================================================

test("list: expands actions into queue", (
    ObjIn = object(
        id(0), type(static), attrs([]),
        actions([
            list([wait(1), move_to(5, 5, 2)]),
            wait(3)
        ]),
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
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(list([wait(1), move_to(5, 5, 2)])),
        obj_old(ObjIn),
        obj_new([ObjOut])
    ),
    ObjOut = object(
        id(0), type(static), attrs([]),
        actions([wait(1), move_to(5, 5, 2), wait(3)]),
        collisions([])
    ),
    ctx_revhints(CtxNew, []),
    ctx_cmds(CtxNew, []),
    ctx_frame(CtxNew, 0),
    ctx_status(CtxNew, playing)
)).

test("list: empty list removes itself", (
    ObjIn = object(
        id(0), type(static), attrs([]),
        actions([list([]), wait(1)]),
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
    execute_action(
        ctx_old(Ctx),
        ctx_new(_),
        action(list([])),
        obj_old(ObjIn),
        obj_new([ObjOut])
    ),
    ObjOut = object(
        id(0), type(static), attrs([]),
        actions([wait(1)]),
        collisions([])
    )
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
    RevHints = [despawned(1, [x(10), y(20)])],
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
    Attrs = [x(10), y(20)]
)).

test("despawn: despawns object and prevents remaining \
actions from executing", (
    ObjIn = object(
        id(1),
        type(static),
        attrs([x(0), y(0)]),
        actions([
            despawn,
            wait(5),
            move_to(10, 10, 3),
            set_attr(hp, 100)
        ]),
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
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(despawn),
        obj_old(ObjIn),
        obj_new(ObjOut)
    ),
    % Object must be despawned (empty list)
    ObjOut = [],
    % Despawn hint must be recorded
    ctx_revhints(CtxNew, [despawned(1, [x(0), y(0)])]),
    % No commands
    ctx_cmds(CtxNew, []),
    % Context frame unchanged
    ctx_frame(CtxNew, 0)
)).

test("despawn: prevents game_over state change from \
executing after despawn", (
    ObjIn = object(
        id(1),
        type(static),
        attrs([]),
        actions([
            despawn,
            trigger_state_change(game_over(won)),
            wait(5)
        ]),
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
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(despawn),
        obj_old(ObjIn),
        obj_new(ObjOut)
    ),
    % Object must be despawned (empty list)
    ObjOut = [],
    % Despawn hint must be recorded
    ctx_revhints(CtxNew, [despawned(1, [])]),
    % Status must remain playing (game_over did NOT execute)
    ctx_status(CtxNew, playing),
    % No commands
    ctx_cmds(CtxNew, [])
)).

% ==========================================================
% Tests: set_attr
% ==========================================================

test("set_attr: set new attribute", (
    ObjIn = object(
        id(1), type(enemy), attrs([x(5), y(10)]),
        actions([set_attr(hp, 100)]), collisions([])
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
        action(set_attr(hp, 100)),
        obj_old(ObjIn),
        obj_new([ObjOut])
    ),
    ObjOut = object(
        id(1), type(enemy), attrs([hp(100), x(5), y(10)]),
        actions([]), collisions([])
    ),
    ctx_revhints(CtxNew, []),
    ctx_cmds(CtxNew, [])
)).

test("set_attr: replace existing attribute", (
    ObjIn = object(
        id(1), type(enemy), attrs([hp(100), x(5), y(10)]),
        actions([set_attr(hp, 50)]), collisions([])
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
        ctx_new(_),
        action(set_attr(hp, 50)),
        obj_old(ObjIn),
        obj_new([ObjOut])
    ),
    ObjOut = object(
        id(1), type(enemy), attrs([hp(50), x(5), y(10)]),
        actions([]), collisions([])
    )
)).

% ==========================================================
% Tests: parallel_race
% ==========================================================

test("parallel_race: stops on child completion", (
    Obj = object(
        id(0), type(tower), attrs([]),
        actions([
            parallel_race([
                wait(5),
                noop,
                wait(10)
            ]),
            wait(3)
        ]),
        collisions([])
    ),
    Ctx = ctx(state(
        frame(0),
        objects([Obj]),
        status(playing),
        next_id(1),
        commands([]),
        rev_hints([])
    )),
    execute_action(
        ctx_old(Ctx),
        ctx_new(_),
        action(parallel_race(
            [wait(5), noop, wait(10)]
        )),
        obj_old(Obj),
        obj_new([NewObj])
    ),
    % After execution, noop finished
    %   immediately, so race stops
    obj_acns(NewObj, [wait(3)|_])
)).

test("parallel_race: continues if no child \
done", (
    Obj = object(
        id(0), type(tower), attrs([]),
        actions([
            parallel_race([
                wait(5), wait(10)
            ]),
            wait(3)
        ]),
        collisions([])
    ),
    Ctx = ctx(state(
        frame(0),
        objects([Obj]),
        status(playing),
        next_id(1),
        commands([]),
        rev_hints([])
    )),
    execute_action(
        ctx_old(Ctx),
        ctx_new(_),
        action(parallel_race(
            [wait(5), wait(10)]
        )),
        obj_old(Obj),
        obj_new([NewObj])
    ),
    % Both children still running
    %   (wait actions yield)
    obj_acns(
        NewObj,
        [parallel_race_running([wait(4), wait(9)])|_]
    )
)).

test("parallel_race: despawns parent when child \
despawns", (
    Obj = object(
        id(1),
        type(static),
        attrs([x(0), y(0)]),
        actions([
            parallel_race([
                despawn,
                wait(10)
            ]),
            trigger_state_change(game_over(won)),
            wait(5)
        ]),
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
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(parallel_race([despawn, wait(10)])),
        obj_old(Obj),
        obj_new(ObjOut)
    ),
    % Parent must be despawned (empty list)
    %   because child despawned
    (ObjOut = [] ; err_write('ObjOut not []\n')),
    % Despawn hint must be recorded
    (ctx_revhints(CtxNew, [despawned(1, [x(0), y(0)])])
    ; err_write('despawned hint not recorded\n')),
    % Status must remain playing (game_over did NOT
    %   execute)
    (ctx_status(CtxNew, playing)
    ; err_write('wasnt playing')),
    % No commands
    ctx_cmds(CtxNew, [])
)).

test("set_attr: multiple sets overwrite, no \
duplicates", (
    ObjIn = object(
        id(1), type(enemy), attrs([x(5), y(10)]),
        actions([
            set_attr(hp, 100),
            set_attr(hp, 75),
            set_attr(hp, 50)
        ]), collisions([])
    ),
    Ctx = ctx(state(
        frame(0),
        objects([]),
        status(playing),
        next_id(1),
        commands([]),
        rev_hints([])
    )),
    % Execute first set_attr
    execute_action(
        ctx_old(Ctx),
        ctx_new(Ctx1),
        action(set_attr(hp, 100)),
        obj_old(ObjIn),
        obj_new([Obj1])
    ),
    % Execute second set_attr
    execute_action(
        ctx_old(Ctx1),
        ctx_new(Ctx2),
        action(set_attr(hp, 75)),
        obj_old(Obj1),
        obj_new([Obj2])
    ),
    % Execute third set_attr
    execute_action(
        ctx_old(Ctx2),
        ctx_new(_),
        action(set_attr(hp, 50)),
        obj_old(Obj2),
        obj_new([ObjOut])
    ),
    % Verify only one hp attribute exists with final value
    obj_attrs(ObjOut, Attrs),
    findall(Val, member(hp(Val), Attrs), HpVals),
    HpVals = [50],  % Only one hp attribute, with value 50
    member(x(5), Attrs),
    member(y(10), Attrs)
)).

% ==========================================================
% Tests: repeat
% ==========================================================

test("repeat: expands actions once and decrements", (
    ObjIn = object(
        id(1),
        type(static),
        attrs([]),
        actions([
            repeat(3, [noop, set_attr(count, 1)]),
            despawn
        ]),
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
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(repeat(3, [noop, set_attr(count, 1)])),
        obj_old(ObjIn),
        obj_new([ObjOut])
    ),
    % Should expand to: [noop, set_attr(count, 1),
    %   repeat(2, [noop, set_attr(count, 1)]), despawn]
    obj_acns(ObjOut, Actions),
    Actions = [
        noop,
        set_attr(count, 1),
        repeat(2, [noop, set_attr(count, 1)]),
        despawn
    ],
    ctx_revhints(CtxNew, []),
    ctx_cmds(CtxNew, [])
)).

test("repeat: last repetition doesn't add repeat", (
    ObjIn = object(
        id(1),
        type(static),
        attrs([]),
        actions([repeat(1, [noop]), despawn]),
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
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(repeat(1, [noop])),
        obj_old(ObjIn),
        obj_new([ObjOut])
    ),
    % Should expand to: [noop, despawn] (no repeat left)
    obj_acns(ObjOut, [noop, despawn]),
    ctx_revhints(CtxNew, []),
    ctx_cmds(CtxNew, [])
)).

test("repeat: multiple actions in repeat list", (
    ObjIn = object(
        id(1),
        type(static),
        attrs([]),
        actions([
            repeat(2, [
                noop,
                set_attr(a, 1),
                set_attr(b, 2)
            ]),
            despawn
        ]),
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
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(repeat(2, [
            noop,
            set_attr(a, 1),
            set_attr(b, 2)
        ])),
        obj_old(ObjIn),
        obj_new([ObjOut])
    ),
    % Should expand to: [noop, set_attr(a, 1),
    %   set_attr(b, 2), repeat(1, [...]), despawn]
    obj_acns(ObjOut, Actions),
    Actions = [
        noop,
        set_attr(a, 1),
        set_attr(b, 2),
        repeat(1, [noop, set_attr(a, 1), set_attr(b, 2)]),
        despawn
    ],
    ctx_revhints(CtxNew, []),
    ctx_cmds(CtxNew, [])
)).

% ==========================================================
% Tests: move_delta
% ==========================================================

test("move_delta: single frame moves and completes", (
    ObjIn = object(
        id(1),
        type(static),
        attrs([x(10), y(20)]),
        actions([move_delta(1, 5, -3)]),
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
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(move_delta(1, 5, -3)),
        obj_old(ObjIn),
        obj_new([ObjOut])
    ),
    ObjOut = object(
        id(1),
        type(static),
        attrs(Attrs),
        actions([]),
        collisions([])
    ),
    member(x(15), Attrs),
    member(y(17), Attrs),
    ctx_revhints(CtxNew, []),
    ctx_cmds(CtxNew, [])
)).

test("move_delta: multiple frames continues", (
    ObjIn = object(
        id(1),
        type(static),
        attrs([x(0), y(0)]),
        actions([move_delta(3, 10, 5)]),
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
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(move_delta(3, 10, 5)),
        obj_old(ObjIn),
        obj_new([ObjOut])
    ),
    ObjOut = object(
        id(1),
        type(static),
        attrs(Attrs),
        actions([move_delta(2, 10, 5)]),
        collisions([])
    ),
    member(x(10), Attrs),
    member(y(5), Attrs),
    ctx_revhints(CtxNew, []),
    ctx_cmds(CtxNew, [])
)).

test("move_delta: negative deltas work", (
    ObjIn = object(
        id(1),
        type(static),
        attrs([x(50), y(50)]),
        actions([move_delta(2, -10, -5)]),
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
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(move_delta(2, -10, -5)),
        obj_old(ObjIn),
        obj_new([ObjOut])
    ),
    ObjOut = object(
        id(1),
        type(static),
        attrs(Attrs),
        actions([move_delta(1, -10, -5)]),
        collisions([])
    ),
    member(x(40), Attrs),
    member(y(45), Attrs),
    ctx_revhints(CtxNew, []),
    ctx_cmds(CtxNew, [])
)).

test("move_delta: preserves other attributes", (
    ObjIn = object(
        id(1),
        type(static),
        attrs([x(10), y(20), hp(100), speed(5)]),
        actions([move_delta(1, 5, -3)]),
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
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(move_delta(1, 5, -3)),
        obj_old(ObjIn),
        obj_new([ObjOut])
    ),
    ObjOut = object(
        id(1),
        type(static),
        attrs(Attrs),
        actions([]),
        collisions([])
    ),
    member(x(15), Attrs),
    member(y(17), Attrs),
    member(hp(100), Attrs),
    member(speed(5), Attrs),
    ctx_revhints(CtxNew, []),
    ctx_cmds(CtxNew, [])
)).
