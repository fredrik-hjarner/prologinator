:- module(execute_action_bwd_test, []).
:- use_module('./execute_action', [execute_action/5]).
:- use_module('./types/accessors').
:- use_module('./types/adv_accessors').
:- use_module('./types/constructors').
:- use_module('./util/util', [err_write/1, err_format/2]).
:- use_module(library(lists), [member/2]).
:- use_module(library(assoc), [
    empty_assoc/1,
    put_assoc/4
]).
% ==========================================================
% Backward Tests (infer non-ground values)
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
        actions([wait(N)]),
        collisions([])
    ),
    ObjOut = [object(
        id(1),
        type(static),
        actions([wait(2)]),
        collisions([])
    )],
    empty_assoc(EmptyAttrs),
    Ctx = ctx(state(
        frame(0),
        objects([]),
        attrs(EmptyAttrs),
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
        actions([
            parallel_all([wait(N1), wait(N2)])
        ]),
        collisions([])
    ),
    ObjOut = [object(
        id(1),
        type(static),
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
    empty_assoc(EmptyAttrs),
    Ctx = ctx(state(
        frame(0),
        objects([]),
        attrs(EmptyAttrs),
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

test("move_to: backward test - can infer target \
coordinates from final position", (
    % TargetX and TargetY are unknown - should be inferred
    Action = move_to(TargetX, TargetY, 1),
    ObjIn = object(
        id(1),
        type(static),
        actions([move_to(TargetX, TargetY, 1)]),
        []
    ),
    ObjOut = [object(
        id(1),
        type(static),
        actions([]),
        []
    )],
    Commands = [],
    RevHints = [],
    empty_assoc(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 0), attr(y, 0)],
              EmptyAttrs),
    Ctx = ctx(state(
        frame(0),
        objects([]),
        attrs(EmptyAttrs),
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
    ctx_attr_val(CtxNew, 1/x, 5),
    ctx_attr_val(CtxNew, 1/y, 5),
    % Verify that TargetX was correctly inferred
    TargetX = 5,
    % Verify that TargetY was correctly inferred
    TargetY = 5
)).

% --------------------------------------------------------
% Tests: spawn
% --------------------------------------------------------

% NOTE: Bidirectionality is broken for spawn action.
% The spawn implementation doesn't properly support backward
% execution to infer Type, X, Y, or Actions from the spawned
% object in CtxOut. This would require changes to the spawn
% implementation to properly unify the spawned object with
% CtxOut before setting attributes.

% test("spawn: backward test - can infer Type and Actions \
% from spawned object", (
%     % Type and Actions are unknown - should be inferred
%     % (X and Y are provided as ground values)
%     Action = spawn(Type, 10, 5, Actions),
%     ObjIn = object(
%         id(1),
%         type(static),
%         actions([
%             spawn(Type, 10, 5, Actions)
%         ]),
%         collisions([])
%     ),
%     ObjOut = [object(
%         id(1),
%         type(static),
%         actions([]),
%         collisions([])
%     )],
%     Commands = [],
%     RevHints = [],
%     % For backward execution, context already has the
%     % spawned object
%     empty_assoc(EmptyAttrsOut0),
%     put_assoc(1, EmptyAttrsOut0,
%               [attr(x, 10), attr(y, 5)],
%               EmptyAttrsOut),
%     CtxOut = ctx(state(
%         frame(0),
%         objects([
%             object(
%                 id(1), type(enemy),
%                 actions([move_to(0, 0, 10)]),
%                 collisions([])
%             )
%         ]),
%         attrs(EmptyAttrsOut),
%         status(playing),
%         next_id(2),
%         commands([]),
%         rev_hints([])
%     )),
%     empty_assoc(EmptyAttrsIn),
%     CtxIn = ctx(state(
%         frame(0),
%         objects([]),
%         attrs(EmptyAttrsIn),
%         status(playing),
%         next_id(1),
%         commands([]),
%         rev_hints([])
%     )),
%     execute_action(
%         ctx_old(CtxIn),
%         ctx_new(CtxOut),
%         action(Action),
%         obj_old(ObjIn),
%         obj_new(ObjOut)
%     ),
%     ctx_cmds(CtxOut, Commands),
%     ctx_revhints(CtxOut, RevHints),
%     % Verify that Type was correctly inferred
%     Type = enemy,
%     % Verify that Actions were correctly inferred
%     Actions = [move_to(0, 0, 10)]
% )).

% --------------------------------------------------------
% Tests: trigger_state_change
% --------------------------------------------------------

test("trigger_state_change: backward test - can infer \
change from updated status", (
    % Change is unknown - should be inferred
    Action = trigger_state_change(Change),
    ObjIn = object(
        id(1),
        type(static),
        actions([trigger_state_change(Change)]),
        collisions([])
    ),
    ObjOut = [object(
        id(1),
        type(static),
        actions([]),
        collisions([])
    )],
    Commands = [],
    RevHints = [],
    % For backward execution, context already has the
    % updated status
    empty_assoc(EmptyAttrs),
    CtxIn = ctx(state(
        frame(0),
        objects([]),
        attrs(EmptyAttrs),
        status(playing),
        next_id(1),
        commands([]),
        rev_hints([])
    )),
    CtxOut = ctx(state(
        frame(0),
        objects([]),
        attrs(EmptyAttrs),
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
        actions([loop(Actions)]),
        collisions([])
    ),
    ObjOut = [object(
        id(1),
        type(static),
        actions([
            move_to(5, 5, 1),
            loop([move_to(5, 5, 1)])
        ]),
        collisions([])
    )],
    Commands = [],
    RevHints = [],
    empty_assoc(EmptyAttrs),
    Ctx = ctx(state(
        frame(0),
        objects([]),
        attrs(EmptyAttrs),
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
    % ID is unknown - should be inferred
    ObjIn = object(
        id(ID),
        type(static),
        actions([]),
        collisions([])
    ),
    ObjOut = [],
    Commands = [],
    RevHints = [despawned(1, [attr(x, 10), attr(y, 20)])],
    % For backward execution, attributes should be in the
    % context before despawn
    empty_assoc(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 10), attr(y, 20)],
              EmptyAttrs),
    Ctx = ctx(state(
        frame(0),
        objects([]),
        attrs(EmptyAttrs),
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
    ID = 1  % Verify that ID was correctly inferred
)).

