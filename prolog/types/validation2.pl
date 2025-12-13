% Validation Schemas Module (v2)
% Defines xod schemas for game types
% Callers use: validate(Term, state_schema),
% etc.

% ==========================================================
% SCHEMA DEFINITIONS
% ==========================================================
% These schemas match the structure used in validation.pl
% Usage: validate(Term, state_schema), etc.

% ==========================================================
% Primitive Schemas
% ==========================================================

% Object type: static, enemy, proj, player, tower
object_type_schema(enum([
    static, enemy, proj, player, tower
])).

% Game status: playing, won, lost
game_status_schema(enum([playing, won, lost])).

% Position: pos(X, Y) where X and Y are integers
pos_schema(struct(pos, [
    x(integer),
    y(integer)
])).

% ==========================================================
% State Change Schemas
% ==========================================================

% Game over: game_over(won) or game_over(lost)
game_over_schema(struct(game_over, [
    status(enum([won, lost]))
])).

% Change content: game_over
state_change_content_schema(schema(game_over_schema)).

% ==========================================================
% Action Schemas
% ==========================================================

% Action: various action types (union of all action schemas)
% Note: despawn_schema checked first (atom, not compound)
action_schema(union([
    schema(despawn_schema),
    schema(wait_schema),
    schema(move_to_schema),
    schema(spawn_action_schema),
    schema(loop_schema),
    schema(trigger_state_change_schema),
    schema(parallel_all_schema),
    schema(parallel_all_running_schema)
])).

% Wait frames: wait(N) where N is integer
wait_schema(struct(wait, [
    frames(integer)
])).

% Move to: move_to(X, Y, Frames) where all are integers
move_to_schema(struct(move_to, [
    x(integer),
    y(integer),
    frames(integer)
])).

% Despawn: despawn (no arguments, just an atom)
% despawn_schema(struct(despawn, [])).
despawn_schema(enum([despawn])).

% Spawn: spawn(Type, Pos, Acts)
spawn_action_schema(struct(spawn, [
    type(schema(object_type_schema)),
    pos(schema(pos_schema)),
    actions(list(schema(action_schema)))
])).

% Loop: loop(Acts) where Acts is list of actions
loop_schema(struct(loop, [
    actions(list(schema(action_schema)))
])).

% Trigger state change: trigger_state_change(Change)
trigger_state_change_schema(struct(trigger_state_change, [
    change(schema(state_change_content_schema))
])).

% Parallel: parallel_all(Children) where Children is list
parallel_all_schema(struct(parallel_all, [
    children(list(schema(action_schema)))
])).

% Parallel all running: parallel_all_running(Children)
parallel_all_running_schema(struct(parallel_all_running, [
    children(list(schema(action_schema)))
])).


% ==========================================================
% Command Schemas
% ==========================================================

% Command: spawn_request or state_change (union)
command_schema(union([
    schema(spawn_request_schema),
    schema(state_change_schema)
])).

% Spawn request: spawn_request(Type, Pos, Acts)
spawn_request_schema(struct(spawn_request, [
    type(schema(object_type_schema)),
    pos(schema(pos_schema)),
    actions(list(schema(action_schema)))
])).

% State change command: state_change(Change)
state_change_schema(struct(state_change, [
    change(schema(state_change_content_schema))
])).

% ==========================================================
% Rev Hint Schemas
% ==========================================================

% ==========================================================
% Object Schema
% ==========================================================

% Wrapped term schemas for object fields
% id(ID) where ID is integer
id_schema(struct(id, [
    value(integer)
])).

% type(Type) where Type is object_type enum
type_schema(struct(type, [
    value(schema(object_type_schema))
])).

% attrs(Attrs) where Attrs is list of any
attrs_schema(struct(attrs, [
    value(any)
])).

% actions(Actions) where Actions is list of actions
actions_schema(struct(actions, [
    value(list(schema(action_schema)))
])).

% collisions(Colls) where Colls is list of any
collisions_schema(struct(collisions, [
    value(list(any))
])).

% Object: object(id(ID), type(Type), attrs(Attrs),
%   actions(Actions), collisions(Colls))
object_schema(struct(object, [
    id(schema(id_schema)),
    type(schema(type_schema)),
    attrs(schema(attrs_schema)),
    actions(schema(actions_schema)),
    collisions(schema(collisions_schema))
])).

% ==========================================================
% State Schema
% ==========================================================

% State: state(frame(Frame), objects(Objects),
%   status(Status), ...
% Each argument is wrapped term like frame(Frame), validate:
% - frame(Frame) where Frame is integer >= 0
% - objects(Objects) where Objects is list of objects
% - status(Status) where Status is playing/won/lost
% - next_id(NextID) where NextID is integer >= 1
% - commands(Commands) where Commands is list

% Wrapped term schemas: frame(Frame), objects(Objects), etc.
% These are structs with arity 1 (single arg is value)
% For frame(Frame), validate frame/1 and Frame is integer
frame_schema(struct(frame, [
    value(integer(0, _))
])).

% For objects(Objects), validate objects/1 and
%   Objects is list
objects_schema(struct(objects, [
    value(list(schema(object_schema)))
])).

% For status(Status), validate status/1 and Status is enum
status_schema(struct(status, [
    value(schema(game_status_schema))
])).

% For next_id(NextID), validate next_id/1 and
%   NextID is integer
next_id_schema(struct(next_id, [
    value(integer(1, _))
])).

% For commands(Commands), validate commands/1 and
%   Commands is list
commands_schema(struct(commands, [
    value(list(schema(command_schema)))
])).

% State schema: validates state/6 with wrapped arguments
state_schema(struct(state, [
    frame(schema(frame_schema)),
    objects(schema(objects_schema)),
    attrs(schema(attrs_schema)),
    status(schema(status_schema)),
    next_id(schema(next_id_schema)),
    commands(schema(commands_schema))
])).

% Context schema: ctx(State) where State is a state
context_schema(struct(ctx, [
    state(schema(state_schema))
])).

% ==========================================================
% Tests
% ==========================================================

% test/2 clauses are intentionally separated by other code
:- discontiguous(test/2).

test("game_state_validation: valid state passes", (
    empty_assoc(EmptyAttrs),
    State = state(
        frame(0),
        objects([object(
            id(0), type(static), attrs([pos(0, 0)]),
            actions([]), collisions([])
        )]),
        attrs(EmptyAttrs),
        status(playing),
        next_id(1),
        commands([])
    ),
    validate(State, state_schema)
)).

test("game_state_validation: complex state with \
multiple objects and commands", (
    empty_assoc(EmptyAttrs),
    State = state(
        frame(5),
        objects([
            object(
                id(0), type(tower), attrs([pos(10, 19)]),
                actions([wait(3)]), collisions([])
            ),
            object(
                id(1), type(enemy), attrs([pos(5, 5)]),
                actions([move_to(10, 10, 5)]),
                collisions([])
            ),
            object(
                id(2), type(proj), attrs([pos(15, 15)]),
                actions([]), collisions([])
            )
        ]),
        attrs(EmptyAttrs),
        status(playing),
        next_id(3),
        commands([
            spawn_request(enemy, pos(0, 0), []),
            spawn_request(enemy, pos(0, 0), [])
        ])
    ),
    validate(State, state_schema)
)).

test("context_validation: complex context with multiple \
objects and commands", (
    Ctx = ctx(state(
        frame(42),
        objects([
            object(
                id(0), type(tower), attrs([pos(5, 19)]),
                actions([
                    loop([
                        wait(3),
                        spawn(proj, pos(5, 19), [
                            move_to(5, 0, 20)
                        ])
                    ])
                ]), collisions([])
            ),
            object(
                id(1), type(enemy), attrs([pos(10, 5)]),
                actions([
                    move_to(19, 5, 15),
                    wait(1)
                ]), collisions([])
            ),
            object(
                id(2), type(proj), attrs([pos(15, 10)]),
                actions([move_to(15, 0, 10)]),
                collisions([])
            ),
            object(
                id(3), type(static), attrs([]),
                actions([
                    parallel_all([
                        wait(5),
                        spawn(enemy, pos(0, 10), [])
                    ])
                ]), collisions([])
            )
        ]),
        attrs(t),
        status(playing),
        next_id(4),
        commands([
            spawn_request(enemy, pos(0, 0), []),
            spawn_request(proj, pos(10, 10), [
                move_to(10, 0, 10)
            ])
        ])
    )),
    validate(Ctx, context_schema)
)).

% ----------------------------------------------------------
% Helper predicate for tests that expect exceptions
% ----------------------------------------------------------
% expect_exception/1: Succeeds if Goal throws an exception,
% fails otherwise (if Goal succeeds or fails without
% throwing).
expect_exception(Goal) :-
    catch((Goal, fail), _, true).

% ----------------------------------------------------------
% Expected to fail cases
% ----------------------------------------------------------
% These tests use expect_exception/1 which encapsulates the
% pattern: catch((Goal, fail), _, true)
%
% How it works:
% - If Goal throws an exception: catch catches it,
%   executes true,
%   succeeds → Test passes ✓ (expected - validation should
%   throw for invalid input)
% - If Goal succeeds: fail is executed (not an exception,
%   just fails)
%   → The goal (Goal, fail) fails, catch fails, test fails ✓
%   → This is what we want - validation should have thrown!
% - If Goal fails: (Goal, fail) fails immediately,
%   catch fails,
%   test fails ✓ → Catches bugs where validation fails
%   instead of throwing
%
% Key insight: catch only catches exceptions, not normal
% failures. The test only passes when an exception is
% thrown.
% ----------------------------------------------------------

test("game_state_validation: invalid status fails", (
    State = state(
        frame(0),
        objects([]),
        status(invalid_status),
        next_id(1),
        commands([])
    ),
    expect_exception(
        validate(State, state_schema)
    )
)).

test("game_state_validation: non-integer frame fails", (
    State = state(
        frame(not_an_int),
        objects([]),
        status(playing),
        next_id(1),
        commands([])
    ),
    expect_exception(
        validate(State, state_schema)
    )
)).

% TODO: xod does not support such validation.
% test("game_state_validation: NextID <= max ID fails", (
%     Obj = object(
%         id(5),
%         type(static),
%         attrs([]),
%         actions([]),
%         collisions([])
%     ),
%     State = state(
%         frame(0),
%         objects([Obj]),
%         status(playing),
%         next_id(5),
%         commands([]),
%         
%     ),
%     expect_exception(
%         validate(State, state_schema)
%     )
% )).

test("object_validation: wrong structure (ID not \
wrapped) throws", (
    Obj = object(
        5,
        type(static),
        attrs([]),
        actions([]),
        collisions([])
    ),
    expect_exception(
        validate(Obj, object_schema)
    )
)).

test("object_validation: invalid object type fails", (
    Obj = object(
        id(0),
        type(invalid_type),
        attrs([]),
        actions([]),
        collisions([])
    ),
    expect_exception(
        validate(Obj, object_schema)
    )
)).

test("command_validation: invalid spawn type fails", (
    Cmd = spawn_request(invalid_type, pos(0, 0), []),
    expect_exception(
        validate(Cmd, command_schema)
    )
)).

test("command_validation: invalid pos in spawn fails", (
    Cmd = spawn_request(static, not_a_pos, []),
    expect_exception(
        validate(Cmd, command_schema)
    )
)).

test("pos_validation: non-integer coords fail via action", (
    Action1 = spawn(static, pos(not_int, 10), []),
    expect_exception(
        validate(Action1, action_schema)
    ),
    Action2 = spawn(static, pos(10, not_int), []),
    expect_exception(
        validate(Action2, action_schema)
    )
)).

test("action_validation: invalid wait fails", (
    expect_exception(
        validate(wait(not_int), action_schema)
    )
)).

test("action_validation: invalid move_to fails", (
    Action1 = move_to(not_int,10,5),
    expect_exception(
        validate(Action1, action_schema)
    ),
    Action2 = move_to(10,not_int,5),
    expect_exception(
        validate(Action2, action_schema)
    ),
    Action3 = move_to(10,10,not_int),
    expect_exception(
        validate(Action3, action_schema)
    )
)).

test("action_validation: invalid spawn action fails", (
    Action1 = spawn(invalid_type, pos(0, 0), []),
    expect_exception(
        validate(Action1, action_schema)
    ),
    Action2 = spawn(static, not_a_pos, []),
    expect_exception(
        validate(Action2, action_schema)
    )
)).

% ----------------------------------------------------------
% Structure mismatch tests
% ----------------------------------------------------------
% These tests verify that validation throws exceptions when
% the structure doesn't match (wrong functor, wrong arity,
% wrong argument structure)

test("game_state_validation: wrong arity throws", (
    State = state(
        frame(0),
        objects([]),
        attrs(t),
        status(playing),
        next_id(1)
    ),
    expect_exception(
        validate(State, state_schema)
    )
)).

test("game_state_validation: wrong functor throws", (
    State = not_state(
        frame(0),
        objects([]),
        attrs(t),
        status(playing),
        next_id(1),
        commands([])
    ),
    expect_exception(
        validate(State, state_schema)
    )
)).

test("object_validation: wrong arity throws", (
    Obj = object(
        id(0), type(static), attrs([]), actions([])
    ),
    expect_exception(
        validate(Obj, object_schema)
    )
)).

test("object_validation: wrong functor throws", (
    Obj = not_object(
        id(0),
        type(static),
        attrs([]),
        actions([]),
        collisions([])
    ),
    expect_exception(
        validate(Obj, object_schema)
    )
)).

test("spawn_request_validation: wrong arity throws", (
    Cmd = spawn_request(enemy, pos(0, 0)),
    expect_exception(
        validate(Cmd, spawn_request_schema)
    )
)).

test("spawn_request_validation: wrong functor throws", (
    Cmd = not_spawn_request(enemy, pos(0, 0), []),
    expect_exception(
        validate(Cmd, spawn_request_schema)
    )
)).

test("state_change_validation: wrong structure throws", (
    Cmd = not_state_change(game_over(won)),
    expect_exception(
        validate(Cmd, state_change_schema)
    )
)).

test("pos_validation: wrong arity throws", (
    Pos = pos(0),
    expect_exception(
        validate(Pos, pos_schema)
    )
)).

test("pos_validation: wrong functor throws", (
    Pos = not_pos(0, 0),
    expect_exception(
        validate(Pos, pos_schema)
    )
)).

test("action_validation: wrong structure throws", (
    Action = not_an_action(1, 2, 3),
    expect_exception(
        validate(Action, action_schema)
    )
)).

test("action_validation: move_to wrong arity throws", (
    Action = move_to(10, 20),
    expect_exception(
        validate(Action, action_schema)
    )
)).

test("action_validation: spawn wrong arity throws", (
    Action = spawn(enemy, pos(0, 0)),
    expect_exception(
        validate(Action, action_schema)
    )
)).

test("action_validation: despawn", (
    validate(despawn, action_schema)
)).

% ==========================================================
% Tests: Actions with unbound variables (backward execution)
% ==========================================================
% These tests prove that validation correctly handles
% unbound variables in actions, which is needed for
% backward execution/inference in execute_action.pl

test("action_validation: spawn with unbound variables \
passes", (
    Action = spawn(Type, Pos, Actions),
    % Type, Pos, and Actions are unbound - should pass
    validate(Action, action_schema),
    % Verify it's still unbound after validation
    var(Type),
    var(Pos),
    var(Actions)
)).

test("action_validation: loop with unbound actions \
passes", (
    Action = loop(Actions),
    % Actions is unbound - should pass
    validate(Action, action_schema),
    % Verify it's still unbound after validation
    var(Actions)
)).

test("action_validation: trigger_state_change with \
unbound change passes", (
    Action = trigger_state_change(Change),
    % Change is unbound - should pass
    validate(Action, action_schema),
    % Verify it's still unbound after validation
    var(Change)
)).

test("action_validation: wait with unbound N passes", (
    Action = wait(N),
    % N is unbound - should pass
    validate(Action, action_schema),
    % Verify it's still unbound after validation
    var(N)
)).

test("action_validation: move_to with unbound coordinates \
passes", (
    Action = move_to(X, Y, Frames),
    % X, Y, and Frames are unbound - should pass
    validate(Action, action_schema),
    % Verify they're still unbound after validation
    var(X),
    var(Y),
    var(Frames)
)).

test("action_validation: spawn with partially bound \
variables passes", (
    Action = spawn(enemy, Pos, Actions),
    % Pos and Actions are unbound, Type is bound -
    %   should pass
    validate(Action, action_schema),
    % Verify unbound variables are still unbound
    var(Pos),
    var(Actions)
)).
