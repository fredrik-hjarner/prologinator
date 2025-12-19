% Validation Tests Module
% Tests for validation.pl

:- module(validation_test, []).

:- use_module('../../build/prologinator').
:- use_module('../../prolog/util/test_util').

:- use_module(library(format)).
:- use_module(library(assoc), [
    empty_assoc/1,
    put_assoc/4
]).

% test/2 clauses are intentionally separated by other code
:- discontiguous(test/2).

% ==========================================================
% Tests for Validation Predicates
% ==========================================================

test("game_state_validation: valid state passes", (
    empty_assoc(EmptyAttrs0),
    put_assoc(0, EmptyAttrs0, [attr(type, static)],
              EmptyAttrs),
    empty_assoc(EmptyActionStore0),
    put_assoc(0, EmptyActionStore0, [[]], EmptyActionStore),
    State = state(
        frame(0),
        objects([object(id(0))]),
        attrs(EmptyAttrs),
        status(playing),
        next_id(1),
        commands(spawn_cmds([]), fork_cmds([])),
        actionstore(EmptyActionStore)
    ),
    state_validation(State)
)).

test("game_state_validation: complex state with \
multiple objects and commands", (
    empty_assoc(EmptyAttrs0),
    put_assoc(0, EmptyAttrs0,
              [attr(type, tower), attr(x, 10), attr(y, 19)],
              Attrs1),
    put_assoc(1, Attrs1,
              [attr(type, enemy), attr(x, 5), attr(y, 5)],
              Attrs2),
    put_assoc(2, Attrs2,
              [attr(type, proj), attr(x, 15), attr(y, 15)],
              EmptyAttrs),
    empty_assoc(EmptyActionStore0),
    put_assoc(0, EmptyActionStore0, [[]], ActionStore1),
    put_assoc(1, ActionStore1, [[]], ActionStore2),
    put_assoc(2, ActionStore2, [[]], EmptyActionStore),
    State = state(
        frame(5),
        objects([
            object(id(0)),
            object(id(1)),
            object(id(2))
        ]),
        attrs(EmptyAttrs),
        status(playing),
        next_id(3),
        commands(spawn_cmds([
            spawn_cmd(actions([])),
            spawn_cmd(actions([]))
        ]), fork_cmds([])),
        actionstore(EmptyActionStore)
    ),
    state_validation(State)
)).

test("context_validation: complex context with multiple \
objects and commands", (
    empty_assoc(EmptyAttrs0),
    put_assoc(0, EmptyAttrs0,
              [attr(type, tower), attr(x, 5), attr(y, 19)],
              Attrs1),
    put_assoc(1, Attrs1,
              [attr(type, enemy), attr(x, 10), attr(y, 5)],
              Attrs2),
    put_assoc(2, Attrs2,
              [attr(type, proj), attr(x, 15), attr(y, 10)],
              Attrs3),
    put_assoc(3, Attrs3, [attr(type, static)], EmptyAttrs),
    ctx_with_frame_attrs(42, EmptyAttrs, Ctx0),
    ctx_set_objs([
        object(id(0)),
        object(id(1)),
        object(id(2)),
        object(id(3))
    ], Ctx0, Ctx1),
    ctx_set_nextid(4, Ctx1, Ctx2),
    % Set up actionstore entries for all objects
    ctx_actionstore(ActionStore0, Ctx2, Ctx2),
    empty_assoc(EmptyActionStore0),
    put_assoc(0, EmptyActionStore0, [[]], ActionStore1),
    put_assoc(1, ActionStore1, [[]], ActionStore2),
    put_assoc(2, ActionStore2, [[]], ActionStore3),
    put_assoc(3, ActionStore3, [[]], ActionStore4),
    ctx_set_actionstore(ActionStore4, Ctx2, Ctx3),
    ctx_set_spawnCmds([
        spawn_cmd(actions([])),
        spawn_cmd(actions([move_to(10, 0, 10)]))
    ], Ctx3, Ctx),
    context_validation(Ctx, Ctx)
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
    empty_assoc(EmptyAttrs),
    empty_assoc(EmptyActionStore),
    State = state(
        frame(0),
        objects([]),
        attrs(EmptyAttrs),
        status(invalid_status),
        next_id(1),
        commands(spawn_cmds([]), fork_cmds([])),
        actionstore(EmptyActionStore)
    ),
    expect_exception(state_validation(State))
)).

test("game_state_validation: non-integer frame fails", (
    empty_assoc(EmptyAttrs),
    empty_assoc(EmptyActionStore),
    State = state(
        frame(not_an_int),
        objects([]),
        attrs(EmptyAttrs),
        status(playing),
        next_id(1),
        commands(spawn_cmds([]), fork_cmds([])),
        actionstore(EmptyActionStore)
    ),
    expect_exception(state_validation(State))
)).

test("game_state_validation: NextID <= max ID fails", (
    Obj = object(id(5)),
    empty_assoc(EmptyAttrs0),
    put_assoc(5, EmptyAttrs0, [attr(type, static)],
              EmptyAttrs),
    empty_assoc(EmptyActionStore),
    State = state(
        frame(0),
        objects([Obj]),
        attrs(EmptyAttrs),
        status(playing),
        next_id(5),
        commands(spawn_cmds([]), fork_cmds([])),
        actionstore(EmptyActionStore)
    ),
    expect_exception(state_validation(State))
)).

test("object_validation: wrong structure (ID not \
wrapped) throws", (
    Obj = object(5),
    expect_exception(object_validation(Obj))
)).

% Note: Invalid type is now tested at attribute level, not
%   object structure level
% test("object_validation: invalid object type fails", (
%     Obj = object(
%         id(0),
%         actions([])
%     ),
%     empty_attr_store(EmptyAttrs0),
%     put_assoc(0, EmptyAttrs0, [attr(type, invalid_type)],
%               EmptyAttrs),
%     % Type validation would happen when accessing
%     %   attribute
%     expect_exception(object_validation(Obj))
% )).

test("command_validation: invalid spawn type fails", (
    Cmd = spawn_request(123, 0, 0, []),
    expect_exception(command_validation(Cmd))
)).

test("command_validation: invalid pos in spawn fails", (
    Cmd = spawn_request(static, not_a_pos, []),
    expect_exception(command_validation(Cmd))
)).

test("action_validation: spawn with non-integer coords \
passes (no validation)", (
    % X and Y are not validated, so these should pass
    Action1 = spawn(static, not_int, 10, []),
    action_validation(Action1),
    Action2 = spawn(static, 10, not_int, []),
    action_validation(Action2)
)).

test("action_validation: invalid wait fails", (
    expect_exception(
        action_validation(wait(not_int))
    )
)).

test("action_validation: invalid move_to fails", (
    Action1 = move_to(not_int,10,5),
    expect_exception(action_validation(Action1)),
    Action2 = move_to(10,not_int,5),
    expect_exception(action_validation(Action2)),
    Action3 = move_to(10,10,not_int),
    expect_exception(action_validation(Action3))
)).

test("action_validation: invalid spawn action fails", (
    Action1 = spawn(123, 0, 0, []),
    expect_exception(action_validation(Action1)),
    Action2 = spawn(static, not_a_pos, []),
    expect_exception(action_validation(Action2))
)).

% ----------------------------------------------------------
% Structure mismatch tests
% ----------------------------------------------------------
% These tests verify that validation throws exceptions when
% the structure doesn't match (wrong functor, wrong arity,
% wrong argument structure)

test("game_state_validation: wrong arity throws", (
    empty_assoc(EmptyAttrs),
    State = state(
        frame(0),
        objects([]),
        attrs(EmptyAttrs),
        status(playing),
        next_id(1)
    ),
    expect_exception(state_validation(State))
)).

test("game_state_validation: wrong functor throws", (
    empty_assoc(EmptyAttrs),
    empty_assoc(EmptyActionStore),
    State = not_state(
        frame(0),
        objects([]),
        attrs(EmptyAttrs),
        status(playing),
        next_id(1),
        commands(spawn_cmds([]), fork_cmds([])),
        actionstore(EmptyActionStore)
    ),
    expect_exception(state_validation(State))
)).

test("object_validation: wrong arity throws", (
    Obj = object(
        id(0),
        extra_arg
    ),
    expect_exception(object_validation(Obj))
)).

test("object_validation: wrong functor throws", (
    Obj = not_object(id(0)),
    expect_exception(object_validation(Obj))
)).

test("spawn_request_validation: wrong arity throws", (
    Cmd = spawn_request(enemy, 0, 0),
    expect_exception(spawn_request_validation(Cmd))
)).

test("spawn_request_validation: wrong functor throws", (
    Cmd = not_spawn_request(enemy, 0, 0, []),
    expect_exception(spawn_request_validation(Cmd))
)).

test("state_change_validation: wrong structure throws", (
    Cmd = not_state_change(game_over(won)),
    expect_exception(state_change_validation(Cmd))
)).

test("pos_validation: wrong arity throws", (
    Pos = pos(0),
    expect_exception(pos_validation(Pos))
)).

test("pos_validation: wrong functor throws", (
    Pos = not_pos(0, 0),
    expect_exception(pos_validation(Pos))
)).

test("action_validation: wrong structure throws", (
    % User-defined actions are now allowed (they're callable
    % and not built-in functors), so this passes validation.
    % It will fail at runtime if not defined.
    % Instead, test that a non-callable term fails:
    Action = 123,
    expect_exception(action_validation(Action))
)).

test("action_validation: move_to wrong arity throws", (
    Action = move_to(10, 20),
    expect_exception(action_validation(Action))
)).

test("action_validation: spawn wrong arity throws", (
    Action = spawn(enemy, 0, 0),
    expect_exception(action_validation(Action))
)).

test("action_validation: set_attr passes", (
    action_validation(set_attr(hp, 100))
)).

test("action_validation: set_attr invalid arity fails", (
    Action = set_attr(hp),
    expect_exception(action_validation(Action))
)).

test("action_validation: noop passes", (
    action_validation(noop)
)).

test("action_validation: noop invalid arity fails", (
    Action = noop(something),
    expect_exception(action_validation(Action))
)).

test("action_validation: list passes", (
    action_validation(list([wait(1), move_to(0, 0, 5)]))
)).

test("action_validation: list invalid arity fails", (
    Action = list([wait(1)], extra_arg),
    expect_exception(action_validation(Action))
)).

test("action_validation: parallel_race passes", (
    action_validation(
        parallel_race([wait(1), noop])
    )
)).

test("action_validation: parallel_race with \
move_to passes", (
    action_validation(
        parallel_race([
            move_to(10, 20, 5),
            move_to(50, 60, 10)
        ])
    )
)).

test("action_validation: parallel_race wrong \
arity fails", (
    Action = parallel_race(
        [wait(1)], extra_arg
    ),
    expect_exception(action_validation(
        Action
    ))
)).

