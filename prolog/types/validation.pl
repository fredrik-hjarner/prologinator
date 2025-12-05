% Validation Predicates Module
% Validates ground terms using constraint predicates
% Based on type_constraint_predicates.md v2 + all addendums
%
% ==========================================================
% IMPORTANT: Structure Matching Pattern
% ==========================================================
% All validation helpers use a pattern matching approach to
% ensure structure mismatches throw exceptions rather than
% silently failing.
%
% Pattern:
%   helper(Term) :-
%       ( ground(Term) ->
%           ( Term = expected_structure(...) ->
%               % Structure matches, validate content
%               ...
%           ;
%               % Structure doesn't match - throw
%               format_("ERROR: ...", [Term]),
%               throw(error(type_error(...), ...))
%           )
%       ;
%           true  % Not ground, succeed
%       ).
%
% Why this is needed:
% - If we just do: Term = expected_structure(...), and it
%   fails, Prolog silently fails (returns false)
% - The wrapper predicate then sees the failure and throws,
%   but we can't distinguish structure mismatches from
%   content validation failures
% - By explicitly checking if pattern matching succeeds,
%   we can throw immediately on structure mismatch with a
%   clear error message
%
% This ensures that:
% - Wrong functor (e.g., not_game_object(...)) → throws
% - Wrong arity (e.g., game_object(...) with 4 args) →
%   throws
% - Wrong structure (e.g., game_object(5, ...) instead of
%   game_object(id(5), ...)) → throws
% - Correct structure but invalid content → wrapper throws
%   with appropriate error
% ==========================================================

:- module(validation, [
    game_state_validation/1,
    game_object_validation/1,
    game_status_validation/1,
    command_validation/1,
    rev_hint_validation/1,
    action_validation/1
]).

:- use_module(library(format)).
:- use_module(library(lists), [length/2]).
:- use_module(library(os), [getenv/2]).

% ==========================================================
% Helper: Conditional format output
% ==========================================================
% format_/2: Outputs format message only if
% VALIDATION_ERR_MSG is not set to "false"
format_(Format, Args) :-
    ( getenv("VALIDATION_ERR_MSG", "false") ->
        true  % Suppress output
    ;
        format(Format, Args)  % Output normally
    ).

% ==========================================================
% Validation Predicates
% ==========================================================
% These predicates validate ground terms only.
% If term is not ground, they succeed without checking.
% If term is ground and invalid, they throw an error.

game_state_validation(Term) :-
    ( ground(Term) ->
        ( game_state_validation_helper(Term) ->
            true
        ;
            format_("ERROR: Invalid game_state:~n  ~w~n",
                   [Term]),
            throw(error(type_error(game_state, Term),
                        game_state_validation/1))
        )
    ;
        true
    ).

% ==========================================================
% game_state_validation_helper/1
% ==========================================================

game_state_validation_helper(Term) :-
    ( ground(Term) ->
        ( Term = game_state(
              frame(Frame),
              objects(Objects),
              status(Status),
              score(Score),
              next_id(NextID),
              commands(Commands),
              rev_hints(RevHints)
          ) ->
            % Structure matches, validate content
            integer(Frame),
            length(Objects, _),
            game_status_validation(Status),
            integer(Score),
            integer(NextID),
            length(Commands, _),
            length(RevHints, _),
            extract_ids(Objects, IDs),
            is_ascending_ids(IDs),
            ( IDs = [] ->
                MaxID = -1
            ;
                last_id(IDs, MaxID)
            ),
            NextID > MaxID
        ;
            % Structure doesn't match - throw
            format_(
                "ERROR: Invalid game_state structure:~n  \
~w~n",
                [Term]
            ),
            throw(
                error(type_error(game_state, Term),
                      game_state_validation_helper/1)
            )
        )
    ;
        true
    ).

% ==========================================================
% Helper predicates for game_state validation
% ==========================================================

extract_ids([], []).
extract_ids(
    [game_object(id(ID), _, _, _, _)|Rest], [ID|IDs]
) :-
    extract_ids(Rest, IDs).

is_ascending_ids([]).
is_ascending_ids([_]).
is_ascending_ids([A, B|Rest]) :-
    A < B,
    is_ascending_ids([B|Rest]).

last_id([X], X).
last_id([_|T], X) :-
    last_id(T, X).

game_object_validation(Term) :-
    ( ground(Term) ->
        ( game_object_validation_helper(Term) ->
            true
        ;
            format_("ERROR: Invalid game_object:~n  ~w~n",
                   [Term]),
            throw(error(type_error(game_object, Term),
                        game_object_validation/1))
        )
    ;
        true
    ).

% ==========================================================
% game_object_validation_helper/1
% ==========================================================

game_object_validation_helper(Term) :-
    ( ground(Term) ->
        ( Term = game_object(
              id(ID), type(Type), attrs(Attrs),
              actions(Actions), collisions(_Colls)
          ) ->
            % Structure matches, validate content
            integer(ID),
            object_type_validation(Type),
            length(Attrs, _),
            length(Actions, _)
        ;
            % Structure doesn't match - throw
            format_(
                "ERROR: Invalid game_object structure:~n  \
~w~n",
                [Term]
            ),
            throw(
                error(type_error(game_object, Term),
                      game_object_validation_helper/1)
            )
        )
    ;
        true
    ).

game_status_validation(Term) :-
    ( ground(Term) ->
        ( Term = playing
        ; Term = won
        ; Term = lost
        ) ->
            true
        ;
            format_("ERROR: Invalid game_status:~n  ~w~n",
                   [Term]),
            throw(error(type_error(game_status, Term),
                        game_status_validation/1))
    ;
        true
    ).

command_validation(Term) :-
    ( ground(Term) ->
        ( ( spawn_request_validation(Term)
          ; state_change_validation(Term)
          ) ->
              true
          ;
              format_("ERROR: Invalid command:~n  ~w~n",
                     [Term]),
              throw(error(type_error(command, Term),
                          command_validation/1))
        )
    ;
        true
    ).

% ==========================================================
% spawn_request_validation/1
% ==========================================================

spawn_request_validation(Term) :-
    ( ground(Term) ->
        ( Term = spawn_request(Type, Pos, Acts) ->
            % Structure matches, validate content
            object_type_validation(Type),
            pos_validation(Pos),
            length(Acts, _)
        ;
            % Structure doesn't match - throw
            format_(
                "ERROR: Invalid spawn_request \
structure:~n  ~w~n",
                [Term]
            ),
            throw(
                error(type_error(spawn_request, Term),
                      spawn_request_validation/1)
            )
        )
    ;
        true
    ).

% ==========================================================
% state_change_validation/1
% ==========================================================

state_change_validation(Term) :-
    ( ground(Term) ->
        ( Term = state_change(Change) ->
            % Structure matches, validate content
            ( ground(Change) ->
                state_change_validation_helper(Change)
            ;
                true
            )
        ;
            % Structure doesn't match - throw
            format_(
                "ERROR: Invalid state_change structure:~n  \
~w~n",
                [Term]
            ),
            throw(
                error(type_error(state_change, Term),
                      state_change_validation/1)
            )
        )
    ;
        true
    ).

state_change_validation_helper(score(Delta)) :-
    ( ground(Delta) ->
        integer(Delta)
    ;
        true
    ).

state_change_validation_helper(game_over(won)).
state_change_validation_helper(game_over(lost)).

% ==========================================================
% Helper validation predicates
% ==========================================================

object_type_validation(static).
object_type_validation(enemy).
object_type_validation(proj).
object_type_validation(player).
object_type_validation(tower).

pos_validation(Term) :-
    ( ground(Term) ->
        ( Term = pos(X, Y) ->
            % Structure matches, validate content
            integer(X),
            integer(Y)
        ;
            % Structure doesn't match - throw
            format_(
                "ERROR: Invalid pos structure:~n  ~w~n",
                [Term]
            ),
            throw(
                error(type_error(pos, Term),
                      pos_validation/1)
            )
        )
    ;
        true
    ).

rev_hint_validation(Term) :-
    ( ground(Term) ->
        ( rev_hint_validation_helper(Term) ->
            true
        ;
            format_("ERROR: Invalid rev_hint:~n  ~w~n",
                   [Term]),
            throw(error(type_error(rev_hint, Term),
                        rev_hint_validation/1))
        )
    ;
        true
    ).

% ==========================================================
% rev_hint_validation_helper/1
% ==========================================================

rev_hint_validation_helper(Term) :-
    ( ground(Term) ->
        ( Term = despawned(ID, Attrs) ->
            % Structure matches, validate content
            integer(ID),
            length(Attrs, _)
        ;
            % Structure doesn't match - throw
            format_(
                "ERROR: Invalid rev_hint structure:~n  \
~w~n",
                [Term]
            ),
            throw(
                error(type_error(rev_hint, Term),
                      rev_hint_validation_helper/1)
            )
        )
    ;
        true
    ).

action_validation(Term) :-
    ( ground(Term) ->
        ( action_validation_helper(Term) ->
            true
        ;
            format_("ERROR: Invalid action:~n  ~w~n",
                   [Term]),
            throw(error(type_error(action, Term),
                        action_validation/1))
        )
    ;
        true
    ).

% ==========================================================
% action_validation_helper/1
% ==========================================================

action_validation_helper(Term) :-
    ( ground(Term) ->
        ( Term = wait_frames(N) ->
            % Structure matches, validate content
            ( ground(N) ->
                integer(N)
            ;
                true
            )
        ; Term = move_to(X, Y, Frames) ->
            % Structure matches, validate content
            integer(X),
            integer(Y),
            integer(Frames)
        ; Term = despawn ->
            % Structure matches, no content to validate
            true
        ; Term = spawn(Type, Pos, Acts) ->
            % Structure matches, validate content
            object_type_validation(Type),
            pos_validation(Pos),
            length(Acts, _)
        ; Term = loop(Acts) ->
            % Structure matches, validate content
            ( ground(Acts) ->
                length(Acts, _)
            ;
                true
            )
        ; Term = trigger_state_change(Change) ->
            % Structure matches, validate content
            ( ground(Change) ->
                state_change_validation_helper(Change)
            ;
                true
            )
        ; Term = parallel(Children) ->
            % Structure matches, validate content
            ( ground(Children) ->
                length(Children, _)
            ;
                true
            )
        ; Term = parallel_running(Children) ->
            % Structure matches, validate content
            ( ground(Children) ->
                length(Children, _)
            ;
                true
            )
        ;
            % No pattern matched - throw
            format_(
                "ERROR: Invalid action structure:~n  \
~w~n",
                [Term]
            ),
            throw(
                error(type_error(action, Term),
                      action_validation_helper/1)
            )
        )
    ;
        true
    ).

% ==========================================================
% Tests for Validation Predicates
% ==========================================================

% test/2 clauses are intentionally separated by other code
:- discontiguous(test/2).

test("game_state_validation: valid state passes", (
    State = game_state(
        frame(0),
        objects([game_object(
            id(0), type(static), attrs([pos(0, 0)]),
            actions([]), collisions([])
        )]),
        status(playing),
        score(0),
        next_id(1),
        commands([]),
        rev_hints([])
    ),
    game_state_validation(State)
)).

test("game_state_validation: complex state with \
multiple objects and commands", (
    State = game_state(
        frame(5),
        objects([
            game_object(
                id(0), type(tower), attrs([pos(10, 19)]),
                actions([wait_frames(3)]), collisions([])
            ),
            game_object(
                id(1), type(enemy), attrs([pos(5, 5)]),
                actions([move_to(10, 10, 5)]),
                collisions([])
            ),
            game_object(
                id(2), type(proj), attrs([pos(15, 15)]),
                actions([]), collisions([])
            )
        ]),
        status(playing),
        score(100),
        next_id(3),
        commands([
            spawn_request(enemy, pos(0, 0), []),
            state_change(score(10))
        ]),
        rev_hints([
            despawned(1, [pos(10, 10)])
        ])
    ),
    game_state_validation(State)
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
    State = game_state(
        frame(0),
        objects([]),
        status(invalid_status),
        score(0),
        next_id(1),
        commands([]),
        rev_hints([])
    ),
    expect_exception(game_state_validation(State))
)).

test("game_state_validation: non-integer frame fails", (
    State = game_state(
        frame(not_an_int),
        objects([]),
        status(playing),
        score(0),
        next_id(1),
        commands([]),
        rev_hints([])
    ),
    expect_exception(game_state_validation(State))
)).

test("game_state_validation: NextID <= max ID fails", (
    Obj = game_object(
        id(5),
        type(static),
        attrs([]),
        actions([]),
        collisions([])
    ),
    State = game_state(
        frame(0),
        objects([Obj]),
        status(playing),
        score(0),
        next_id(5),
        commands([]),
        rev_hints([])
    ),
    expect_exception(game_state_validation(State))
)).

test("game_object_validation: wrong structure (ID not \
wrapped) throws", (
    Obj = game_object(
        5,
        type(static),
        attrs([]),
        actions([]),
        collisions([])
    ),
    expect_exception(game_object_validation(Obj))
)).

test("game_object_validation: invalid object type fails", (
    Obj = game_object(
        id(0),
        type(invalid_type),
        attrs([]),
        actions([]),
        collisions([])
    ),
    expect_exception(game_object_validation(Obj))
)).

test("command_validation: invalid spawn type fails", (
    Cmd = spawn_request(invalid_type, pos(0, 0), []),
    expect_exception(command_validation(Cmd))
)).

test("command_validation: invalid pos in spawn fails", (
    Cmd = spawn_request(static, not_a_pos, []),
    expect_exception(command_validation(Cmd))
)).

test("pos_validation: non-integer coords fail via action", (
    Action1 = spawn(static, pos(not_int, 10), []),
    expect_exception(action_validation(Action1)),
    Action2 = spawn(static, pos(10, not_int), []),
    expect_exception(action_validation(Action2))
)).

test("action_validation: invalid wait_frames fails", (
    expect_exception(
        action_validation(wait_frames(not_int))
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
    Action1 = spawn(invalid_type, pos(0, 0), []),
    expect_exception(action_validation(Action1)),
    Action2 = spawn(static, not_a_pos, []),
    expect_exception(action_validation(Action2))
)).

test("rev_hint_validation: invalid despawned fails", (
    Hint = despawned(not_int,[]),
    expect_exception(rev_hint_validation(Hint))
)).

% ----------------------------------------------------------
% Structure mismatch tests
% ----------------------------------------------------------
% These tests verify that validation throws exceptions when
% the structure doesn't match (wrong functor, wrong arity,
% wrong argument structure)

test("game_state_validation: wrong arity throws", (
    State = game_state(
        frame(0),
        objects([]),
        status(playing),
        score(0),
        next_id(1),
        commands([])
    ),
    expect_exception(game_state_validation(State))
)).

test("game_state_validation: wrong functor throws", (
    State = not_game_state(
        frame(0),
        objects([]),
        status(playing),
        score(0),
        next_id(1),
        commands([]),
        rev_hints([])
    ),
    expect_exception(game_state_validation(State))
)).

test("game_object_validation: wrong arity throws", (
    Obj = game_object(
        id(0), type(static), attrs([]), actions([])
    ),
    expect_exception(game_object_validation(Obj))
)).

test("game_object_validation: wrong functor throws", (
    Obj = not_game_object(
        id(0),
        type(static),
        attrs([]),
        actions([]),
        collisions([])
    ),
    expect_exception(game_object_validation(Obj))
)).

test("spawn_request_validation: wrong arity throws", (
    Cmd = spawn_request(enemy, pos(0, 0)),
    expect_exception(spawn_request_validation(Cmd))
)).

test("spawn_request_validation: wrong functor throws", (
    Cmd = not_spawn_request(enemy, pos(0, 0), []),
    expect_exception(spawn_request_validation(Cmd))
)).

test("state_change_validation: wrong structure throws", (
    Cmd = not_state_change(score(10)),
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

test("rev_hint_validation: wrong arity throws", (
    Hint = despawned(1),
    expect_exception(rev_hint_validation(Hint))
)).

test("rev_hint_validation: wrong functor throws", (
    Hint = not_despawned(1, []),
    expect_exception(rev_hint_validation(Hint))
)).

test("action_validation: wrong structure throws", (
    Action = not_an_action(1, 2, 3),
    expect_exception(action_validation(Action))
)).

test("action_validation: move_to wrong arity throws", (
    Action = move_to(10, 20),
    expect_exception(action_validation(Action))
)).

test("action_validation: spawn wrong arity throws", (
    Action = spawn(enemy, pos(0, 0)),
    expect_exception(action_validation(Action))
)).
