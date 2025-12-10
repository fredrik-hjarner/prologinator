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
    context_validation/1,
    state_validation/1,
    object_validation/1,
    game_status_validation/1,
    command_validation/1,
    rev_hint_validation/1,
    action_validation/1
]).

:- use_module(library(format)).
:- use_module(library(lists), [length/2]).
:- use_module(library(assoc), [
    empty_assoc/1,
    put_assoc/4
]).
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

% ==========================================================
% context_validation/1
% ==========================================================

context_validation(ctx(State)) :-
    state_validation(State).

% ==========================================================
% state_validation/1
% ==========================================================

state_validation(Term) :-
    ( ground(Term) ->
        ( state_validation_helper(Term) ->
            true
        ;
            format_("ERROR: Invalid game_state:~n  ~w~n",
                   [Term]),
            throw(error(type_error(game_state, Term),
                        state_validation/1))
        )
    ;
        true
    ).

% ==========================================================
% state_validation_helper/1
% ==========================================================

state_validation_helper(Term) :-
    ( ground(Term) ->
        ( Term = state(
              frame(Frame),
              objects(Objects),
              attrs(Attrs),
              status(Status),
              next_id(NextID),
              commands(Commands),
              rev_hints(RevHints)
          ) ->
            % Structure matches, validate content
            integer(Frame),
            length(Objects, _),
            % Attrs is an assoc tree - just check it's
            % ground
            ground(Attrs),
            game_status_validation(Status),
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
                "ERROR: Invalid game_state structure:\n  \
 ~w~n",
                [Term]
            ),
            throw(
                error(type_error(game_state, Term),
                      state_validation_helper/1)
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
    [object(id(ID), _, _, _)|Rest], [ID|IDs]
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

object_validation(Term) :-
    ( ground(Term) ->
        ( object_validation_helper(Term) ->
            true
        ;
            format_("ERROR: Invalid object:~n  ~w~n",
                   [Term]),
            throw(error(type_error(object, Term),
                        object_validation/1))
        )
    ;
        true
    ).

% ==========================================================
% object_validation_helper/1
% ==========================================================

object_validation_helper(Term) :-
    ( ground(Term) ->
        ( Term = object(
              id(ID), type(Type),
              actions(Actions), collisions(_Colls)
          ) ->
            % Structure matches, validate content
            integer(ID),
            object_type_validation(Type),
            length(Actions, _)
        ;
            % Structure doesn't match - throw
            format_(
                "ERROR: Invalid object structure:~n  \
~w~n",
                [Term]
            ),
            throw(
                error(type_error(object, Term),
                      object_validation_helper/1)
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
        ( Term = spawn_request(Type, _X, _Y, Acts) ->
            % Structure matches, validate content
            object_type_validation(Type),
            % X and Y: no validation needed
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
% Helper: Check if term has a built-in action functor
% ==========================================================
% This checks the functor name, not the arity, to catch
% invalid built-in actions with wrong arity

is_builtin_functor(Term) :-
    callable(Term),
    functor(Term, Name, _Arity),
    builtin_action_name(Name).

builtin_action_name(wait).
builtin_action_name(move_to).
builtin_action_name(move_delta).
builtin_action_name(despawn).
builtin_action_name(spawn).
builtin_action_name(set_attr).
builtin_action_name(loop).
builtin_action_name(list).
builtin_action_name(repeat).
builtin_action_name(noop).
builtin_action_name(parallel_all).
builtin_action_name(parallel_race).
builtin_action_name(parallel_all_running).
builtin_action_name(parallel_race_running).
builtin_action_name(trigger_state_change).
builtin_action_name(define_action).
builtin_action_name(incr).
builtin_action_name(decr).

% ==========================================================
% action_validation_helper/1
% ==========================================================

action_validation_helper(Term) :-
    ( ground(Term) ->
        ( Term = wait(N) ->
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
        ; Term = noop ->
            % Structure matches, no content to validate
            true
        ; Term = spawn(Type, _X, _Y, Acts) ->
            % Structure matches, validate content
            object_type_validation(Type),
            % X and Y: no validation needed
            length(Acts, _)
        ; Term = set_attr(_Name, _Value) ->
            % Structure matches, no validation (2-arg)
            true
        ; Term = set_attr(_TargetID, _Name,
                         _Value) ->
            % Structure matches, no validation (3-arg)
            true
        ; Term = incr(_Key, _Amount) ->
            % Structure matches, no validation (2-arg)
            true
        ; Term = incr(_TargetID, _Key,
                      _Amount) ->
            % Structure matches, no validation (3-arg)
            true
        ; Term = decr(_Key, _Amount) ->
            % Structure matches, no validation (2-arg)
            true
        ; Term = decr(_TargetID, _Key,
                      _Amount) ->
            % Structure matches, no validation (3-arg)
            true
        ; Term = loop(Acts) ->
            % Structure matches, validate content
            ( ground(Acts) ->
                length(Acts, _)
            ;
                true
            )
        ; Term = list(Acts) ->
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
        ; Term = parallel_all(Children) ->
            % Structure matches, validate content
            ( ground(Children) ->
                length(Children, _)
            ;
                true
            )
        ; Term = parallel_all_running(Children) ->
            % Structure matches, validate content
            ( ground(Children) ->
                length(Children, _)
            ;
                true
            )
        ; Term = parallel_race(Children) ->
            % Structure matches, validate
            %   content
            ( ground(Children) ->
                length(Children, _)
            ;
                true
            )
        ; Term = parallel_race_running(Children) ->
            % Structure matches, validate
            %   content
            ( ground(Children) ->
                length(Children, _)
            ;
                true
            )
        ; Term = repeat(Times, Acts) ->
            % Structure matches, validate content
            ( ground(Times) ->
                integer(Times)
            ;
                true
            ),
            ( ground(Acts) ->
                length(Acts, _)
            ;
                true
            )
        ; Term = move_delta(Frames, _DX, _DY) ->
            % Structure matches, validate content
            ( ground(Frames) ->
                integer(Frames)
            ;
                true
            )
        ; Term = define_action(Signature, Body) ->
            % Structure matches, validate content
            % Signature should be callable (a term)
            % TODO: I think highest levet should always be
            %       ground
            % callable(Signature),
            % Body should be a list of actions or a single
            %action
            ( ground(Signature), ground(Body) ->
                ( is_list(Body) ->
                    true
                ; callable(Body) ->
                    true
                ;
                    true  % Allow other structures for
                    % flexibility
                )
            ;
                true
            )
        ; \+ is_builtin_functor(Term),
          callable(Term) ->
            % User-defined action - allow any callable term
            % that is NOT a built-in action functor
            % (validation of actual expansion happens at
            % runtime)
            true
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
    empty_assoc(EmptyAttrs),
    State = state(
        frame(0),
        objects([object(
            id(0), type(static),
            actions([]), collisions([])
        )]),
        attrs(EmptyAttrs),
        status(playing),
        next_id(1),
        commands([]),
        rev_hints([])
    ),
    state_validation(State)
)).

test("game_state_validation: complex state with \
multiple objects and commands", (
    empty_assoc(EmptyAttrs0),
    put_assoc(0, EmptyAttrs0, [attr(x, 10), attr(y, 19)],
              Attrs1),
    put_assoc(1, Attrs1, [attr(x, 5), attr(y, 5)],
              Attrs2),
    put_assoc(2, Attrs2, [attr(x, 15), attr(y, 15)],
              EmptyAttrs),
    State = state(
        frame(5),
        objects([
            object(
                id(0), type(tower),
                actions([wait(3)]), collisions([])
            ),
            object(
                id(1), type(enemy),
                actions([move_to(10, 10, 5)]),
                collisions([])
            ),
            object(
                id(2), type(proj),
                actions([]), collisions([])
            )
        ]),
        attrs(EmptyAttrs),
        status(playing),
        next_id(3),
        commands([
            spawn_request(enemy, 0, 0, []),
            spawn_request(enemy, 0, 0, [])
        ]),
        rev_hints([
            despawned(1, [attr(x, 10), attr(y, 10)])
        ])
    ),
    state_validation(State)
)).

test("context_validation: complex context with multiple \
objects, commands, and rev_hints", (
    empty_assoc(EmptyAttrs0),
    put_assoc(0, EmptyAttrs0, [attr(x, 5), attr(y, 19)],
              Attrs1),
    put_assoc(1, Attrs1, [attr(x, 10), attr(y, 5)],
              Attrs2),
    put_assoc(2, Attrs2, [attr(x, 15), attr(y, 10)],
              EmptyAttrs),
    Ctx = ctx(state(
        frame(42),
        objects([
            object(
                id(0), type(tower),
                actions([
                    loop([
                        wait(3),
                        spawn(proj, 5, 19, [
                            move_to(5, 0, 20)
                        ])
                    ])
                ]), collisions([])
            ),
            object(
                id(1), type(enemy),
                actions([
                    move_to(19, 5, 15),
                    wait(1)
                ]), collisions([])
            ),
            object(
                id(2), type(proj),
                actions([move_to(15, 0, 10)]),
                collisions([])
            ),
            object(
                id(3), type(static),
                actions([
                    parallel_all([
                        wait(5),
                        spawn(enemy, 0, 10, [])
                    ])
                ]), collisions([])
            )
        ]),
        attrs(EmptyAttrs),
        status(playing),
        next_id(4),
        commands([
            spawn_request(enemy, 0, 0, []),
            spawn_request(proj, 10, 10, [
                move_to(10, 0, 10)
            ])
        ]),
        rev_hints([
            despawned(1, [attr(x, 19), attr(y, 5)]),
            despawned(2, [attr(x, 15), attr(y, 0)])
        ])
    )),
    context_validation(Ctx)
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
    State = state(
        frame(0),
        objects([]),
        attrs(EmptyAttrs),
        status(invalid_status),
        next_id(1),
        commands([]),
        rev_hints([])
    ),
    expect_exception(state_validation(State))
)).

test("game_state_validation: non-integer frame fails", (
    empty_assoc(EmptyAttrs),
    State = state(
        frame(not_an_int),
        objects([]),
        attrs(EmptyAttrs),
        status(playing),
        next_id(1),
        commands([]),
        rev_hints([])
    ),
    expect_exception(state_validation(State))
)).

test("game_state_validation: NextID <= max ID fails", (
    Obj = object(
        id(5),
        type(static),
        actions([]),
        collisions([])
    ),
    empty_assoc(EmptyAttrs),
    State = state(
        frame(0),
        objects([Obj]),
        attrs(EmptyAttrs),
        status(playing),
        next_id(5),
        commands([]),
        rev_hints([])
    ),
    expect_exception(state_validation(State))
)).

test("object_validation: wrong structure (ID not \
wrapped) throws", (
    Obj = object(
        5,
        type(static),
        actions([]),
        collisions([])
    ),
    expect_exception(object_validation(Obj))
)).

test("object_validation: invalid object type fails", (
    Obj = object(
        id(0),
        type(invalid_type),
        actions([]),
        collisions([])
    ),
    expect_exception(object_validation(Obj))
)).

test("command_validation: invalid spawn type fails", (
    Cmd = spawn_request(invalid_type, 0, 0, []),
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
    Action1 = spawn(invalid_type, 0, 0, []),
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
    empty_assoc(EmptyAttrs),
    State = state(
        frame(0),
        objects([]),
        attrs(EmptyAttrs),
        status(playing),
        next_id(1),
        commands([])
    ),
    expect_exception(state_validation(State))
)).

test("game_state_validation: wrong functor throws", (
    empty_assoc(EmptyAttrs),
    State = not_state(
        frame(0),
        objects([]),
        attrs(EmptyAttrs),
        status(playing),
        next_id(1),
        commands([]),
        rev_hints([])
    ),
    expect_exception(state_validation(State))
)).

test("object_validation: wrong arity throws", (
    Obj = object(
        id(0), type(static), actions([])
    ),
    expect_exception(object_validation(Obj))
)).

test("object_validation: wrong functor throws", (
    Obj = not_object(
        id(0),
        type(static),
        actions([]),
        collisions([])
    ),
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

test("rev_hint_validation: wrong arity throws", (
    Hint = despawned(1),
    expect_exception(rev_hint_validation(Hint))
)).

test("rev_hint_validation: wrong functor throws", (
    Hint = not_despawned(1, []),
    expect_exception(rev_hint_validation(Hint))
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
