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
%               val_format("ERROR: ...", [Term]),
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

% ==========================================================
% Helper: Conditional format output
% ==========================================================
% val_format/2: Outputs format message only if
% VALIDATION_ERR_MSG is not set to "false"
val_format(Format, Args) :-
    ( catch(getenv("VALIDATION_ERR_MSG", Value), _, fail),
      Value = "false" ->
        true  % Suppress when set to "false"
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
% context_validation/2
% ==========================================================

context_validation(Ctx, Ctx) :-
    Ctx = ctx(State, Input),
    state_validation(State),
    input_validation(Input).

% ==========================================================
% input_validation/1
% ==========================================================

input_validation(Term) :-
    ( ground(Term) ->
        ( input_validation_helper(Term) ->
            true
        ;
            val_format("ERROR: Invalid input:~n  ~w~n",
                   [Term]),
            throw(error(type_error(input, Term),
                        input_validation/1))
        )
    ;
        true
    ).

input_validation_helper(
    input(events(Events), held(Held))
) :-
    is_list(Events),
    is_list(Held).

% ==========================================================
% event_validation/1
% ==========================================================

event_validation(event(key(KeyCode), State)) :-
    integer(KeyCode),
    ( State = down ; State = up ).

% ==========================================================
% state_validation/1
% ==========================================================

state_validation(Term) :-
    ( ground(Term) ->
        ( state_validation_helper(Term) ->
            true
        ;
            val_format("ERROR: Invalid game_state:~n  ~w~n",
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
              commands(Commands)
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
            val_format(
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
            val_format("ERROR: Invalid object:~n  ~w~n",
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
            val_format(
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
           val_format("ERROR: Invalid game_status:~n  ~w~n",
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
              val_format("ERROR: Invalid command:~n  ~w~n",
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
            val_format(
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
            val_format(
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

% Helper: Check if term is an integer or evaluates to an
% integer
% Handles both direct integers (20) and arithmetic
% expressions (-(20), -Amp, etc.)
integer_or_evaluable(Term) :-
    ( integer(Term) ->
        true
    ; catch((Eval is Term, integer(Eval)), _, fail) ->
        true
    ;
        fail
    ).

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
            val_format(
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

% ==========================================================
action_validation(Term) :-
    ( ground(Term) ->
        ( action_validation_helper(Term) ->
            true
        ;
            val_format("ERROR: Invalid action:~n  ~w~n",
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
% Optimized: cache functor names for faster lookup

builtin_functor_name(Name) :-
    builtin_action(Template),
    functor(Template, Name, _).

is_builtin_functor(Term) :-
    callable(Term),
    functor(Term, Name, _Arity),
    builtin_functor_name(Name).

% ==========================================================
% action_validation_helper/1
% ==========================================================

action_validation_helper(Term) :-
    ( ground(Term) ->
        ( Term = wait(N) ->
            % Structure matches, validate content
            integer(N)
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
                integer_or_evaluable(Frames)
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
        ; Term = load(Path) ->
            % Structure matches, validate Path is a string
            is_list(Path)
        ; Term = log(Msg) ->
            % Structure matches, validate Msg is a string
            is_list(Msg)
        ; builtin_action(Term) ->
            % Built-in action - allow it (arity already
            % validated by builtin_action/1)
            true
        ; \+ is_builtin_functor(Term),
          callable(Term) ->
            % User-defined action - allow any callable term
            % that is NOT a built-in action functor
            % (validation of actual expansion happens at
            % runtime)
            true
        ;
            % No pattern matched - throw
            val_format(
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
