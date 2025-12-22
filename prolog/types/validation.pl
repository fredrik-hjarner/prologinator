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
              commands(spawn_cmds(SpawnCmds),
                       fork_cmds(ForkCmds)),
              actionstore(ActionStore)
          ) ->
            % Structure matches, validate content
            integer(Frame),
            length(Objects, _),
            % Attrs is an assoc tree - just check it's
            % ground
            % TODO: Better validation of Attrs
            ground(Attrs),
            game_status_validation(Status),
            integer(NextID),
            % Validate commands structure
            length(SpawnCmds, _),
            length(ForkCmds, _),
            % ActionStore is an assoc tree - just check it's
            % ground
            % TODO: Better validation of ActionStore
            ground(ActionStore),
            % Validate that number of objects matches
            % number of actionstore entries
            length(Objects, NumObjects),
            assoc_to_keys(ActionStore, ActionStoreKeys),
            length(ActionStoreKeys, NumActionStoreEntries),
            ( NumObjects = NumActionStoreEntries ->
                true
            ;
                val_format(
                    "ERROR: Number of objects (~w) does \
not match number of actionstore entries \
(~w)~n",
                    [NumObjects, NumActionStoreEntries]
                ),
                throw(
                    error(
                        validation_error,
                        state_validation_helper/1
                    )
                )
            ),
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
                "ERROR: Invalid game_state structure:~n  \
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
    [object(id(ID))|Rest], [ID|IDs]
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
        ( Term = object(id(ID)) ->
            % Structure matches, validate content
            integer(ID)
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
        ( (spawn_request_validation(Term)
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
            atom(Type),
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
% Helper: Check if term is a valid value specification
% ==========================================================
% A value specification is a term that resolve_action/5
% attempts to resolve before execution.
is_valid_value_spec(Term) :-
    (   number(Term)
    % ;   atom(Term)
    ;   Term = .(_)
    ;   Term = -(_)
    ;   Term = default(_, _)
    ).

% Helper: Check if term is a valid path
is_valid_path(Term) :-
    ( atom(Term)
    ; (compound(Term), functor(Term, '.', 1))
    ; (compound(Term), functor(Term, '.', 2))
    ).

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
            ( integer(N) ->
                true
            ;
                val_format(
                    "ERROR: wait/1: ~w not integer~n",
                    [N]
                ),
                throw(error(
                    type_error(integer, N),
                    action_validation_helper/1
                ))
            )
        ; Term = move_to(X, Y, Frames) ->
            % Structure matches, validate content
            ( is_valid_value_spec(X),
              is_valid_value_spec(Y),
              is_valid_value_spec(Frames) ->
                true
            ;
                val_format(
                   "ERROR: Invalid arg for move_to/3: ~w~n",
                    [Term]
                ),
                throw(error(
                    type_error(value_spec, Term),
                    action_validation_helper/1
                ))
            )
        ; Term = despawn ->
            % Structure matches, no content to validate
            true
        ; Term = noop ->
            % Structure matches, no content to validate
            true
        ; Term = spawn(Acts) ->
            % Structure matches, validate content
            length(Acts, _)
        ; Term = set_attr(Path, _Value) ->
            % Structure matches, validate Path is not a
            % value spec (i.e., it must be a location path)
            is_valid_path(Path)
        ; Term = incr(Path, Amount) ->
            % Structure matches, validate Path (location)
            % and Amount (value spec)
            is_valid_path(Path),
            is_valid_value_spec(Amount)
        ; Term = decr(Path, Amount) ->
            % Structure matches, validate Path (location)
            % and Amount (value spec)
            is_valid_path(Path),
            is_valid_value_spec(Amount)
        ; Term = copy_attr(SourcePath, DestPath) ->
            % Structure matches, validate Paths are not
            % value specs (both locations)
            is_valid_path(SourcePath),
            is_valid_path(DestPath)
        ; Term = loop(Acts) ->
            % Structure matches, validate content
            ( ground(Acts) ->
                length(Acts, _)
            ;
                true
            )
        % loop continuation
        ; Term = loop(Running, Original) ->
            % Structure matches, validate content
            ( ground(Running) ->
                length(Running, _)
            ;
                true
            ),
            ( ground(Original) ->
                length(Original, _)
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
        % repeat continuation
        ; Term = repeat(Times, Running, Original) ->
             % Structure matches, validate content
             ( ground(Times) ->
                 integer(Times)
             ;
                 true
             ),
             ( ground(Running) ->
                 length(Running, _)
             ;
                 true
             ),
             ( ground(Original) ->
                 length(Original, _)
             ;
                 true
             )
        ; Term = move_delta(Frames, DX, DY) ->
            % Structure matches, validate content
            ( is_valid_value_spec(Frames),
              is_valid_value_spec(DX),
              is_valid_value_spec(DY) ->
                true
            ;
                val_format(
                    "ERROR: Invalid arg move_delta/3: ~w~n",
                    [Term]
                ),
                throw(error(
                    type_error(value_spec, Term),
                    action_validation_helper/1
                ))
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
        ; Term = fork(Acts) ->
            % Structure matches, validate content
            ( ground(Acts) ->
                length(Acts, _)
            ;
                true
            )
        ; Term = wait_until(_Cond) ->
             % Structure matches, no validation needed here
             % (Condition validation happens elsewhere)
             true
        ; Term = attr_if(_Cond, Then, Else) ->
             % Structure matches, validate action lists
             ( ground(Then) ->
                 length(Then, _)
             ;
                 true
             ),
             ( ground(Else) ->
                 length(Else, _)
             ;
                 true
             )
        % --- START FIX: Add validation for wait_key_*
        % actions ---
        ; Term = wait_key_down(KeyCode) ->
            integer(KeyCode)
        ; Term = wait_key_up(KeyCode) ->
            integer(KeyCode)
        ; Term = wait_key_held(KeyCode) ->
            integer(KeyCode)
        % --- END FIX ---
        
        % If we reached here, the action failed to match
        % the specific pattern (meaning wrong arity or
        % structure).
        
        % If it's callable but not a built-in functor name,
        % assume user-defined action (success).
        ; ( callable(Term), \+ is_builtin_functor(Term) ) ->
            true
        
        % Check if it has a built-in functor name. If so,
        % it's an invalid built-in action structure/arity.
        ; is_builtin_functor(Term) ->
            val_format(
                "ERROR: Invalid built-in action :~n  ~w~n",
                [Term]
            ),
            throw(
                error(
                    type_error(builtin_action_arity, Term),
                    action_validation_helper/1
                )
            )
        
        % Otherwise, it's an invalid structure (e.g., a bare
        % variable or non-callable term).
        ;
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
