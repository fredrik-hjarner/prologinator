% ==========================================================
% THE PROLOGINATOR MONOLITH BUILD
% ==========================================================

% #define ENABLE_LOG_ACTIONS
% #define ENABLE_VALIDATION

% 1. Global Imports (Import all libraries ONCE)
% Centralized library imports
% Imports all libraries used across the codebase

:- use_module(library(assoc)).
:- use_module(library(charsio)).
:- use_module(library(clpz)).
:- use_module(library(dif)).
:- use_module(library(format)).
:- use_module(library(iso_ext)).
:- use_module(library(lists)).
:- use_module(library(os)).
:- use_module(library(time)).


% 1.5. Discontiguous Declarations (must be before any clauses)
% ==========================================================
% Discontiguous Declarations
% ==========================================================
% All discontiguous predicate declarations for the
% monolithic build
% This file must be included BEFORE any clauses are defined

% 3-arg signature (becomes 5 with DCG)
:- discontiguous(execute_action_impl/5).



% 1.6. Dynamic Declarations (must be before any clauses)
% ==========================================================
% Dynamic Declarations
% ==========================================================
% All dynamic predicate declarations for the monolithic
% build
% This file must be included BEFORE any clauses are defined

:- dynamic(user_action/2).



% 1.7. Operators (must be before any clauses)
% . operator for attribute paths

% prefix: .hp
:- op(101, fy, '.').
% infix: .a.b
:- op(100, yfx, '.').


% 2. Types (foundational - no dependencies on game logic)
% Context Accessors Module
% Provides getter and setter predicates for context data
% structures
%
% This module centralizes all field access patterns to
% ensure consistent access to nested structures and make
% refactoring easier.

% Old convention (deprecated):
% For getters the functors start with the "thing" we get
% from, then comes _ followed by all "stuff" we want to get.
% The fields come in the order in which they occur on the
% "thing"! So try to keep that!
% For setters it's the same, except we add _ followed by the
% "thing" in the end.
% Rationale for covention:
% The functor immediately tells you what to put where.

% New convention (what we're refactoring towards):
% The setters should have CtxIn and CtxOut as last arguments
% so that they can be better used in DCGs.
% The new naming convention is to start the predicate name
% with the entity then followed by all the fields to be set
% on it such as ctx_set_frame, ctx_set_objs_attrs.
% Single fields setters don't use dcg, but bulk setters do
% use dcg.

% ==========================================================
%
%   Context Accessors
%
% ==========================================================

% ==========================================================
% Context Getters
% ==========================================================

% ----------------------------------------------------------
% Single Field Getters
% ----------------------------------------------------------

% getter ctx_get/3 for dcg use to get the whole context
ctx_get(Ctx, Ctx, Ctx).

% getter ctx_frame/3 for dcg use
ctx_frame(F, Ctx, Ctx) :-
    Ctx = ctx(state(frame(F), _, _, _, _, _, _), _).

ctx_objs(O, Ctx, Ctx) :-
    Ctx = ctx(state(_, objects(O), _, _, _, _, _), _).

ctx_attrs(A, Ctx, Ctx) :-
    Ctx = ctx(state(_, _, attrs(A), _, _, _, _), _).

ctx_state(S, Ctx, Ctx) :-
    Ctx = ctx(S, _).

ctx_cmds(commands(SC, FC), Ctx, Ctx) :-
    Ctx = ctx(state(_, _, _, _, _,
                    commands(SC, FC), _), _).

ctx_spawnCmds(SC, Ctx, Ctx) :-
    Ctx = ctx(state(_, _, _, _, _,
                    commands(spawn_cmds(SC),
                             fork_cmds(_)), _), _).

ctx_forkCmds(FC, Ctx, Ctx) :-
    Ctx = ctx(state(_, _, _, _, _,
                    commands(spawn_cmds(_),
                             fork_cmds(FC)), _), _).

ctx_status(S, Ctx, Ctx) :-
    Ctx = ctx(state(_, _, _, status(S), _, _, _), _).

ctx_nextid(N, Ctx, Ctx) :-
    Ctx = ctx(state(_, _, _, _, next_id(N), _, _), _).

ctx_actionstore(AS, Ctx, Ctx) :-
    Ctx = ctx(state(_, _, _, _, _, _,
                    actionstore(AS)), _).

ctx_input(I, Ctx, Ctx) :-
    Ctx = ctx(_, I).

ctx_events(E, Ctx, Ctx) :-
    Ctx = ctx(_, input(events(E), _)).

ctx_held(H, Ctx, Ctx) :-
    Ctx = ctx(_, input(_, held(H))).

% ----------------------------------------------------------
% Bulk Getters
% ----------------------------------------------------------

ctx_objs_cmds(Objs, Cmds) -->
    ctx_objs(Objs),
    ctx_cmds(Cmds).

ctx_objs_attrs(Objs, Attrs) -->
    ctx_objs(Objs),
    ctx_attrs(Attrs).

ctx_status_cmds(Status, Cmds) -->
    ctx_status(Status),
    ctx_cmds(Cmds).

ctx_objs_nextid_cmds(Objs, NextID, Cmds) -->
    ctx_objs(Objs),
    ctx_nextid(NextID),
    ctx_cmds(Cmds).

% ==========================================================
% Context Setters
% ==========================================================

% ----------------------------------------------------------
% Single Field Setters
% ----------------------------------------------------------

ctx_set_frame(F, ctx(state(_, O, A, S, N, C, AS), I),
              ctx(state(frame(F), O, A, S, N, C, AS), I)).

ctx_set_objs(O, ctx(state(F, _, A, S, N, C, AS), I),
             ctx(state(F, objects(O), A, S, N, C, AS), I)).

ctx_set_attrs(A, ctx(state(F, O, _, S, N, C, AS), I),
              ctx(state(F, O, attrs(A), S, N, C, AS), I)).

ctx_set_state(S, ctx(_, I), ctx(S, I)).

ctx_set_cmds(commands(SC, FC),
             ctx(state(F, O, A, S, N, _, AS), I),
             ctx(state(F, O, A, S, N,
                       commands(SC, FC), AS), I)).

ctx_set_spawnCmds(SC,
                  ctx(state(F, O, A, S, N,
                            commands(spawn_cmds(_),
                                     fork_cmds(FC)), AS),
                      I),
                  ctx(state(F, O, A, S, N,
                            commands(spawn_cmds(SC),
                                     fork_cmds(FC)), AS),
                      I)).

ctx_set_forkCmds(FC,
                 ctx(state(F, O, A, S, N,
                           commands(spawn_cmds(SC),
                                    fork_cmds(_)), AS),
                     I),
                 ctx(state(F, O, A, S, N,
                           commands(spawn_cmds(SC),
                                    fork_cmds(FC)), AS),
                     I)).

ctx_set_status(S, ctx(state(F, O, A, _, N, C, AS), I),
               ctx(state(F, O, A, status(S), N, C, AS),
                   I)).

ctx_set_nextid(N, ctx(state(F, O, A, S, _, C, AS), I),
               ctx(state(F, O, A, S, next_id(N), C, AS),
                   I)).

ctx_set_actionstore(AS, ctx(state(F, O, A, S, N, C, _),
                            I),
                    ctx(state(F, O, A, S, N, C,
                              actionstore(AS)), I)).

ctx_set_input(I, ctx(S, _), ctx(S, I)).

% ----------------------------------------------------------
% Bulk Setters (CtxOld, CtxNew as hidden last arguments)
% ----------------------------------------------------------

ctx_set_objs_cmds(Objs, Cmds) -->
    ctx_set_objs(Objs),
    ctx_set_cmds(Cmds).

ctx_set_objs_attrs(Objs, Attrs) -->
    ctx_set_objs(Objs),
    ctx_set_attrs(Attrs).

ctx_set_status_cmds(Status, Cmds) -->
    ctx_set_status(Status),
    ctx_set_cmds(Cmds).

ctx_set_nextid_cmds(NextID, Cmds) -->
    ctx_set_nextid(NextID),
    ctx_set_cmds(Cmds).

ctx_set_objs_nextid_cmds(Objs, NextID, Cmds) -->
    ctx_set_objs(Objs),
    ctx_set_nextid(NextID),
    ctx_set_cmds(Cmds).

% This module centralizes all field access patterns to
% ensure consistent access to nested structures and make
% refactoring easier.

% Convention:
% The setters should have ObjIn and ObjOut as last arguments
% so that they can be better used in DCGs.
% The new naming convention is to start the predicate name
% with the entity then followed by all the fields to be
% get/set on it such as obj_id, obj_set_id.
% The fields come in the order in which they occur on the
% "thing"! So try to keep that!
% Rationale for covention:
% The functor immediately tells you what each argument is.

% ==========================================================
% Object Accessors
% ==========================================================

% Extract ID from object
obj_id(object(id(ID)), ID).


% Constructors Module
% Provides constructors for common empty/default structures.
% Useful for shortening test code substantially.

% ==========================================================
% Context Constructors
% ==========================================================

% Empty context with all default values:
% - frame(0)
% - objects([])
% - attrs(empty assoc)
% - status(playing)
% - next_id(1)
% - commands(spawn_cmds([]), fork_cmds([]))
% - actionstore(empty assoc)
% - input(events([]), held([]))
empty_ctx(ctx(state(
    frame(0),
    objects([]),
    attrs(EmptyAttrs),
    status(playing),
    next_id(1),
    commands(spawn_cmds([]), fork_cmds([])),
    actionstore(EmptyActionStore)
), input(events([]), held([])))) :-
    empty_assoc(EmptyAttrs),
    empty_assoc(EmptyActionStore).

% Creates a context with a provided attribute store.
% All other fields use defaults.
ctx_with_attrs(Attrs, Ctx) :-
    empty_ctx(Def),
    ctx_set_attrs(Attrs, Def, Ctx).

% Creates a context with provided frame and attribute store.
% All other fields use defaults.
ctx_with_frame_attrs(Frame, Attrs, Ctx) :-
    empty_ctx(Def),
    ctx_set_frame(Frame, Def, Ctx1),
    ctx_set_attrs(Attrs, Ctx1, Ctx).

% Creates a context with custom input (events and/or held
% keys). All other fields use defaults.
ctx_with_inputevents_inputheld(Events, Held, Ctx) :-
    empty_ctx(Def),
    ctx_set_input(input(events(Events), held(Held)), Def,
                  Ctx).

% Creates a context with objects. All other fields use
% defaults.
ctx_with_objs(Objects, Ctx) :-
    empty_ctx(Def),
    ctx_set_objs(Objects, Def, Ctx).

% Creates a context with objects and input (events and/or
% held keys). All other fields use defaults.
ctx_with_objs_input(Objects, Events, Held, Ctx) :-
    empty_ctx(Def),
    ctx_set_objs(Objects, Def, Ctx1),
    ctx_set_input(input(events(Events), held(Held)), Ctx1,
                  Ctx).

% Creates a context with frame, objects, and input (events
% and/or held keys). All other fields use defaults.
ctx_with_frame_objs_input(Frame, Objects, Events, Held,
                          Ctx) :-
    empty_ctx(Def),
    ctx_set_frame(Frame, Def, Ctx1),
    ctx_set_objs(Objects, Ctx1, Ctx2),
    ctx_set_input(input(events(Events), held(Held)), Ctx2,
                  Ctx).

% ==========================================================
% Attribute Store Constructors
% ==========================================================

% Alias for empty_assoc/1 for consistency.
empty_attr_store(EmptyAttrs) :-
    empty_assoc(EmptyAttrs).

% #include "../prolog/types/constraints.pl" % no dont import
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

% Advanced Accessors Module
% Provides high-level helpers for working with the
% centralized attribute store. These predicates abstract
% away the details of storage and retrieval from assoc
% trees.

% ==========================================================
% Attribute Store Helpers
% ==========================================================

% Retrieves a specific attribute value for an object from
% the centralized attribute store.
% Fails if the object or attribute doesn't exist. Uses
% ctx_attrs to get the store, then looks up the object's
% attributes and finds the matching key-value.
% getter ctx_attr_val//2 for dcg use
% Bidirectional: ObjectID and Key can be non-ground.
% Uses ObjectID/Key format internally for full
% bidirectionality.
ctx_attr_val(ObjectID/Key, Value) -->
    ctx_attrs(AttrStore),
    {
        gen_assoc(ObjectID, AttrStore, Attrs),
        member(attr(Key, Value), Attrs)
    }.

% getter ctx_attr_val/3 for non-dcg use
ctx_attr_val(ObjectID/Key, Value, Ctx) :-
    % utilize dcg version
    ctx_attr_val(ObjectID/Key, Value, Ctx, Ctx).

% Updates or creates an attribute for an object in the
% centralized store. Returns a new context with the updated
% attribute store. Replaces existing values for the same
% key if they exist, otherwise appends to the object's
% attribute list.
% ctx_set_attr_val(+ObjectID/Key, +Value)
ctx_set_attr_val(ObjectID/Key, Value) -->
    ctx_attrs(AttrStoreIn),
    {
        set_attr_in_store_helper(
            AttrStoreIn, ObjectID, Key, Value, AttrStoreOut
        )
    },
    ctx_set_attrs(AttrStoreOut).

% Helper to update attribute in assoc tree
% Handles the low-level logic of updating the assoc tree:
% removes old attribute values if they exist, then adds the
% new value. Creates a new entry if the object isn't in
% the store yet. This is the internal implementation
% detail for ctx_attr_val_ctx/4.
% Uses ObjectID/Key format for full bidirectionality.
set_attr_in_store_helper(AttrStoreIn, ObjectID, Key,
                         Value, AttrStoreOut) :-
    ( gen_assoc(ObjectID, AttrStoreIn, OldAttrs) ->
        % Remove old value if exists
        ( select(attr(Key, _), OldAttrs, Rest) ->
            true
        ;
            Rest = OldAttrs
        ),
        NewAttrs = [attr(Key, Value)|Rest],
        put_assoc(ObjectID, AttrStoreIn, NewAttrs,
                  AttrStoreOut)
    ;
        % Object doesn't exist in store, create it
        NewAttrs = [attr(Key, Value)],
        put_assoc(ObjectID, AttrStoreIn, NewAttrs,
                  AttrStoreOut)
    ).

% ==========================================================
% Object Type Accessors
% ==========================================================

% Extract type from object (reads from attributes via
%   context)
% DCG version for use with -->
obj_type(Obj, Type) -->
    {obj_id(Obj, ID)},
    ctx_attr_val(ID/type, Type).

% Extract ID and type from object (requires context)
% DCG version for use with -->
obj_id_type(Obj, ID, Type) -->
    {obj_id(Obj, ID)},
    ctx_attr_val(ID/type, Type).

% #include "../prolog/types/validation2.pl" % no dont import

% 3. Utilities and Macros
% Utility functions

catch_dcg(Goal, Catcher, Handler, S0, S) :-
    catch(
        call(Goal, S0, S),
        Catcher,
        Handler
    ).

% ==========================================================
% Partition helper
% ==========================================================
% partition(+Pred, +List, -Yes, -No)
% Splits List into Yes (elements where Pred succeeds)
%   and No (elements where Pred fails)

partition(_Pred, [], [], []).
partition(Pred, [X|Xs], Yes, No) :-
    ( call(Pred, X) ->
        Yes = [X|YesRest],
        partition(Pred, Xs, YesRest, No)
    ;
        No = [X|NoRest],
        partition(Pred, Xs, Yes, NoRest)
    ).

% ==========================================================
% Flatten helper
% ==========================================================
% flatten(+ListOfLists, -FlatList)
% Note: flattening is shallow!
% Flattens a list of lists into a single list
% This is an alias for append/2 from library(lists)

flatten(ListOfLists, FlatList) :-
    append(ListOfLists, FlatList).

% ==========================================================
% Select Many helper
% ==========================================================
% select_many(+Patterns, +List, -Remaining)
% Selects multiple items from List matching Patterns,
% returning Remaining list without those items.
% Patterns is a list of terms to match (e.g., [attr(x, X),
%   attr(y, Y)])
%
% Example:
%   select_many([attr(x, CurrX), attr(y, CurrY)],
%               [attr(x, 5), attr(y, 10), attr(z, 3)],
%               Remaining)
%   binds CurrX=5, CurrY=10, Remaining=[attr(z, 3)]

select_many([], List, List).
select_many([Pattern|Patterns], List, Remaining) :-
    select(Pattern, List, ListWithoutPattern),
    select_many(Patterns, ListWithoutPattern, Remaining).

% ==========================================================
% List Check Helper
% ==========================================================
% is_list(+Term)
% Succeeds if Term is a list ([] or [H|T] where T is a list)

is_list([]).
is_list([_|T]) :- is_list(T).

% ==========================================================
% Pretty Print Helper
% ==========================================================
% pretty_print(+Term)
% Pretty prints Term with indentation (like
% JSON.stringify with 2 spaces)
% Makes complex Prolog terms much easier to read
%
% Example:
%   pretty_print([a, b, [c, d], e(f, g)])
%   Output:
%   [
%     a,
%     b,
%     [
%       c,
%       d
%     ],
%     e(
%       f,
%       g
%     )
%   ]
%
% Note: This function does NOT handle cyclic terms
% (will cause infinite recursion). For cyclic
% terms,
% use write_term/2 with max_depth option instead.
%
% Note: Operators like 1 + 2 will be printed in
% canonical form as +(1, 2). This is intentional
% for structural clarity.

pretty_print(Term) :-
    pretty_print(Term, 0).

% pretty_print(+Term, +IndentLevel)
% Pretty prints Term with IndentLevel spaces of indentation

% 1. Variables
pretty_print(Term, Indent) :-
    var(Term), !,
    print_indent(Indent),
    write(Term).

% 2. Atoms
pretty_print(Term, Indent) :-
    atom(Term), !,
    print_indent(Indent),
    writeq(Term).

% 3. Numbers
pretty_print(Term, Indent) :-
    number(Term), !,
    print_indent(Indent),
    write(Term).

% 4. Empty List
pretty_print([], Indent) :- !,
    print_indent(Indent),
    write('[]').

% 5. Non-Empty Lists
pretty_print([H|T], Indent) :- !,
    print_indent(Indent),
    write('['),
    nl,
    NextIndent is Indent + 1,
    pretty_print(H, NextIndent),
    pretty_print_list_tail(T, NextIndent),
    nl,
    print_indent(Indent),
    write(']').

% 6. Compounds (and Dicts/Strings if supported by dialect)
% Note: Args = [] check is technically dead code
% (compound/1 fails for zero-arity terms), but kept
% for safety/clarity
pretty_print(Term, Indent) :-
    compound(Term), !,
    Term =.. [Functor|Args],
    print_indent(Indent),
    writeq(Functor),
    (   Args = [] -> true
    ;   write('('),
        nl,
        NextIndent is Indent + 1,
        pretty_print_args(Args, NextIndent),
        nl,
        print_indent(Indent),
        write(')')
    ).

% 7. Catch-all (Strings, Blobs, etc.)
pretty_print(Term, Indent) :-
    print_indent(Indent),
    writeq(Term).

% ----------------------------------------------------------
% Helpers
% ----------------------------------------------------------

% Efficient Indentation (2 spaces per level)
print_indent(0).
print_indent(Level) :-
    Level > 0,
    write('  '),
    NextLevel is Level - 1,
    print_indent(NextLevel).

% Handle List Tails (including improper lists [a|b])
pretty_print_list_tail([], _).
pretty_print_list_tail([H|T], Indent) :-
    !, % Cut ensures we don't fall through to
    % improper list handler
    write(','),
    nl,
    pretty_print(H, Indent),
    pretty_print_list_tail(T, Indent).
pretty_print_list_tail(Tail, Indent) :-
    % Handle improper list (dotted pair)
    write(','),
    nl,
    print_indent(Indent),
    write('| '),
    writeq(Tail).

% Handle Compound Arguments
% Note: Indent here is the *Inner* indentation
% level (already calculated)
pretty_print_args([Arg], Indent) :-
    pretty_print(Arg, Indent).
pretty_print_args([Arg|Args], Indent) :-
    pretty_print(Arg, Indent),
    write(','),
    nl,
    pretty_print_args(Args, Indent).

% #include "../prolog/util/test_util.pl" % no dont import
% #include "../prolog/macros.pl" % no dont import
% #include "../prolog/test_macros.pl" % no dont import

% 4. Action Resolution and Builtins
% Value Resolution Module
% Resolves attr() references in actions before execution
%
% This module handles resolution of attr() references in
% actions. When a user writes: set_attr(dest, .x)
% The attr(x) gets resolved to the actual value (e.g., 42)
% before the action executes.

% ==========================================================
% Value Resolution
% ==========================================================
% Resolve all value specs in an action
%
% Example:
%   Input:  set_attr(Path, ValueExpr)
%   Output: set_attr(Path, 42)  (if x = 42)
%
%   Input:  move_to(.target_x, .target_y, 5)
%   Output: move_to(100, 200, 5)
%           (if target_x=100, target_y=200)

% set_attr(Path, ValueExpr) - Path is NOT resolved (it's a
% location), Value IS resolved
resolve_action(
    MyID, set_attr(Path, ValueExpr), set_attr(Path, Value)
) -->
    !,
    resolve_arg(MyID, ValueExpr, Value).

% copy_attr(SrcPath, DestPath) - Neither is resolved (both
% are locations)
resolve_action(
    _MyID, copy_attr(Src, Dest), copy_attr(Src, Dest)
) -->
    !,
    [].

% incr(Path, AmountExpr) - Path is NOT resolved (location),
% Amount IS resolved
resolve_action(
    MyID, incr(Path, AmtExpr), incr(Path, Amt)
) -->
    !,
    resolve_arg(MyID, AmtExpr, Amt).

% decr(Path, AmountExpr) - Path is NOT resolved (location),
% Amount IS resolved
resolve_action(
    MyID, decr(Path, AmtExpr), decr(Path, Amt)
) -->
    !,
    resolve_arg(MyID, AmtExpr, Amt).

% TODO: These prolly don't need to be dcgs because I'm not
%       doing anything with context... but dunno maybe it
%       can be good "just in case"?
% Don't resolve load/1
resolve_action(
    _MyID, load(Path), load(Path)
) -->
    !,
    [].

% Don't resolve wait_until/1
resolve_action(
    _MyID, wait_until(Condition), wait_until(Condition)
) -->
    !,
    [].

resolve_action(
    _MyID,
    attr_if(Condition, ThenActions, ElseActions),
    attr_if(Condition, ThenActions, ElseActions)
) -->
    !,
    [].

resolve_action(
    _MyID,
    spawn(Actions),
    spawn(Actions)
) -->
    !,
    [].

resolve_action(
    _MyID,
    despawn,
    despawn
) -->
    !,
    [].

resolve_action(
    _MyID,
    fork(Actions),
    fork(Actions)
) -->
    !,
    [].

resolve_action(
    MyID,
    wait(Frames),
    wait(ResolvedFrames)
) -->
    !,
    resolve_arg(MyID, Frames, ResolvedFrames).

resolve_action(
    _MyID,
    list(Actions),
    list(Actions)
) -->
    !,
    [].

resolve_action(
    _MyID,
    noop,
    noop
) -->
    !,
    [].

resolve_action(
    MyID,
    move_delta(Frames, DX, DY),
    move_delta(ResolvedFrames, ResolvedDX, ResolvedDY)
) -->
    !,
    resolve_arg(MyID, Frames, ResolvedFrames),
    resolve_arg(MyID, DX, ResolvedDX),
    resolve_arg(MyID, DY, ResolvedDY).

resolve_action(
    MyID,
    move_to(X, Y, Frames),
    move_to(ResolvedX, ResolvedY, ResolvedFrames)
) -->
    !,
    resolve_arg(MyID, X, ResolvedX),
    resolve_arg(MyID, Y, ResolvedY),
    resolve_arg(MyID, Frames, ResolvedFrames).

resolve_action(
    _MyID,
    parallel_all(Actions),
    parallel_all(Actions)
) -->
    !,
    [].

resolve_action(
    _MyID,
    parallel_race(Actions),
    parallel_race(Actions)
) -->
    !,
    [].

resolve_action(
    MyID,
    repeat(Times, Actions),
    repeat(ResolvedTimes, Actions)
) -->
    !,
    resolve_arg(MyID, Times, ResolvedTimes).

% This an an internal repeat (continuation) so all stuff
% should already have been resolved
resolve_action(
    _MyID,
    repeat(Times, A1, A2),
    repeat(Times, A1, A2)
) -->
    !,
    [].

resolve_action(
    _MyID,
    trigger_state_change(Change),
    trigger_state_change(Change)
) -->
    !,
    [].

resolve_action(
    MyID,
    wait_key_down(Key),
    wait_key_down(ResolvedKey)
) -->
    !,
    resolve_arg(MyID, Key, ResolvedKey).

resolve_action(
    MyID,
    wait_key_held(Key),
    wait_key_held(ResolvedKey)
) -->
    !,
    resolve_arg(MyID, Key, ResolvedKey).

resolve_action(
    MyID,
    wait_key_up(Key),
    wait_key_up(ResolvedKey)
) -->
    !,
    resolve_arg(MyID, Key, ResolvedKey).

resolve_action(
    _MyID,
    loop(Actions),
    loop(Actions)
) -->
    !,
    [].

% internal loop (continuation). should not be resolved
% can never be called by user and stuff should already have
% been resolved.
resolve_action(
    _MyID,
    loop(Running, Original),
    loop(Running, Original)
) -->
    !,
    [].

% TODO: It's retarded that `resolve_action` only takes ONE
% actions as it's 2nd argument and not a list of actions.
resolve_action(
    _MyID,
    define_action(Name, Action),
    define_action(Name, Action)
) -->
    !,
    [].

resolve_action(
    _MyID,
    log(Message),
    log(Message)
) -->
    !,
    [].

% base case for builtins. should raise exception because all
% builtins should be resolved.
resolve_action(
    _MyID,
    Action,
    Action
) -->
    {builtin_action(Action)},
    !,
    {format(
        "resolve_action not implemented for: ~w~n",
        [Action]
    )},
    {throw(not_implemented(resolve_action/3))}.

% base case for non-builtin actions, i.e. custom
% user-defined actions. should never be resolved, I think...
% because it's the built-in actions themselves that does the
% resolution.
resolve_action(
    _MyID,
    Action,
    Action
) -->
    !,
    [].

% Helper: resolve list of arguments, threading context
resolve_args(_MyID, [], []) --> [].
resolve_args(MyID, [Arg|Rest], [ResArg|ResRest]) -->
    resolve_arg(MyID, Arg, ResArg),
    resolve_args(MyID, Rest, ResRest).

% Resolve individual arguments
%
% If the argument is prefixed with ., it is treated as a
% path reference that must resolve to a value (read
% context).
resolve_arg(MyID, .Path, V) -->
    !,
    (   {ground(Path)} % Only resolve if path is ground
    % Use strict path resolution to get the value
    ->  resolve_path_strict(MyID, Path, V)
    ;   {V = .Path}
    ).


% NEW: Handle default/2 expressions
resolve_arg(MyID, default(ValueExpr, Fallback), V) -->
    !,
    resolve_default(MyID, ValueExpr, Fallback, V).

% Lists need recursive resolution
% TODO: Do I even need this? I don't think I ever need this
%       in current design.
resolve_arg(MyID, List, ResolvedList) -->
    {List = [_|_]},
    !,
    resolve_args(MyID, List, ResolvedList).
resolve_arg(_MyID, [], []) --> [].

% Catch-all?
% Pass through primitives and other terms
resolve_arg(_MyID, Other, Other) --> !, [].

% Helper for default/2 resolution in actions
resolve_default(MyID, .Path, Fallback, V) -->
    {ground(Path)},
    !,
    ( resolve_path_strict(MyID, Path, ResolvedValue) ->
        {V = ResolvedValue}
    ;
        % If strict path resolution fails (due to missing
        % attribute),
        % use the fallback. Context remains unchanged.
        {V = Fallback}
    ).

% Catch-all?
% If ValueExpr is not an .Path, resolve it normally.
% Note: This handles cases like default(10, 0) -> 10
resolve_default(MyID, ValueExpr, _Fallback, V) -->
    !,
    resolve_arg(MyID, ValueExpr, V).

% ==========================================================
% Path Resolution (DCG version: handles x.y.z chains)
% NOTE: resolve_path_strict//3 and resolve_path_to_attr//3
% definitions are now consolidated in
% prolog/conditions/path_resolution.pl to avoid
% discontiguous warnings.
% ==========================================================

% Built-in Actions Module
% Defines which actions are built-in (with arity checking)

% Check if an action is a built-in action (with arity
% checking)
builtin_action(wait(_)).
builtin_action(move_to(_, _, _)).
builtin_action(move_delta(_, _, _)).
builtin_action(despawn).
builtin_action(spawn(_)).
builtin_action(set_attr(_, _)).
builtin_action(copy_attr(_, _)).
builtin_action(incr(_, _)).
builtin_action(decr(_, _)).
builtin_action(loop(_)).
builtin_action(loop(_, _)). % loop continuation
builtin_action(list(_)).
builtin_action(repeat(_, _)).
builtin_action(repeat(_, _, _)). % repeat continuation
builtin_action(noop).
builtin_action(parallel_all(_)).
builtin_action(parallel_race(_)).
builtin_action(parallel_all_running(_)).
builtin_action(trigger_state_change(_)).
builtin_action(define_action(_, _)).
builtin_action(load(_)).
builtin_action(log(_)).
builtin_action(wait_key_down(_)).
builtin_action(wait_key_up(_)).
builtin_action(wait_key_held(_)).
builtin_action(wait_until(_)).
builtin_action(fork(_)).
builtin_action(attr_if(_, _, _)).


% 5. Execute Action (declares multifile, must come before actions)
% Action Execution Module
% Handles execution of all game actions
% Core interface - implementations are in separate modules

% Multifile declaration for execute_action_impl/5
% MUST be declared before loading implementation modules
% Implementation clauses are provided by the modules below
% NOTE: discontiguous declaration is now in
% prolog/discontiguous.pl

% Load all action implementation modules (after multifile
%   declaration)

% ==========================================================
% Custom Actions: Runtime Expansion
% ==========================================================

% Dynamic predicate to store user-defined action definitions
% NOTE: dynamic declaration is now in prolog/dynamic.pl

% ==========================================================
% Top-level execute_action (with resolution)
% ==========================================================
execute_action(
    actions_old([Action|Rest]),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    % TODO: Add toggle to enable/disable debug logging here
    % {Action =.. [Functor|_]},
    % {format("execute_action: ~w~n", [Functor])},
    % 1. Resolve value specs in action
    resolve_action(ID, Action, ResolvedAction),
    % 2. Delegate to existing logic (renamed)
    % We need to put the ResolvedAction as new head on
    % actions_old
    execute_action_resolved(
        actions_old([ResolvedAction|Rest]),
        obj_id(ID),
        result(Status, actions_new(ActionsOut))
    ).

% ==========================================================
% execute_action_resolved
% ==========================================================
% Wrapper: validates action then delegates to implementation
% Now threads Context directly to accumulate side effects.
% Also handles user-defined actions via runtime expansion.
execute_action_resolved(
    actions_old([Action|Rest]),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    % TODO: action_validation nowadays it almost useless
    %       since custom actions allow anything.
    %       so maybe I should validate builtins separately?

    % validate(Action, action_schema),
    ( {builtin_action(Action)} ->
        % It's a built-in action - execute normally

        % execute_action_impl(
        %     actions_old([Action|Rest]),
        %     obj_id(ID),
        %     result(Status, actions_new(ActionsOut))
        % )
        catch_dcg(
            execute_action_impl(
                actions_old([Action|Rest]),
                obj_id(ID),
                result(Status, actions_new(ActionsOut))
            ),
            Error,
            ( write('Error during execute_action_impl: '),
              write(Error), nl,
              throw(Error)
            )
        )
    ; {user_action(Action, Body)} -> % total prolog voodoo!
        % It's a user-defined action!
        % Action unifies with Template, binding variables
        % in Body automatically
        % Body now has the correct bindings, use it directly
        % Note: Body may contain attr() specs, so recurse
        % through top-level execute_action for resolution
        % Replace the user action with its Body in the queue
        execute_action(
            actions_old([Body|Rest]),
            obj_id(ID),
            result(Status, actions_new(ActionsOut))
        )
    ;
        % Unknown action
        {throw(unknown_action(Action))}
    ).

% ==========================================================
% execute_action_impl/5
% ==========================================================
% Internal implementation (no validation)
% Implementation clauses are provided by individual action
%   files in ./actions/ directory (one file per action)
% ==========================================================



% 6. Action Implementations (all actions)
% =========================================================
% attr_if Action
% =========================================================
% Conditionally executes one of two action lists based on
% a condition.
%
% Syntax: attr_if(Condition, ThenActions, ElseActions)
%
% Semantics:
%   - Checks Condition immediately (non-blocking)
%   - If true: executes ThenActions
%   - If false: executes ElseActions
%   - Always completes in the same frame (instant)
%
% Condition supports same forms as wait_until:
%   - Comparisons: hp < 0, parent_id/hp >= 10
%   - Membership: sword in inventory
%   - Logical: and([...]), or([...]), not(...)

execute_action_impl(
    actions_old([
        attr_if(Condition, ThenActions, ElseActions)|Rest
    ]),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    execute_attr_if(
        ID,
        Condition,
        ThenActions,
        ElseActions,
        Rest,
        result(Status, actions_new(ActionsOut))
    ).

% ==========================================================
% Implementation: execute_attr_if
% ==========================================================
% Immediate conditional: checks condition once and
% branches. No yielding.

execute_attr_if(
    ObjID,
    Condition,
    ThenActions,
    ElseActions,
    Rest,
    result(Status, actions_new(ActionsOut))
) -->
    ctx_get(Ctx),
    % TODO: Check condition should be made to work with dcgs
    %       by taking two Ctxs as last 2 args.
    (   {check_condition(Ctx, ObjID, Condition)}
    ->
        % Condition succeeded: execute ThenActions
        tick_object(
            actions_old(ThenActions),
            obj_id(ObjID),
            result(Status, actions_new(ActionAfterTick))
        ),
        {append(ActionAfterTick, Rest, ActionsOut)}
    ;
        % Condition failed: execute ElseActions
        tick_object(
            actions_old(ElseActions),
            obj_id(ObjID),
            result(Status, actions_new(ActionAfterTick))
        ),
        {append(ActionAfterTick, Rest, ActionsOut)}
    ).

execute_action_impl(
    actions_old([wait(N)|Rest]),
    obj_id(_ID),
    result(Status, actions_new(ActionsOut))
) -->
    execute_wait(N, Rest, Status, ActionsOut).

execute_wait(N, Rest, Status, ActionsOut) -->
    {
        ( N = 0 ->
            % wait(0): noop, removes itself, completes
            ActionsOut = Rest,
            Status = completed
        ; N = 1 ->
            % wait(1): yields but also removes itself
            ActionsOut = Rest,
            Status = yielded
        ; N #> 1 ->
            % wait(N>1): decrement and yield
            N1 #= N - 1,
            ActionsOut = [wait(N1)|Rest],
            Status = yielded
        )
    }.

execute_action_impl(
    actions_old([move_to(TargetX, TargetY, Frames)|Rest]),
    obj_id(ID),
    result(Status, actions_new(NewActions))
) -->
    execute_move_to(
        TargetX,
        TargetY,
        Frames,
        ID,
        Rest,
        Status,
        NewActions
    ).

% 0 frames: teleport instantly to target
execute_move_to(
    TargetX,
    TargetY,
    0,
    ID,
    Rest,
    completed,
    Rest
) -->
    ctx_set_attr_val(ID/x, TargetX),
    ctx_set_attr_val(ID/y, TargetY).

% 1+ frames: compute step and continue or finish
execute_move_to(
    TargetX,
    TargetY,
    Frames,
    ID,
    Rest,
    Status,
    NewActions
) -->
    % Get current position from attribute store
    % Fails if object doesn't have x/y attributes
    ctx_attr_val(ID/x, CurrX),
    ctx_attr_val(ID/y, CurrY),
    % Compute step using integer division
    {DX #= (TargetX - CurrX) // Frames,
     DY #= (TargetY - CurrY) // Frames,
     NewX #= CurrX + DX,
     NewY #= CurrY + DY},
    % Label at the boundary where we need ground values for
    % game objects
    {(ground(TargetX), ground(TargetY), ground(CurrX),
      ground(CurrY), ground(Frames) ->
        labeling([], [NewX, NewY])
    ;
        true
    )},
    % Update position in attribute store
    ctx_set_attr_val(ID/x, NewX),
    ctx_set_attr_val(ID/y, NewY),
    {( Frames #> 1 ->
        Frames1 #= Frames - 1,
        NewActions = [
            move_to(TargetX, TargetY, Frames1)|Rest],
        Status = yielded
    ;
        NewActions = Rest,  % Arrived
        % We arrived, but this movement took 1 frame of
        % time.
        % We must yield execution to the next frame.
        Status = yielded
    )}.

% move_delta(+Frames, +DX, +DY)
% Mode: move_delta(+Frames, +DX, +DY)
% Description: Relative movement: move by (DX, DY)
%   each frame for Frames frames
% Yields: true when Frames > 0

execute_action_impl(
    actions_old([move_delta(Frames, DX, DY)|Rest]),
    obj_id(ID),
    result(Status, actions_new(NewActions))
) -->
    execute_move_delta(
        Frames,
        DX,
        DY,
        ID,
        Rest,
        Status,
        NewActions
    ).

% 0 frames: teleport instantly by delta
execute_move_delta(
    0,
    DX,
    DY,
    ID,
    Rest,
    completed,
    Rest
) -->
    % Get current position from attribute store
    ( ctx_attr_val(ID/x, CurrX),
      ctx_attr_val(ID/y, CurrY) ->
        []
    ;
        % Default to 0,0 if not set
        {CurrX = 0, CurrY = 0}
    ),
    % Apply delta instantly
    {NewX #= CurrX + DX,
     NewY #= CurrY + DY},
    % Label for grounding
    {( ground(CurrX), ground(CurrY), ground(DX), ground(DY)
    ->
        labeling([], [NewX, NewY])
    ;
        true
    )},
    % Update position in attribute store
    ctx_set_attr_val(ID/x, NewX),
    ctx_set_attr_val(ID/y, NewY).

% 1+ frames: apply delta and continue or finish
execute_move_delta(
    Frames,
    DX,
    DY,
    ID,
    Rest,
    Status,
    NewActions
) -->
    % Get current position from attribute store
    ( ctx_attr_val(ID/x, CurrX),
      ctx_attr_val(ID/y, CurrY) ->
        []
    ;
        % Default to 0,0 if not set
        {CurrX = 0, CurrY = 0}
    ),
    % Apply delta
    {NewX #= CurrX + DX,
     NewY #= CurrY + DY},
    % Label for grounding
    {( ground(CurrX), ground(CurrY), ground(DX),
      ground(DY) ->
        labeling([], [NewX, NewY])
    ;
        true
    )},
    % Update position in attribute store
    ctx_set_attr_val(ID/x, NewX),
    ctx_set_attr_val(ID/y, NewY),
    % Continue or arrive
    {( Frames #> 1 ->
        Frames1 #= Frames - 1,
        NewActions = [move_delta(Frames1, DX, DY)|Rest],
        Status = yielded
    ;
        NewActions = Rest,
        % We arrived, but this movement took 1 frame of
        % time.
        % We must yield execution to the next frame.
        Status = yielded
    )}.

execute_action_impl(
    actions_old([despawn|_]),
    obj_id(ID),
    result(despawned, actions_new([]))
) -->
    execute_despawn(ID).

execute_despawn(ID) -->
    % Remove object's attributes from store
    ctx_attrs(AttrStore),
    {
        ( gen_assoc(ID, AttrStore, _Attrs) ->
            del_assoc(ID, AttrStore, _, NewAttrStore)
        ;
            NewAttrStore = AttrStore
        )
    },
    ctx_set_attrs(NewAttrStore).



execute_action_impl(
    actions_old([noop|Rest]),
    obj_id(_ID),
    result(completed, actions_new(Rest))
) -->
    execute_noop.

execute_noop --> [].

% Defines a custom action macro at runtime. Stores the
% Signature->Body mapping in user_action/2. When Signature
% is called later, execute_action/5 will expand it with
% parameter substitution and execute the Body.
execute_action_impl(
    actions_old([define_action(Signature, Body)|Rest]),
    obj_id(_ID),
    result(completed, actions_new(Rest))
) -->
    execute_define_action(Signature, Body).

execute_define_action(Signature, Body) -->
    {assertz(user_action(Signature, Body))}.



execute_action_impl(
    actions_old([set_attr(Path, Value)|Rest]),
    obj_id(MyID),
    result(completed, actions_new(Rest))
) -->
    execute_set_attr(MyID, Path, Value).

execute_set_attr(MyID, Path, Value) -->
    resolve_path_to_attr(MyID, Path, TargetID/Key),
    ctx_set_attr_val(TargetID/Key, Value).



execute_action_impl(
    actions_old([copy_attr(SourcePath, DestPath)|Rest]),
    obj_id(MyID),
    result(completed, actions_new(Rest))
) -->
    execute_copy_attr(MyID, SourcePath, DestPath).

execute_copy_attr(MyID, SourcePath, DestPath) -->
    resolve_path_to_attr(MyID, SourcePath,
                         SourceID/SourceKey),
    resolve_path_to_attr(MyID, DestPath,
                         DestID/DestKey),
    ctx_attr_val(SourceID/SourceKey, Value),
    ctx_set_attr_val(DestID/DestKey, Value).

execute_action_impl(
    actions_old([incr(Path, Amount)|Rest]),
    obj_id(MyID),
    result(completed, actions_new(Rest))
) -->
    execute_incr(MyID, Path, Amount).

execute_incr(MyID, Path, Amount) -->
    resolve_path_to_attr(MyID, Path, TargetID/Key),
    ( ctx_attr_val(TargetID/Key, CurrentValue) ->
        {NewValue #= CurrentValue + Amount}
    ;
        {NewValue = Amount}
    ),
    ctx_set_attr_val(TargetID/Key, NewValue).

execute_action_impl(
    actions_old([decr(Path, Amount)|Rest]),
    obj_id(MyID),
    result(completed, actions_new(Rest))
) -->
    execute_decr(MyID, Path, Amount).

execute_decr(MyID, Path, Amount) -->
    resolve_path_to_attr(MyID, Path, TargetID/Key),
    ( ctx_attr_val(TargetID/Key, CurrentValue) ->
        {NewValue #= CurrentValue - Amount}
    ;
        {NewValue #= 0 - Amount}
    ),
    ctx_set_attr_val(TargetID/Key, NewValue).

execute_action_impl(
    actions_old([log(Msg)|Rest]),
    obj_id(_ID),
    result(completed, actions_new(Rest))
) -->
    execute_log(Msg).

execute_log(Msg) -->
    {format("~s~n", [Msg])}.



execute_action_impl(
    actions_old([spawn(Actions)|Rest]),
    obj_id(MyID),
    result(completed, actions_new(Rest))
) -->
    execute_spawn(MyID, Actions).

% NOTE: Important to notice that spawn_cmd:s are added in
%       reverse for performance reasons.
execute_spawn(ParentID, Actions) -->
    % Build actions list with parent_id automatically added
    {SpawnActions = [
        set_attr(parent_id, ParentID)
        | Actions
    ]},
    % Add spawn command instead of directly modifying
    % actionstore
    ctx_spawnCmds(SpawnCmdsOld),
    {SpawnCmdsNew = [
        spawn_cmd(actions(SpawnActions))
        | SpawnCmdsOld
    ]},
    ctx_set_spawnCmds(SpawnCmdsNew).



execute_action_impl(
    actions_old([fork(Actions)|Rest]),
    obj_id(ID),
    result(completed, actions_new(Rest))
) -->
    execute_fork(ID, Actions).

% NOTE: Important to notice that fork_cmd:s are added in
%       reverse for performance reasons.
execute_fork(ObjID, Actions) -->
    % Add fork command instead of directly modifying
    % actionstore
    ctx_forkCmds(ForkCmdsOld),
    {ForkCmdsNew = [
        fork_cmd(obj_id(ObjID), actions(Actions))
        | ForkCmdsOld
    ]},
    ctx_set_forkCmds(ForkCmdsNew).


% loop(+Actions)
% Mode: loop(+Actions)
% Description: Executes Actions repeatedly.
%   Self-managed: threads the state of the loop body across
%   frames.
%   If the body finishes in a tick, it restarts immediately
%   in the same tick.

% Initial call: loop(Actions)
execute_action_impl(
    actions_old([loop(Actions)|Rest]),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    % Start the loop with Actions as both Running and
    %Original
    execute_loop_managed(
        Actions, Actions, Rest, ID, Status, ActionsOut
    ).

% Continuation: loop(Running, Original)
execute_action_impl(
    actions_old([loop(Running, Original)|Rest]),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    execute_loop_managed(
        Running, Original, Rest, ID, Status, ActionsOut
    ).

execute_loop_managed(
    Running, Original, Rest, ID, Status, ActionsOut
) -->
    % Execute the current running actions (threads context)
    tick_object(
        actions_old(Running),
        obj_id(ID),
        result(RunStatus, actions_new(RunRemaining))
    ),
    handle_loop_result(
        RunStatus,
        RunRemaining,
        Original,
        Rest,
        ID,
        Status,
        ActionsOut
    ).

% Case 1: Despawned
handle_loop_result(
    despawned, _, _, _, _, despawned, []
) --> [].

% Case 2: Yielded - Body yielded, so we yield the loop state
handle_loop_result(
    yielded,
    RunRemaining,
    Original,
    Rest,
    _,
    yielded,
    [loop(RunRemaining, Original)|Rest]
) --> [].

% Case 3: Completed - Body finished, restart loop
% immediately
handle_loop_result(
    completed, _, Original, Rest, ID, Status, ActionsOut
) -->
    execute_loop_managed(
        Original, Original, Rest, ID, Status, ActionsOut
    ).

execute_action_impl(
    actions_old([list(ListActions)|RestActions]),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    execute_list(
        ListActions,
        RestActions,
        ID,
        Status,
        ActionsOut
    ).

execute_list(
    ListActions,
    RestActions,
    ID,
    Status,
    ActionsOut
) -->
    % Execute the inner list until it yields, completes,
    % or despawns
    tick_object(
        actions_old(ListActions),
        obj_id(ID),
        result(ListStatus, actions_new(RemainingInner))
    ),
    % Handle the result
    ( {ListStatus = despawned} ->
        {Status = despawned, ActionsOut = []}
    ; {ListStatus = yielded} ->
        % The inner list paused. Wrap remaining actions back
        % in list() and keep them at the head of the queue.
        {ActionsOut = [list(RemainingInner)|RestActions],
         Status = yielded}
    ; % ListStatus = completed
        % The inner list finished. Continue with the rest of
        % the original queue.
        {ActionsOut = RestActions,
         Status = completed}
    ).



% repeat(+Times, +Actions)
% Mode: repeat(+Times, +Actions)
% Description: Execute action list N times.
%   Self-managed: threads the state of the repetition across
%   frames.
%   If the body finishes in a tick, it proceeds to the next
%   repetition immediately.

% TODO: I prefer no implementation in execute_action_impl
% but to have execute_action_impl simply pass stuff
% to execute_${actionName} to do the implementation.
execute_action_impl(
    actions_old([repeat(0, _)|Rest]),
    _,
    result(completed, actions_new(Rest))
) --> [].

% Initial call: repeat(Times, Actions)
execute_action_impl(
    actions_old([repeat(Times, Actions)|Rest]),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    % Constraint from original: Times > 0
    {Times #> 0},
    execute_repeat_managed(
        Times,
        Actions,
        Actions,
        Rest,
        ID,
        Status,
        ActionsOut
    ).

% Continuation: repeat(RemainingTimes, Running, Original)
execute_action_impl(
    actions_old([repeat(Times, Running, Original)|Rest]),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    execute_repeat_managed(
        Times,
        Running,
        Original,
        Rest,
        ID,
        Status,
        ActionsOut
    ).

execute_repeat_managed(
    Times, Running, Original, Rest, ID, Status, ActionsOut
) -->
    % Execute the current running actions (threads context)
    tick_object(
        actions_old(Running),
        obj_id(ID),
        result(RunStatus, actions_new(RunRemaining))
    ),
    handle_repeat_result(
        RunStatus,
        RunRemaining,
        Times,
        Original,
        Rest,
        ID,
        Status,
        ActionsOut
    ).

% Case 1: Despawned
handle_repeat_result(
    despawned, _, _, _, _, _, despawned, []
) --> [].

% Case 2: Yielded - Body yielded, update state
handle_repeat_result(
    yielded,
    RunRemaining,
    Times, Original,
    Rest,
    _,
    yielded, [repeat(Times, RunRemaining, Original)|Rest]
) --> [].

% Case 3: Completed - Body finished
handle_repeat_result(
    completed,
    _,
    Times,
    Original,
    Rest,
    ID,
    Status,
    ActionsOut
) -->
    {Times1 #= Times - 1},
    ( {Times1 #> 0} ->
        % More repetitions needed, recurse immediately
        execute_repeat_managed(
            Times1,
            Original,
            Original,
            Rest,
            ID,
            Status,
            ActionsOut
        )
    ;
        % All repetitions done
        {Status = completed, ActionsOut = Rest}
    ).

% load(+Path)
% Mode: load(+Path)
% Description: Loads a file containing a list of actions
%   and prepends them to the action queue
% Yields: false (expands immediately)

execute_action_impl(
    actions_old([load(Path)|Rest]),
    obj_id(_ID),
    result(completed, actions_new(NewActions))
) -->
    execute_load(Path, Rest, NewActions).

execute_load(Path, Rest, NewActions) -->
    {
        % Convert Path (list of chars) to atom for open/3
        atom_chars(PathAtom, Path),
        % Read file as term (expects list of actions)
        setup_call_cleanup(
            open(PathAtom, read, Stream),
            read_term(Stream, Actions, []),
            close(Stream)
        ),
        % Validate it's a list
        ( is_list(Actions) ->
            append(Actions, Rest, NewActions)
        ;
            throw(error(
                type_error(list, Actions),
                load/1
            ))
        )
    }.



execute_action_impl(
    actions_old([trigger_state_change(Change)|Rest]),
    obj_id(_ID),
    result(completed, actions_new(Rest))
) -->
    execute_trigger_state_change(Change).

execute_trigger_state_change(Change) -->
    ctx_status(CurrentStatus),
    {update_status(Change, CurrentStatus, NewStatus)},
    ctx_set_status(NewStatus).

update_status(game_over(lost), _, lost).
update_status(game_over(won), lost, lost).
update_status(game_over(won), _, won).



% wait_key_down(+KeyCode)
% Mode: wait_key_down(+KeyCode)
% Description: Waits until specified key is pressed
%   (detects 'down' event for the key)
% Yields: false (checks each frame)

execute_action_impl(
    actions_old([wait_key_down(KeyCode)|Rest]),
    obj_id(_ID),
    result(Status, actions_new(ActionsOut))
) -->
    execute_wait_key_down(
        KeyCode,
        Rest,
        Status,
        ActionsOut
    ).

execute_wait_key_down(KeyCode, Rest, Status, ActionsOut) -->
    % Check if key was pressed THIS frame
    ( key_down(KeyCode) ->
        % Key pressed: action complete
        {ActionsOut = Rest, Status = completed}
    ;
        % Key not pressed: keep waiting
        {ActionsOut = [wait_key_down(KeyCode)|Rest],
         Status = yielded}
    ).



% wait_key_up(+KeyCode)
% Mode: wait_key_up(+KeyCode)
% Description: Waits until specified key is released
%   (detects 'up' event for the key)
% Yields: false (checks each frame)

execute_action_impl(
    actions_old([wait_key_up(KeyCode)|Rest]),
    obj_id(_ID),
    result(Status, actions_new(ActionsOut))
) -->
    execute_wait_key_up(KeyCode, Rest, Status, ActionsOut).

execute_wait_key_up(KeyCode, Rest, Status, ActionsOut) -->
    % Check if key released THIS frame
    ( key_up(KeyCode) ->
        % Key released: action complete
        {ActionsOut = Rest, Status = completed}
    ;
        % Key still pressed: keep waiting
        {ActionsOut = [wait_key_up(KeyCode)|Rest],
         Status = yielded}
    ).



% wait_key_held(+KeyCode)
% Mode: wait_key_held(+KeyCode)
% Description: Yields every frame while key is held
%   Use in loop: loop([wait_key_held(39), move(...)])
% Yields: true when key is held, false otherwise

execute_action_impl(
    actions_old([wait_key_held(KeyCode)|Rest]),
    obj_id(_ID),
    result(Status, actions_new(ActionsOut))
) -->
    execute_wait_key_held(
        KeyCode,
        Rest,
        Status,
        ActionsOut
    ).

execute_wait_key_held(KeyCode, Rest, Status, ActionsOut) -->
    ( key_held(KeyCode) ->
        % {format("Key held: ~w~n", [KeyCode])},
        % Key held: yield (action complete)
        {ActionsOut = Rest, Status = completed}
    ;
        % {format("Key not held: ~w~n", [KeyCode])},
        % Key not held: keep waiting
        {ActionsOut = [wait_key_held(KeyCode)|Rest],
         Status = yielded}
    ).



% wait_until(+Condition)
% Description: Waits until the specified condition is
%   satisfied.
%   Supports comparisons, logical composition, and list
%   membership.
%   Yields until the condition is true, then completes.
%
% Examples:
%   wait_until(hp < 0)
%   wait_until(parent_id/hp <= 0)
%   wait_until(sword in inventory)
%   wait_until(and([hp < 0, not(invulnerable = 1)]))

execute_action_impl(
    actions_old([wait_until(Condition)|Rest]),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    execute_wait_until(
        ID, Condition, Rest, Status, ActionsOut
    ).

execute_wait_until(
    ID, Condition, Rest, Status, ActionsOut
) -->
    ctx_get(Ctx),
    % Try to check condition
    ( {check_condition(Ctx, ID, Condition)} ->
        % Condition satisfied - proceed
        {ActionsOut = Rest, Status = completed}
    ;
        % Condition not satisfied - wait (keep action in
        % queue)
        {ActionsOut = [wait_until(Condition)|Rest],
         Status = yielded}
    ).

% ==========================================================
% Interface
% ==========================================================

execute_action_impl(
    actions_old([parallel_all(Children)|RestActions]),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    % Execute children sequentially, threading the object
    % state. Stop immediately if any child despawns.
    tick_children(Children, ID, Result),
    {( Result = despawned ->
        Status = despawned,
        ActionsOut = []
    ; Result = remaining(RemainingChildren) ->
        ( RemainingChildren = [] ->
            % All children finished actions.
            Status = completed,
            ActionsOut = RestActions
        ;
            % Some children yielded or are still running.
            % We yield the parallel_all action itself to
            % continue next tick.
            Status = yielded,
            ActionsOut = [parallel_all(
                RemainingChildren)|RestActions]
        )
    )}.

% ==========================================================
% Helpers
% ==========================================================

% tick_children(+Children, +ID, -Result, +CtxIn, -CtxOut)
% Result's either 'despawned' or 'remaining(List)'
% Threads the object state (attributes) through each child
% execution via context.

tick_children([], _ID, remaining([])) --> [].

% NOTE: Only the upper-/outer-most actions are executed in
% parallel so to speak. If those are composite actions then
% the composite actions tend to be executed "as normal"
% (unless of course if such an action happens to be a
% parallel_all, parallel_race or similar)
tick_children(
    [FirstTopLayerAcn|TopLayerAcnsRest],
    ID,
    Result
) -->
    % Normalize child to a list of actions (handle single
    % action atoms)
    {( FirstTopLayerAcn = [_|_] ->
        FirstTopLayerAcnList = FirstTopLayerAcn
    ;
        FirstTopLayerAcnList = [FirstTopLayerAcn]
    )},
    % Tick the actions (executes until yield, despawn, or
    % complete)
    tick_object(
        actions_old(FirstTopLayerAcnList),
        obj_id(ID),
        result(
            ChildStatusAfterTick,
            actions_new(RemainingAcnsAfterTick)
        )
    ),
    % So after tick the `remaining actions` in
    % RemainingAcnsAfterTick.
    ( {ChildStatusAfterTick = despawned} ->
        % Immediate exit on despawn
        {Result = despawned}
    ;
        % FirstTopLayerAcn yielded or completed.
        % Attributes are updated in context automatically.
        % We collect remaining actions and continue with
        % next child.
        tick_children(
            TopLayerAcnsRest,
            ID,
            % RestResult contains all remaining actions
            % of the top level actions (except the first
            % which we already executed above) have each
            % been ticked, having been built up by the
            % code beneath.
            RestResult
        ),
        {( RestResult = despawned ->
            Result = despawned
        ; RestResult = remaining(RestRemaining) ->
            ( RemainingAcnsAfterTick = [] ->
                Result = remaining(RestRemaining)
            ;
                append(
                    RemainingAcnsAfterTick,
                    RestRemaining,
                    AllRemaining
                ),
                Result = remaining(AllRemaining)
            )
        )}
    ).



execute_action_impl(
    actions_old([parallel_race(Children)|RestActions]),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    % Execute children sequentially (stop immediately
    % if one finishes).
    tick_children_race(Children, ID, Result),
    {( Result = despawned ->
        Status = despawned,
        ActionsOut = []
    ; Result = race_won ->
        % A child finished! The race is won.
        % We discard losers' actions and restore parent
        % actions.
        Status = completed,
        ActionsOut = RestActions
    ; Result = ongoing(RemainingChildren) ->
        % Everyone yielded.
        % We yield and update the parallel_race state.
        Status = yielded,
        ActionsOut = [parallel_race(
            RemainingChildren)|RestActions]
    )}.

% ==========================================================
% Helpers
% ==========================================================

% tick_children_race(+Children, +ID, -Result, +CtxIn,
% -CtxOut)
% Result is one of:
%   - despawned
%   - race_won       <-- At least one finished
%   - ongoing(List)  <-- All children yielded
tick_children_race([], _ID, ongoing([])) --> [].

tick_children_race(
    [ChildAction|RestChildren],
    ID,
    Result
) -->
    % 1. Normalize child to list
    {( ChildAction = [_|_] ->
        ChildList = ChildAction
    ;
        ChildList = [ChildAction]
    )},
    % 2. Tick the child
    tick_object(
        actions_old(ChildList),
        obj_id(ID),
        result(ChildStatus, actions_new(RemainingActions))
    ),
    ( {ChildStatus = despawned} ->
        {Result = despawned}
    ; {ChildStatus = completed} ->
        % WINNER: Stop immediately (Skip fairness).
        % Subsequent children are NOT executed.
        {Result = race_won}
    ; % ChildStatus = yielded
        % Child yielded, check the next racer
        tick_children_race(
            RestChildren,
            ID,
            RestResult
        ),
        {( RestResult = despawned ->
            Result = despawned
        ; RestResult = race_won ->
            % Someone downstream won. Propagate the win.
            Result = race_won
        ; RestResult = ongoing(RestRemaining) ->
            % No one won. Flatten the list
            % (Fix nested list bug).
            append(
                RemainingActions,
                RestRemaining,
                AllRemaining
            ),
            Result = ongoing(AllRemaining)
        )}
    ).




% 7. Core Engine Components
% ==========================================================
% Execution Model: tick_action_streams
% ==========================================================

% Area of responsibility:
% - recurse over all streams and call tick_object for each.
% - resposible for removing despawned objects from acnnstore
% tick_action_streams threads the context via DCG.
% Takes an object ID and processes all action streams for
% that object.
% Returns Status: despawned, not_despawned
tick_action_streams(ObjID, Status) -->
    ctx_actionstore(AcnStoreIn),
    ( {gen_assoc(ObjID, AcnStoreIn, AcnStreamsOld)} ->
        % Process streams and get updated list
        tick_action_streams_loop(
            obj_id(ObjID),
            left(AcnStreamsOld),
            accum_old([]),
            accum_new(AcnStreamsNew),
            result(Status)
        ),
        ({Status = despawned} ->
            (
                % TODO: Implement a dbg_write/dbg_format
                %       togglable via env.
                % {format(
                %   "INFO: Removing obj ~w from acnstore~n",
                %   [ObjID]
                % )},
                % remove object from actionstore
                {del_assoc(
                    ObjID, AcnStoreIn, _, AcnStoreOut
                )},
                ctx_set_actionstore(AcnStoreOut)
            )
        ;
            % Update actionstore
            {put_assoc(
                ObjID,
                AcnStoreIn,
                AcnStreamsNew,
                AcnStoreOut
            )},
            ctx_set_actionstore(AcnStoreOut)
        )
    ;
        % the object is missing action streams for it!
        % throw error
        {format(
            "ERROR: Object ~w not found in actionstore~n",
            [ObjID]
        )},
        {halt(1)}
    ).

% base case: no streams left to process
% AccumRev is in reverse order, reverse it here to get it
% into non-reverse order.
tick_action_streams_loop(
    obj_id(_),
    left([]),
    accum_old(AccumRev),
    accum_new(Accum),
    result(not_despawned)
) -->
    { reverse(AccumRev, Accum) },
    !.

tick_action_streams_loop(
    obj_id(ObjId),
    % We store what we have left to process in Left.
    left([StreamToProcess|StreamToProcessRest]),
    % We build the new Action Streams List part by part and
    % store in Accum
    accum_old(AccumOld),
    accum_new(AccumNew),
    result(Result)
) -->
    tick_object(
        actions_old(StreamToProcess),
        obj_id(ObjId),
        result(TickStatus, actions_new(StreamsAfterTick))
    ),
    % So at this point we run one stream until a stop state
    %     and some might have added fork_cmd:s (added in
    %     reverse with cons).
    % Check for fork commands and append to left (once,
    % before branching)
    collect_and_append_forks(
        StreamToProcessRest,
        StreamToProcessRestWithForkAcns
    ),
    ({TickStatus = despawned} -> 
        ({AccumNew = []}, {Result = despawned})
    ;
        ({TickStatus = completed} ->
            % recurse
            tick_action_streams_loop(
                obj_id(ObjId),
                % head processed. process rest (with forks).
                left(StreamToProcessRestWithForkAcns),
                % completed, so don't need to add any
                % resulting actions because there were none.
                accum_old(AccumOld),
                accum_new(AccumNew),
                result(Result)
            )
        ; % yielded
            (
                % add what was left after yield.
                % AccumOld is in reverse order, prepend to
                % build backwards (O(1))
                {Accum = [StreamsAfterTick | AccumOld]},
                % recurse
                tick_action_streams_loop(
                    obj_id(ObjId),
                    % head processed. process rest (with
                    % forks).
                    left(StreamToProcessRestWithForkAcns),
                    accum_old(Accum),
                    accum_new(AccumNew),
                    result(Result)
                )
            )
        )
    ).

% ==========================================================
% Fork Command Processing
% ==========================================================

% Collect fork commands and append their actions to the left
% list. Removes processed commands from context.
collect_and_append_forks(
    StreamToProcessRest, % input
    StreamToProcessRestWithForkAcns % output
) -->
    ctx_forkCmds(ForkCmds),
    ( {ForkCmds = []} ->
        % No fork commands, nothing to append
        {StreamToProcessRestWithForkAcns
          = StreamToProcessRest}
    ;
        % Extract actions from all fork commands (all are
        % for this object)
        {extract_actions_from_fork_cmds(
            ForkCmds, NewStreams
        )},
        % Append new streams to left
        {append(
            StreamToProcessRest, % this was/is the input
            NewStreams, % new streams. from the fork_cmd:s.
            StreamToProcessRestWithForkAcns % output.
        )},
        % Clear fork commands (all processed)
        ctx_set_forkCmds([])
    ).

% Extract actions from fork commands
% Recurses.
% takes the fork_cmd:s (which are stored in revere) and
%     simply returns action streams.
% Simply "cons" them together.
%     So reversed twice i.e. put back in correct order.
% Wrapper
extract_actions_from_fork_cmds(ForkCmds, ActionStreams) :-
    extract_actions_acc(ForkCmds, [], ActionStreams).

% Base case: Assign Accumulator to Result
extract_actions_acc([], Acc, Acc).

% Recursive: Take Head, put it at front of Accumulator
% (Reversing logic)
extract_actions_acc(
    [fork_cmd(obj_id(_), actions(Actions)) | ForkCmdsRest], 
    Acc, 
    Result
) :-
    extract_actions_acc(
        ForkCmdsRest, [Actions|Acc], Result
    ).


% ==========================================================
% Execution Model: tick_object
% ==========================================================

% tick_object threads the context via DCG.
% Returns result(Status, actions_new(ActionsOut)) where
% Status is completed, yielded, or despawned.
% TODO: Change name because it kinda does not tick an object
%       any more but rather executes streams until a "stop"
%       state.
tick_object(
    actions_old([]),
    obj_id(_ID),
    result(completed, actions_new([]))
) --> [].

tick_object(
    actions_old([Act|Rest]),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    % Call execute_action with actions passed separately
    execute_action(
        actions_old([Act|Rest]),
        obj_id(ID),
        result(ActStatus, actions_new(ActionsTemp))
    ),
    ( {ActStatus = despawned} ->
        {Status = despawned, ActionsOut = []}
    ; {ActStatus = yielded} ->
        {Status = yielded, ActionsOut = ActionsTemp}
    ; % ActStatus = completed
        tick_object(
            actions_old(ActionsTemp),
            obj_id(ID),
            result(Status, actions_new(ActionsOut))
        )
    ).


% Collision Detection Module
% Handles collision detection between game objects
% Sets collision_id attributes on colliding objects

% ==========================================================
% Main entry: runs before tick_all_objects
% ==========================================================
detect_collisions -->
    % 1. Clear old collision_id attributes
    clear_all_collisions,
    % 2. Find all position overlaps
    ctx_objs_attrs(Objects, AttrStore),
    {find_collision_pairs(Objects, AttrStore, Pairs)},
    % Pairs = [(5, 12), (5, 18), (8, 12), ...]
    % 3. Write collision_id for each object
    write_collision_ids(Pairs).

% ----------------------------------------------------------
% Step 1: Clear collision_id attributes
% ----------------------------------------------------------
clear_all_collisions -->
    ctx_attrs(StoreIn),
    ctx_objs(Objects),
    {foldl(clear_one_collision, 
          Objects, 
          StoreIn, 
          StoreOut)},
    ctx_set_attrs(StoreOut).

clear_one_collision(Obj, StoreIn, StoreOut) :-
    obj_id(Obj, ID),
    ( gen_assoc(ID, StoreIn, Attrs) ->
        % Remove collision_id attribute if it exists
        ( select(attr(collision_id, _), Attrs, NewAttrs) ->
            put_assoc(ID, StoreIn, NewAttrs, StoreOut)
        ;
            StoreOut = StoreIn
        )
    ;
        StoreOut = StoreIn
    ).

% ----------------------------------------------------------
% Step 2: Find all collision pairs (optimized with
% spatial bucketing)
% ----------------------------------------------------------
% O(N) approach: Build position buckets, then extract pairs
% from buckets with multiple objects
find_collision_pairs(Objects, AttrStore, Pairs) :-
    % Build buckets: pos(X,Y) -> [ID1, ID2, ...]
    empty_assoc(EmptyBuckets),
    foldl(add_obj_to_bucket(AttrStore), Objects,
          EmptyBuckets, Buckets),
    % Extract collision pairs from buckets with 2+ objects
    collision_pairs_from_buckets(Buckets, Pairs).

% Add an object to the position bucket map
% Buckets: pos(X,Y) -> [ID1, ID2, ...] (reverse insertion
% order)
add_obj_to_bucket(AttrStore, Obj, BucketsIn, BucketsOut) :-
    obj_id(Obj, ID),
    ( gen_assoc(ID, AttrStore, Attrs),
      member(attr(x, X), Attrs),
      member(attr(y, Y), Attrs) ->
        Pos = pos(X, Y),
        ( get_assoc(Pos, BucketsIn, IDs) ->
            % Position exists: prepend ID to bucket
            put_assoc(Pos, BucketsIn, [ID|IDs], BucketsOut)
        ;
            % New position: create bucket with single ID
            put_assoc(Pos, BucketsIn, [ID], BucketsOut)
        )
    ;
        % Object has no position attributes, skip
        BucketsOut = BucketsIn
    ).

% Extract collision pairs from buckets
% Only buckets with 2+ objects have collisions
collision_pairs_from_buckets(Buckets, Pairs) :-
    findall(
        (ID1, ID2),
        (
            gen_assoc(_Pos, Buckets, IDs),
            % Bucket must have at least 2 objects
            IDs = [_,_|_],
            % Generate all pairs from this bucket
            select(ID1, IDs, Rest),
            member(ID2, Rest),
            % Ensure ID1 < ID2 to avoid duplicates
            ID1 @< ID2
        ),
        Pairs
    ).

% ----------------------------------------------------------
% Step 3: Write collision_id (last wins)
% ----------------------------------------------------------
write_collision_ids([]) --> [].
write_collision_ids([(ID1, ID2)|Rest]) -->
    % Write both directions
    ctx_set_attr_val(ID1/collision_id, ID2),
    ctx_set_attr_val(ID2/collision_id, ID1),
    % Continue (later writes override)
    write_collision_ids(Rest).

% Input Helper Predicates
% Utilities for checking key events in actions

% ==========================================================
% Key Event Queries (this frame only)
% ==========================================================

% key_down(+KeyCode)
% True if KeyCode had a 'down' event this frame
key_down(KeyCode) -->
    ctx_events(Events),
    {member(event(key(KeyCode), down), Events)}.

% key_up(+KeyCode)
% True if KeyCode had an 'up' event this frame
key_up(KeyCode) -->
    ctx_events(Events),
    {member(event(key(KeyCode), up), Events)}.

% ==========================================================
% Key State Queries (current state)
% ==========================================================

% key_held(+KeyCode)
% True if KeyCode is currently being held down
% (not just pressed this frame, but held since
%  some past frame)
key_held(KeyCode) -->
    ctx_held(Keys),
    {member(KeyCode, Keys)}.


% =========================================================
% Path Resolution Module
% =========================================================
% Resolves attribute paths strictly: tries to find attrs.
% If an attribute is missing, resolution fails (crash).
% Fallback behavior is only allowed via the explicit
% default/2 construct.
%
% New Path Structure (Input to resolve_path):
%   - Key (Atom): Resolves attribute Key on ObjID.
%   - ObjectSpec.Key: ObjectSpec resolves to NextID, then
%     Key is resolved on NextID.
%   - .Path: Wraps a path to indicate it is an attribute.
%
% NOTE: The . prefix is treated as syntactic sugar and is
% stripped automatically to resolve the underlying path.

% ==========================================================
% Main: resolve_path(+Ctx, +ObjID, +Path, -Value)
% ==========================================================
% Resolves a path (attribute reference or literal) to its
% value.

resolve_path(Ctx, ObjID, Path, Value) :-
    % Strip . prefix if present (syntactic sugar)
    ( Path = .(InnerPath) ->
        TruePath = InnerPath
    ;
        TruePath = Path
    ),

    (   % Case 1: Compound path using dot functor
        TruePath = Head.Rest
    ->
        resolve_path(Ctx, ObjID, Head, NextID),
        % Then resolve Rest starting from that object.
        % Rest must resolve to the final attribute Key/Value
        resolve_path(Ctx, NextID, Rest, Value)

    ;   % Case 2: Simple atom (Basecase: final attribute
        % Key)
        atom(TruePath)
    ->
        % MUST find an attribute. If ctx_attr_val fails,
        % the predicate fails (crash).
        ctx_attr_val(ObjID/TruePath, Value, Ctx, Ctx)

    ;  % Case 3: Numbers, variables, compounds (that aren't
       % dot):
       % pass through as-is (not attribute references)
        Value = TruePath
    ).


% ==========================================================
% Strict Path Resolution (for exists() checks)
% ==========================================================
% Unlike resolve_path, this fails if any attribute in the
% path doesn't exist. No fallback to literals.

% 1. Handle . prefix (strip it and recurse)
strict_resolve_path(Ctx, ObjID, .(Path), Value) :-
    !,
    strict_resolve_path(Ctx, ObjID, Path, Value).

% 2. Compound path using dot functor (recursive step)
strict_resolve_path(
    Ctx, ObjID, FirstAttr.RestPath, Value
) :-
    !,
    % First resolve Head to get the next object ID
    strict_resolve_path(Ctx, ObjID, FirstAttr, NextID),
    % Then resolve Rest starting from that object
    strict_resolve_path(Ctx, NextID, RestPath, Value).

% 3. Simple atom (base case for navigation, MUST exist)
strict_resolve_path(Ctx, ObjID, Path, Value) :-
    atom(Path),
    !,
    % Fails if attribute missing
    ctx_attr_val(ObjID/Path, Value, Ctx, Ctx).

% 4. Pass through (Numbers/Variables/Non-path compounds)
% This must be the last clause. It handles literals that
% were passed in (e.g., numbers, unbound variables, or
% complex terms that aren't paths).
strict_resolve_path(_Ctx, _ObjID, Path, Path).


% ==========================================================
% DCG Wrappers for Strict Path Resolution (Used by
% resolve_action)
% ==========================================================

% resolve_path_strict(+ObjID, +Path, -Value) -->
% Uses strict_resolve_path/4 internally
resolve_path_strict(ObjID, Path, Value) -->
    ctx_get(Ctx),
    {strict_resolve_path(Ctx, ObjID, Path, Value)}.

% resolve_path_to_attr(+ObjID, +Path, -Pair) -->
% Resolves the path down to the final object ID and
% attribute Key.
% Path can be a location path or wrapped in ..
resolve_path_to_attr(MyID, .(Path), Pair) -->
    !,
    resolve_path_to_attr(MyID, Path, Pair).

resolve_path_to_attr(MyID, AttrName, MyID/AttrName) -->
    {atom(AttrName)},  % Base case: simple attribute name
    !.

resolve_path_to_attr(MyID,
                     FirstAttr.RestPath,
                     FinalID/Key) -->
    !,
    % Navigate through FirstAttr to get NextID
    resolve_path_strict(MyID, FirstAttr, NextID),
    % Continue resolving RestPath
    resolve_path_to_attr(NextID, RestPath, FinalID/Key).
% =========================================================
% Operator Normalization Module
% =========================================================
% Converts familiar comparison operators to CLP(FD)
% operators for consistent evaluation.
%
% Goal: Allow users to write familiar syntax like < and >
% while maintaining relational/bidirectional properties
% of CLP(FD) internally.
%
% Examples:
%   hp < 0       → hp #< 0
%   parent_id/hp <= 100  → parent_id/hp #=< 100
%   type = enemy → type #= enemy

% ==========================================================
% normalize_comparison(+ComparisonIn, -ComparisonOut)
% ==========================================================
% Converts a comparison operator to its CLP(FD) equivalent.
% If already a CLP(FD) operator, passes through unchanged.

normalize_comparison(Comparison, Normalized) :-
    Comparison =.. [Op, Left, Right],
    (   op_map(Op, CLP_Op)
    ->
        % Operator needs conversion
        Normalized =.. [CLP_Op, Left, Right]
    ;
        % Already in good form (CLP(FD) or pass-through)
        Normalized = Comparison
    ).

% ==========================================================
% Operator Mapping Table
% ==========================================================
% Maps familiar operators to CLP(FD) equivalents.
%
% Standard Prolog → CLP(FD):
%   < (less than)         → #< (clpz less than)
%   > (greater than)      → #> (clpz greater than)
%   =< (less or equal)    → #=< (clpz less or equal)
%   >= (greater or equal) → #>= (clpz greater or equal)
%   = (unification)       → #= (clpz equal)
%   \= (not unifiable)    → #\= (clpz not equal)
%
% Already good (pass-through):
%   #<, #>, #=<, #>=, #=, #\= (CLP(FD) operators)

% Friendly operators → CLP(FD)
op_map(  <,  #< ).
op_map(  >,  #> ).
op_map( =<, #=< ).
op_map( >=, #>= ).
op_map(  =, #= ).
op_map( \=, #\= ).

% CLP(FD) operators are already normalized
op_map( #<,  #< ).
op_map( #>,  #> ).
op_map( #=<, #=< ).
op_map( #>=, #>= ).
op_map( #=,  #= ).
op_map( #\=, #\= ).

% =========================================================
% Main Entry: check_condition(+Ctx, +ObjID, +Condition)
% ==========================================================
% Evaluates a condition in the context of a game object ID.
%
% Returns: true if condition holds, false otherwise
%
% Condition Forms:
%   and(List)         - All conditions in list must succeed
%   or(List)          - Any condition in list must succeed
%   not(Condition)    - Condition must fail
%   Item in Path      - Item (value/path) in list attribute
%   Comparison        - Operators: <, >, =<, >=, =, \=, etc.

check_condition(Ctx, ObjID, Condition) :-
    % Original definition used Object but actions pass ObjID
    % We use ObjID directly here.
    check_condition_impl(Ctx, ObjID, Condition).

% Implementation dispatcher
check_condition_impl(Ctx, ObjID, and(Conditions)) :-
    !,
    % ALL conditions must succeed
    maplist(check_condition_impl(Ctx, ObjID), 
            Conditions).

check_condition_impl(Ctx, ObjID, or(Conditions)) :-
    !,
    % ANY condition must succeed
    member(Condition, Conditions),
    check_condition_impl(Ctx, ObjID, Condition).

check_condition_impl(Ctx, ObjID, not(Condition)) :-
    !,
    % Negation: condition must fail
    \+ check_condition_impl(Ctx, ObjID, Condition).

check_condition_impl(Ctx, ObjID, Item in AttributePath) :-
    !,
    % List membership: check if Item is in the list at
    % AttributePath
    check_membership(Ctx, ObjID, Item, AttributePath).

check_condition_impl(Ctx, ObjID, Comparison) :-
    % Handle comparison operators (<, >, =, etc.)
    is_comparison(Comparison),
    !,
    check_comparison(Ctx, ObjID, Comparison).

% NEW: Explicit existence check (strict - no fallback)
check_condition_impl(Ctx, ObjID, exists(PathSpec)) :-
    !,
    % Rule: PathSpec MUST be an attribute reference (start
    % with .)
    ( PathSpec = .(Path) ->
        PathToResolve = Path
    ;
        % If it doesn't start with ., it's an invalid path
        % specification for exists/1 (bare atom or wrong
        % structure)
        throw(error(invalid_path_spec(PathSpec), exists/1))
    ),
    % Use strict resolution: fails if attr doesn't exist
    strict_resolve_path(Ctx, ObjID, PathToResolve, _Value).

check_condition_impl(_Ctx, _ObjID, Condition) :-
    % Unknown condition type
    throw(error(
        unknown_condition(Condition),
        check_condition_impl/3
    )).

% ==========================================================
% Helper: is_comparison(+Term)
% ==========================================================
% Checks if a term is a comparison operator (has 2 args
% and uses a comparison functor).

is_comparison(Term) :-
    compound(Term),
    functor(Term, Op, 2),
    memberchk(Op, [
        % Standard Prolog operators
        <, >, =<, >=, =, \=,
        % CLP(FD) operators
        #<, #>, #=<, #>=, #=, #\=
    ]).

% ==========================================================
% Helper: check_comparison(+Ctx, +ObjID, +Comparison)
% ==========================================================
% Evaluates a comparison after path resolution and
% operator normalization.

check_comparison(Ctx, ObjID, Comparison) :-
    % Decompose comparison into operator and operands
    Comparison =.. [_Op, LeftSpec, RightSpec],
    
    % Resolve both sides (using strict path resolution,
    % allowing default/2 for explicit fallback)
    resolve_condition_value(
        Ctx, ObjID, LeftSpec, LeftValue
    ),
    resolve_condition_value(
        Ctx, ObjID, RightSpec, RightValue
    ),
    
    % Build the resolved comparison
    ResolvedComparison =.. [_Op, LeftValue, RightValue],
    
    % Normalize operators (< becomes #<, etc.)
    % NOTE: normalize_comparison/2 is expected to be
    % available
    normalize_comparison(ResolvedComparison, 
                         Normalized),
    
    % Evaluate the normalized comparison
    call(Normalized).

% ==========================================================
% Helper: resolve_condition_value(+Ctx, +ObjID,+Spec,-Value)
% ==========================================================
% Resolves a value specification (Path or default/2) for use
% in conditions.

% Helper to strip the prefix . operator if present
% If PathSpec is .(Path), we extract Path.
strip_prefix_at(.(Path), Path) :- !.
strip_prefix_at(Path, Path).

% Case 1: Explicit default/2 handling
resolve_condition_value(
    Ctx, ObjID, default(ValueExpr, Fallback), Value
) :-
    !,
    % Check if ValueExpr is bare atom (e.g., default(hp, 0))
    ( atom(ValueExpr), \+ compound(ValueExpr) ->
        throw(
            error(
                missing_at_prefix(ValueExpr),
                resolve_condition_value/4
            )
        )
    ;
        true
    ),
    
    strip_prefix_at(ValueExpr, PathSpec),
    
    % Try path resolution. If it fails, use fallback.
    ( resolve_path(Ctx, ObjID, PathSpec, Value) ->
        true
    ;
        % Path resolution failed (due to missing attribute)
        Value = Fallback
    ).

% Case 2: Standard path resolution
resolve_condition_value(Ctx, ObjID, PathSpec, Value) :-
    % --- FIX: Enforce . prefix for attribute lookups ---
    ( PathSpec = .(Path) ->
        % Explicit attribute reference: resolve path
        resolve_path(Ctx, ObjID, Path, Value)
    ; atom(PathSpec) ->
        % Bare atom used where attribute reference is
        % expected (e.g., hp)
        throw(
            error(
                missing_at_prefix(PathSpec),
                resolve_condition_value/4
            )
        )
    ;
        % Literal value (number, variable, or compound
        % literal like 1+2)
        % resolve_path will return PathSpec as Value (via
        % its literal passthrough clause)
        resolve_path(Ctx, ObjID, PathSpec, Value)
    ).


% ==========================================================
% Strict Path Resolution (for exists() checks)
% ==========================================================
% NOTE: Definition moved to
% prolog/conditions/path_resolution.pl
% to avoid discontiguous warnings.

% ==========================================================
% Helper: check_membership(+Ctx, +ObjID, +Item, 
%                          +AttributePath)
% ==========================================================
% Checks if Item is a member of the list at AttributePath.
%
% Item can be:
%   - A literal value (sword, 42, etc.)
%   - An attribute path (parent_id.weapon, etc.)
%
% AttributePath must resolve to a list.
%
% Examples:
%   sword in inventory
%   → Item=sword, Path=inventory
%   → resolve inventory to list
%   → check member(sword, list)
%
%   parent_id.weapon in allowed_weapons
%   → Item=parent_id.weapon, Path=allowed_weapons
%   → resolve Item to weapon on parent
%   → resolve Path to list
%   → check membership

check_membership(Ctx, ObjID, Item, AttributePath) :-
    % Resolve the item (could be path or literal)
    resolve_condition_value(Ctx, ObjID, Item, ItemValue),
    
    % Resolve the attribute path to get the list
    resolve_condition_value(
        Ctx, ObjID, AttributePath, ListValue
    ),
    
    % ListValue must be a list
    (   is_list(ListValue)
    ->
        % Check membership
        member(ItemValue, ListValue)
    ;
        % AttributePath didn't resolve to a list
        throw(error(
            type_error(list, ListValue),
            check_membership/4
        ))
    ).

% ==========================================================
% Notes on Design
% ==========================================================
%
% 1. Path Resolution Strategy
%    - STRICT: Missing attributes cause failure/crash.
%    - Fallback only via explicit default/2.
%
% 2. Operator Normalization
%    - Converts < to #< automatically
%    - Happens after path resolution
%    - Maintains bidirectionality via CLP(FD)
%
% 3. Composition (and/or/not)
%    - Standard logical composition
%    - Arbitrary nesting allowed
%    - Uses maplist/member for efficiency
%
% 4. List Membership (in operator)
%    - Works with literal items or paths
%    - Works with literal lists or attribute paths
%    - Provides referential transparency
%
% Example Complex Condition:
%   and([
%       or([
%           default(.hp, 0) < 0,
%           status = dead
%       ]),
%       not(exists(.invulnerable)),
%       sword in inventory,
%       parent_id.team = ally
%   ])
%
% All of these compose naturally without special syntax.


% 8. Main Engine
% ==========================================================
% Main Tick Function
% ==========================================================
tick -->

    % 1. Detect collisions BEFORE ticking
    % (so objects can react)
    detect_collisions,

    % 2. Tick Physics & Logic
    % Iterates by ID, allowing new spawns to be picked up
    % immediately.
    tick_all_objects,

    % 3. Increment Frame
    increment_frame.

% ==========================================================
% Pipeline Stages
% ==========================================================

increment_frame -->
    ctx_frame(F),
    {F1 #= F + 1},
    ctx_set_frame(F1).

% ==========================================================
% Tick Logic (The ID Cursor)
% ==========================================================

% The responsibilities for tick_all_objects is to iterate
% over all objects and execute tick_action_streams for each.
% It is also responsible for cleaning up objects in the
% objects list and cleaning up actions in actionstore.
% It is also responsible for reading spawn commands and
% spawning new objects.
tick_all_objects -->
    ctx_objs(ObjsQueue),
    % At first we have all objects left to process.
    tick_objects_loop(ObjsQueue).

% base case: no objects left to process
tick_objects_loop([]) --> !.

% The loop finds the next object with ID > LastID
tick_objects_loop([TargetObj|ObjsQueue]) -->
    % Found an object to tick
    {obj_id(TargetObj, TargetID)},
    % Process actions from actionstore
    tick_action_streams(TargetID, Status),
    % Update the context with the result of this
    % specific object based on status
    ( {Status = despawned} ->
        % Remove object from context and actionstore
        % (cleaned by tick_action_streams)
        update_object_in_context(TargetID, [])
    ;
        % Keep object (yielded or completed)
        % No need to reconstruct - actions are in
        % actionstore
        []
    ),
    % So at this point we have processed all action streams
    %     of one specific object... and in the process
    %     the object might have spawned new objects...
    %     So we need to add them!
    % Process spawn commands ASAP so spawned objects
    % can execute in the same frame
    % So assuming `process_spawn_commands` works in a
    %     reasonable way it should simply add the new
    %     objects to the end.
    process_spawn_commands(ObjsQueue, ObjsQueueNew),
    % Recurse using the current TargetID as the new
    % floor
    tick_objects_loop(ObjsQueueNew).

% Helper: Replaces the object with TargetID with the
% NewList (which is [Obj] or [])
update_object_in_context(TargetID, NewList) -->
    ctx_objs(Objects),
    {replace_by_id(Objects, TargetID, NewList, NewObjects)},
    ctx_set_objs(NewObjects).

% replace_by_id(+CurrentList, +TargetID, +ReplacementList,
% -NewList)
replace_by_id(
    [], % Objects
    _, % TargetID
    _, % Replacement
    [] % NewObjects
).
replace_by_id(
    [Obj|Rest],
    TargetID,
    Replacement,
    Result
) :-
    obj_id(Obj, ID),
    ( ID = TargetID ->
        % MATCH FOUND: Determine operation based on
        % Replacement list
        ( Replacement = [] ->
            % CASE 1: Deletion
            % The result is simply the rest of the list.
            Result = Rest
        ;
            % CASE 2: Update (Optimization: Assume 1 item)
            % The result is the new object attached to the
            % rest.
            Replacement = [NewObj],
            Result = [NewObj|Rest]
        )
    ;
        % NO MATCH: Keep searching
        Result = [Obj|NewRest],
        replace_by_id(Rest, TargetID, Replacement, NewRest)
    ).

% ==========================================================
% Spawn Command Processing
% ==========================================================

% `process_spawn_commands` takes the ObjsQueue as input
%     which is the queue (list) of objects left to process
%     and it look through the spawn_cmd:s and add new
%     spawned object to the end of the end the the queue so
%     (TODO: Well actually it adds them to the front!)
%     that the game loop will process them (this frame).
% `process_spawn_commands` also adds the objects to the ctx.
% `process_spawn_commands` also adds to the actionstore.
process_spawn_commands(ObjsQueue, ObjsQueueNew) -->
    % At this point SpawnCmds are in reverse order!
    ctx_spawnCmds(SpawnCmds), 
    % Process all spawn commands
    process_spawn_commands_loop(
        spawn_cmds(SpawnCmds),
        ObjsQueue,
        ObjsQueueNew,
        [],           % SpawnedObjs starts empty
        SpawnedObjs   % collected spawns in correct order
    ),
    % add the spawned objects to the context
    ctx_objs(ObjsOld),
    {append(ObjsOld, SpawnedObjs, ObjsNew)},
    ctx_set_objs(ObjsNew),
    % Clear spawn commands after processing
    ctx_set_spawnCmds([]).

% Process each spawn command
% base case: no spawn commands left to process
process_spawn_commands_loop(
    spawn_cmds([]),
    ObjsQueue,
    ObjsQueue,
    SpawnedObjs,
    SpawnedObjs
) --> !.
process_spawn_commands_loop(
    spawn_cmds([
        spawn_cmd(actions(SpawnObjActions))|CmdsRest
    ]),
    ObjsQueue,
    ObjsQueueNew,
    SpawnedObjsOld,
    SpawnedObjsNew
) -->
    % Generate new ID
    ctx_nextid(NewID),
    {NextID #= NewID + 1},
    ctx_set_nextid(NextID),
    % Create object
    {NewObj = object(id(NewID))},

    % Add object to the left to process queue
    % so yea we both add it to the the left to process queue
    % and to the context.
    % TODO: prepend because it's faster but this causes
    % newly spawned objects to be processed first, which
    % might not be ideal since the user might rely on stuff
    % happening in a strict order.
    {ObjsQueueAfterPrepend = [NewObj | ObjsQueue]},

    % "collect" the object. we will add it to context but
    % we don't do it one per one becuase that's slow, we
    % collect them and then add them in bulk.
    % Since the spawn_cmd:s were added in reverse originally
    %     prepending here causes a double reverse i.e. order
    %     of objects in `SpawnedObjsAfterPrepend` we be
    %     in correct/normal order.
    {SpawnedObjsAfterPrepend = [NewObj | SpawnedObjsOld]},

    % Add actions to actionstore (wrap in list for single
    % stream)
    % tick_objects_loop will automatically pick up and
    % execute these actions
    ctx_actionstore(ActionStore),
    {put_assoc(
        NewID,
        ActionStore,
        [SpawnObjActions],
        NewActionStore
    )},
    ctx_set_actionstore(NewActionStore),
    % Process remaining commands
    process_spawn_commands_loop(
        spawn_cmds(CmdsRest),
        ObjsQueueAfterPrepend,
        ObjsQueueNew,
        SpawnedObjsAfterPrepend,
        SpawnedObjsNew
    ).



% 9. Game Entry Point
% ==========================================================
% Main Game Loop
% ==========================================================

main :-
    catch(
        (
            % ----------------------------------------------
            % Resolve GAME directory
            % ----------------------------------------------
            ( catch(getenv("GAME", RawGame), _, fail) ->
                atom_chars(GameName, RawGame) 
            ;
                throw('GAME environment variable missing')
            ),

            atom_concat('games/', GameName, GameDir),
            atom_concat(GameDir, '/game.pl', GameFileAtom),
            atom_concat(GameDir, '/input.pl',InputFileAtom),

            atom_chars(GameFileAtom, GameFile),

            (   catch(consult(InputFileAtom), _, fail),
                catch(
                    input_timeline(TimelineList),
                    _,
                    fail
                )
            ->
                list_to_assoc(TimelineList, Timeline)
            ;
                throw('input_timeline not found in file')
            ),

            % ----------------------------------------------
            % Create root object & context
            % ----------------------------------------------
            empty_assoc(AttrStore0),
            put_assoc(
                0,
                AttrStore0,
                [ attr(type, static),
                  attr(x, 0),
                  attr(y, 0)
                ],
                AttrStore1
            ),

            ctx_with_objs_input(
                [object(id(0))],
                [],
                [],
                InitialContext0
            ),
            ctx_set_attrs(
                AttrStore1, InitialContext0, InitialContext1
            ),

            % ----------------------------------------------
            % Actionstore: load game via ENGINE (CRITICAL)
            % ----------------------------------------------
            ctx_actionstore(
                ActionStore0,
                InitialContext1,
                InitialContext1
            ),
            put_assoc(
                0,
                ActionStore0,
                [[load(GameFile)]],
                ActionStore1
            ),
        
            ctx_set_actionstore(
                ActionStore1,
                InitialContext1,
                InitialContext
            ),

            % ----------------------------------------------
            % Start game loop
            % ----------------------------------------------
            game_loop(
                ctx_in(InitialContext),
                [],
                input_timeline(Timeline),
                keys_held([])
            )
        ),
        Error,
        (
            write('Fatal error in main: '),
            write(Error), nl,
            halt(1)
        )
    ).


% ==========================================================
% Game Loop
% ==========================================================

game_loop(
    ctx_in(Ctx),
    History,
    input_timeline(Timeline),
    keys_held(KeysHeld)
) :-
    catch(
        (
            ctx_status(Status, Ctx, Ctx),
            ( Status = playing ->
                render(Ctx, Ctx),
                write(
                    'Press: f=forward, r=reverse, q=quit'
                ), nl,
                flush_output,
                % Scryer/Others return an Atom directly.
                get_single_char(Char),
                ( Char = end_of_file ->
                    ctx_set_status(lost, Ctx, NewCtx),
                    NewHistory = History,
                    NewKeysHeld = KeysHeld
                ;
                    handle_input(
                        Char,
                        ctx_in(Ctx),
                        History,
                        input_timeline(Timeline),
                        keys_held(KeysHeld),
                        ctx_out(NewCtx),
                        NewHistory,
                        keys_held(NewKeysHeld)
                    )
                ),

                game_loop(
                    ctx_in(NewCtx),
                    NewHistory,
                    input_timeline(Timeline),
                    keys_held(NewKeysHeld)
                )
            ;
                render(Ctx, Ctx),
                write('Game Over!'), nl
            )
        ),
        Error,
        ( write('Error in game_loop: '),
          write(Error), nl,
          halt(1)
        )
    ).

handle_input(
    Char, 
    ctx_in(Ctx), 
    History,
    input_timeline(Timeline),
    keys_held(KeysHeld),
    ctx_out(NewCtx), 
    NewHistory,
    keys_held(NewKeysHeld)
) :-
    (   char_code(Char, 102) ->  % 'f'
        % Forward: lookup events, update keys, tick
        ctx_frame(Frame, Ctx, Ctx),
        Frame1 #= Frame + 1,
        
        % Get events for next frame
        ( get_assoc(Frame1, Timeline, Events) ->
            true
        ;
            Events = []
        ),
        
        % Update keys_held based on events
        apply_events(Events, KeysHeld, NewKeysHeld),
        
        % Inject input(events, held) into context
        ctx_set_input(
          input(events(Events), held(NewKeysHeld)),
          Ctx,
          CtxWithInput
        ),
        
        % Tick
        catch(
            tick(CtxWithInput, NewCtx),
            Error,
            ( write('Error during tick: '),
              write(Error), nl,
              throw(Error)
            )
        ),
        NewHistory = [ctx_in(Ctx)|History]
    ;   char_code(Char, 114) ->  % 'r'
        % Reverse: go back
        ( History = [ctx_in(PrevCtx)|RestHistory] ->
            NewCtx = PrevCtx,
            NewHistory = RestHistory,
            % Reconstruct keys_held from PrevCtx
            ctx_input(
                input(_, held(NewKeysHeld)),
                PrevCtx, PrevCtx
            )
        ;
            NewCtx = Ctx,
            NewHistory = History,
            NewKeysHeld = KeysHeld
        )
    ;   char_code(Char, 113) ->  % 'q'
        % Quit
        ctx_set_status(lost, Ctx, NewCtx),
        NewHistory = History,
        NewKeysHeld = KeysHeld
    ;
        % Unknown input
        NewCtx = Ctx,
        NewHistory = History,
        NewKeysHeld = KeysHeld
    ).

% apply_events(+Events, +KeysIn, -KeysOut)
% Update key state based on frame events
apply_events([], Keys, Keys).
apply_events(
    [event(key(K), down)|Rest], 
    KeysIn, 
    KeysOut
) :-
    % Add K if not present
    ( member(K, KeysIn) ->
        KeysTemp = KeysIn
    ;
        KeysTemp = [K|KeysIn]
    ),
    apply_events(Rest, KeysTemp, KeysOut).
apply_events(
    [event(key(K), up)|Rest], 
    KeysIn, 
    KeysOut
) :-
    % Remove K if present
    ( select(K, KeysIn, KeysTemp) ->
        true
    ;
        KeysTemp = KeysIn
    ),
    apply_events(Rest, KeysTemp, KeysOut).

% ==========================================================
% ASCII Rendering
% ==========================================================
render(Ctx, Ctx) :-
    % Clear screen (ANSI escape code)
    % COMMENTED OUT for debugging
    % char_code(Esc, 27),  % ESC character
    % write(Esc), write('[2J'), write(Esc), write('[H'),
    
    % ctx_actionstore(ActionStore, Ctx, _),
    % write('--- Action Store ---'), nl,
    % pretty_print(ActionStore),
    % nl,
    % write('--------------------'), nl,

    ctx_frame(Frame, Ctx, Ctx),
    ctx_status(Status, Ctx, Ctx),
    ctx_objs(Objects, Ctx, Ctx),
    length(Objects, ObjCount),
    
    % Header
    write('=== Tower Defense ==='), nl,
    write('Frame: '), write(Frame),
    write(' | Status: '), write(Status),
    write(' | Objects: '), write(ObjCount), nl,
    write('================================'), nl, nl,
    
    % Render grid (20x20)
    render_grid(Objects, Ctx, Ctx),
    nl.

render_grid(Objects, CtxIn, CtxOut) :-
    % Build position map once: pos(X, Y) -> Symbol
    build_position_map(Objects, PosMap, CtxIn, CtxOut),
    % Grid is 20x20, positions 0-19
    render_grid_rows(PosMap, 0).

build_position_map(Objects, PosMap, CtxIn, CtxOut) :-
    empty_assoc(EmptyMap),
    build_position_map_loop(
        Objects, EmptyMap, PosMap, CtxIn, CtxOut
    ).

build_position_map_loop([], Map, Map, CtxIn, CtxOut) :-
    CtxOut = CtxIn.
build_position_map_loop(
    [Obj|Rest], MapIn, MapOut, CtxIn, CtxOut
) :-
    obj_id(Obj, ID),
    ( ctx_attr_val(ID/x, X, CtxIn, CtxIn),
      ctx_attr_val(ID/y, Y, CtxIn, CtxIn),
      get_symbol(ID, Symbol, CtxIn, CtxMid) ->
        Pos = pos(X, Y),
        put_assoc(Pos, MapIn, Symbol, MapTemp)
    ;
        MapTemp = MapIn,
        CtxMid = CtxIn
    ),
    build_position_map_loop(
        Rest, MapTemp, MapOut, CtxMid, CtxOut
    ).

render_grid_rows(PosMap, Y) :-
    Y =< 19,
    render_grid_row(PosMap, Y, 0),
    Y1 is Y + 1,
    render_grid_rows(PosMap, Y1).
render_grid_rows(_, Y) :-
    Y > 19.

render_grid_row(PosMap, Y, X) :-
    X =< 19,
    Pos = pos(X, Y),
    ( get_assoc(Pos, PosMap, CharCode) ->
        put_code(CharCode)
    ;
        put_code(46)  % '.'
    ),
    ( X = 19 -> nl ; put_code(32) ),  % space
    X1 is X + 1,
    render_grid_row(PosMap, Y, X1).
render_grid_row(_, _, X) :-
    X > 19.

get_symbol(ID, Symbol, CtxIn, CtxOut) :-
    % Get displayChar attribute (character code as integer)
    % If attribute doesn't exist, predicate fails
    % (object not displayed)
    ctx_attr_val(ID/displayChar, Symbol, CtxIn, CtxIn),
    integer(Symbol),
    CtxOut = CtxIn.


