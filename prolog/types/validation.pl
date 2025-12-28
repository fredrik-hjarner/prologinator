
val_format(Format, Args) :-
    ( catch(getenv("VALIDATION_ERR_MSG", Value), _, fail),
      Value = "false" ->
        true  % Suppress when set to "false"
    ;
        format(Format, Args)  % Output normally
    ).



context_validation(Ctx, Ctx) :-
    Ctx = ctx(State, Input),
    state_validation(State),
    input_validation(Input).


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


event_validation(event(key(KeyCode), State)) :-
    integer(KeyCode),
    ( State = down ; State = up ).


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
            integer(Frame),
            length(Objects, _),
            ground(Attrs),
            game_status_validation(Status),
            integer(NextID),
            length(SpawnCmds, _),
            length(ForkCmds, _),
            ground(ActionStore),
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


object_validation_helper(Term) :-
    ( ground(Term) ->
        ( Term = object(id(ID)) ->
            integer(ID)
        ;
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


spawn_request_validation(Term) :-
    ( ground(Term) ->
        ( Term = spawn_request(Type, _X, _Y, Acts) ->
            atom(Type),
            length(Acts, _)
        ;
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


state_change_validation(Term) :-
    ( ground(Term) ->
        ( Term = state_change(Change) ->
            ( ground(Change) ->
                state_change_validation_helper(Change)
            ;
                true
            )
        ;
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

is_valid_value_spec(Term) :-
    (   number(Term)
    ;   Term = .(_)
    ;   Term = -(_)
    ;   Term = default(_, _)
    ).

is_valid_path(Term) :-
    ( atom(Term)
    ; (compound(Term), functor(Term, '.', 1))
    ; (compound(Term), functor(Term, '.', 2))
    ).

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
            integer(X),
            integer(Y)
        ;
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


builtin_functor_name(Name) :-
    builtin_action(Template),
    functor(Template, Name, _).

is_builtin_functor(Term) :-
    callable(Term),
    functor(Term, Name, _Arity),
    builtin_functor_name(Name).


action_validation_helper(Term) :-
    ( ground(Term) ->
        ( Term = wait(N) ->
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
            true
        ; Term = spawn(Acts) ->
            length(Acts, _)
        ; Term = set_attr(Path, _Value) ->
            is_valid_path(Path)
        ; Term = incr(Path, Amount) ->
            is_valid_path(Path),
            is_valid_value_spec(Amount)
        ; Term = decr(Path, Amount) ->
            is_valid_path(Path),
            is_valid_value_spec(Amount)
        ; Term = copy_attr(SourcePath, DestPath) ->
            is_valid_path(SourcePath),
            is_valid_path(DestPath)
        ; Term = loop(Acts) ->
            ( ground(Acts) ->
                length(Acts, _)
            ;
                true
            )
        ; Term = loop(Running, Original) ->
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
            ( ground(Acts) ->
                length(Acts, _)
            ;
                true
            )
        ; Term = trigger_state_change(Change) ->
            ( ground(Change) ->
                state_change_validation_helper(Change)
            ;
                true
            )
        ; Term = parallel_all(Children) ->
            ( ground(Children) ->
                length(Children, _)
            ;
                true
            )
        ; Term = parallel_race(Children) ->
            ( ground(Children) ->
                length(Children, _)
            ;
                true
            )
        ; Term = repeat(Times, Acts) ->
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
        ; Term = repeat(Times, Running, Original) ->
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
            ( ground(Signature), ground(Body) ->
                ( is_list(Body) ->
                    true
                ; callable(Body) ->
                    true
                ;
                    true  % Allow other structures for
                )
            ;
                true
            )
        ; Term = load(Path) ->
            is_list(Path)
        ; Term = log(Msg) ->
            is_list(Msg)
        ; Term = fork(Acts) ->
            ( ground(Acts) ->
                length(Acts, _)
            ;
                true
            )
        ; Term = wait_until(_Cond) ->
             true
        ; Term = attr_if(_Cond, Then, Else) ->
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
        ; Term = wait_key_down(KeyCode) ->
            integer(KeyCode)
        ; Term = wait_key_up(KeyCode) ->
            integer(KeyCode)
        ; Term = wait_key_held(KeyCode) ->
            integer(KeyCode)
        
        
        ; ( callable(Term), \+ is_builtin_functor(Term) ) ->
            true
        
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
