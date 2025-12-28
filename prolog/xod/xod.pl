


validate(Term, SchemaName) :-
    call(SchemaName, Schema),
    validates_against(Term, Schema, []).


validates_against(_, any, _) :- !.

validates_against(V, integer, Path) :-
    check(V, integer(V), expected_integer,
          Path).

validates_against(V, integer(Min, Max),
                  Path) :-
    (var(V) ->
        true
    ;
        (integer(V) ->
            check_int_range(V, Min, Max, Path)
        ;
            throw_error(Path,
                  expected_integer, V)
        )
    ).

validates_against(V, atom, Path) :-
    check(V, atom(V), expected_atom, Path).

validates_against(V, enum(Opts), Path) :-
    (ground(V) ->
        check(V, memberchk(V, Opts),
              invalid_enum_option, Path)
    ;
        true
    ).

validates_against(V, list(Schema),
                  Path) :-
    check(V, is_list(V), expected_list,
          Path),
    check_list_items(V, Schema, Path, 0).

validates_against(V, struct(Functor,
                           Fields), Path) :-
    (var(V) ->
        true
    ;
        check_struct(V, Functor, Fields,
                    Path)
    ).

validates_against(V, schema(SchemaName),
                  Path) :-
    (var(V) ->
        true
    ;
        (atom(SchemaName) ->
            (call(SchemaName, Definition) ->
                validates_against(V,
                    Definition, Path)
            ;
                throw_error(Path,
                      unknown_schema,
                      SchemaName)
            )
        ;
            throw_error(Path,
                  invalid_schema_ref,
                  SchemaName)
        )
    ).

validates_against(V, union(Schemas),
                  Path) :-
    (var(V) ->
        true
    ;
        try_union_schemas(V, Schemas, Path)
    ).

try_union_schemas(V, [], Path) :-
    throw_error(Path, no_matching_union, V).
try_union_schemas(V, [Schema|Rest], Path) :-
    ( catch(validates_against(V, Schema, Path),
            validation_error(_, _, _),
            fail) ->
        true  % Schema matched, succeed
    ;
        try_union_schemas(V, Rest, Path)
    ).



check(V, Goal, ErrorReason, Path) :-
    (var(V) ->
        true
    ;
        (call(Goal) ->
            true
        ;
            throw_error(Path, ErrorReason, V)
        )
    ).

is_list([]).
is_list([_|T]) :- is_list(T).


expect_exception(Goal) :-
    catch((Goal, fail), _, true).


format_(Format, Args) :-
    ( catch(getenv("VALIDATION_ERR_MSG", Value), _, fail),
      Value = "false" ->
        true  % Suppress when set to "false"
    ;
        format(Format, Args)  % Output normally
    ).



throw_error(RevPath, Reason, Value) :-
    reverse(RevPath, Path),
    format_error_message(Path, Reason, Value),
    throw(validation_error(Path, Reason,
                          Value)).

format_error_message(Path, Reason, Value) :-
    format_("ERROR: Validation failed~n", []),
    format_("  Path: ~w~n", [Path]),
    ( Reason = struct_arity_mismatch(Expected) ->
        format_("  Reason: struct_arity_mismatch~n", []),
        format_("  Expected arity: ~w~n", [Expected]),
        format_("  Actual arity: ~w~n", [Value])
    ;
        format_("  Reason: ~w~n", [Reason]),
        format_("  Value: ~w~n", [Value])
    ),
    flush_output.


check_int_range(V, Min, Max, Path) :-
    (nonvar(Min), V < Min ->
        throw_error(Path, min_value(Min), V)
    ;
        true
    ),
    (nonvar(Max), V > Max ->
        throw_error(Path, max_value(Max), V)
    ;
        true
    ).


check_struct(V, ExpectedFunctor, Fields,
            Path) :-
    (compound(V) ->
        functor(V, ActualFunctor, Arity),
        (ActualFunctor == ExpectedFunctor ->
            check_struct_arity(V, Arity,
                Fields, Path)
        ;
            throw_error(Path,
                  wrong_functor,
                  ExpectedFunctor)
        )
    ;
        throw_error(Path, expected_struct, V)
    ).

check_struct_arity(V, Arity, Fields,
                   Path) :-
    length(Fields, FLen),
    (Arity == FLen ->
        check_struct_args(V, 1, Fields,
                         Path)
    ;
        throw_error(Path,
              struct_arity_mismatch(FLen),
              Arity)
    ).

check_struct_args(_, _, [], _).
check_struct_args(V, Idx, [F|Fs],
                 Path) :-
    arg(Idx, V, Val),
    F =.. [FieldName, Schema],
    validates_against(Val, Schema,
        [FieldName|Path]),
    NextIdx is Idx + 1,
    check_struct_args(V, NextIdx, Fs,
                     Path).


check_list_items([], _, _, _).
check_list_items([Item|Rest], Schema,
                 Path, Index) :-
    validates_against(Item, Schema,
        [[Index]|Path]),
    NextIndex is Index + 1,
    check_list_items(Rest, Schema, Path,
                    NextIndex).


game_state_schema(struct(game_state, [
    frame(integer(0, _)),
    objects(list(schema(
        game_object_schema))),
    status(enum([playing, won, lost])),
    score(integer(0, _)),
    next_id(integer(1, _)),
    commands(list(any)),
    rev_hints(list(schema(
        test_rev_hint_schema)))
])).

game_object_schema(struct(game_object, [
    id(integer),
    type(enum([static, enemy, proj,
               player, tower])),
    attrs(list(any)),
    actions(list(any)),
    collisions(list(any))
])).

test_rev_hint_schema(struct(rev_hint, [
    id(integer),
    attrs(list(any))
])).


test("valid game_state", (
    State = game_state(
        0,
        [game_object(0, static,
                    [], [], [])],
        playing,
        0,
        1,
        [],
        []
    ),
    validate(State, game_state_schema)
)).

test("game_state with unbound fields", (
    State = game_state(0, _, playing, 0,
                      1, _, _),
    validate(State, game_state_schema)
)).

test("complex game_state", (
    State = game_state(
        5,
        [
            game_object(0, tower,
                       [pos(10, 19)],
                       [wait(3)], []),
            game_object(1, enemy,
                       [pos(5, 5)],
                       [], []),
            game_object(2, proj,
                       [pos(15, 15)],
                       [], [])
        ],
        playing,
        100,
        3,
        [],
        [rev_hint(1,
                 [pos(10, 10)])]
    ),
    validate(State, game_state_schema)
)).

test("invalid status throws", (
    State = game_state(0, [], 
                      invalid_status,
                      0, 1, [], []),
    catch(
        (validate(State,
                 game_state_schema), fail),
        validation_error(Path, _, _),
        (Path = [status|_])  % Verify path
    )
)).

test("non-integer frame throws", (
    State = game_state(not_int, [],
                      playing, 0, 1,
                      [], []),
    expect_exception(validate(State,
                             game_state_schema))
)).

test("wrong functor throws", (
    State = enemy_state(0, [],
                       playing, 0, 1,
                       [], []),
    expect_exception(validate(State,
                             game_state_schema))
)).

test("nested path in error", (
    State = game_state(
        0,
        [game_object(0, static, [],
                    [], []),
         game_object(not_int, static,
                    [], [], [])],
        playing,
        0,
        2,
        [],
        []
    ),
    catch(
        (validate(State,
                 game_state_schema), fail),
        validation_error(Path, _, _),
        (Path = [objects, [1],
                id|_])
    )
)).

test("valid game_object", (
    Obj = game_object(0, static,
                     [], [], []),
    validate(Obj, game_object_schema)
)).

test("invalid object type throws", (
    Obj = game_object(0, invalid_type,
                     [], [], []),
    expect_exception(validate(Obj,
                             game_object_schema))
)).

test("integer range in bounds", (
    validates_against(50,
                     integer(0, 100),
                     [])
)).

test("integer range zero lower bound", (
    validates_against(0,
                     integer(0, 100),
                     [])
)).

test("integer range unbounded upper", (
    validates_against(1000000,
                     integer(0, _),
                     [])
)).

test("integer range unbound passes", (
    validates_against(_,
                     integer(0, 100),
                     [])
)).

test("integer range too high throws", (
    expect_exception(validates_against(150,
                                     integer(0, 100),
                                     []))
)).

test("empty list passes", (
    validates_against([], list(integer),
                     [])
)).

test("valid integer list", (
    validates_against([1, 2, 3, 4],
                     list(integer),
                     [])
)).

test("list with invalid item throws", (
    expect_exception(validates_against([1, not_int, 3],
                                      list(integer),
                                      []))
)).

test("enum valid option", (
    validates_against(playing,
                     enum([playing, won,
                          lost]),
                     [])
)).

test("enum invalid option throws", (
    expect_exception(validates_against(invalid,
                                      enum([a, b, c]),
                                      []))
)).

test("any always passes", (
    validates_against(anything, any, [])
)).

test("any with unbound passes", (
    validates_against(_, any, [])
)).

test("union first schema matches", (
    validates_against(50,
        union([integer(0, 100), atom]),
        [])
)).

test("union second schema matches", (
    validates_against(my_atom,
        union([integer, atom]),
        [])
)).

test("union with unbound passes", (
    validates_against(_,
        union([integer, atom]),
        [])
)).

test("union no schema matches throws", (
    expect_exception(validates_against(3.14,
        union([integer, atom]),
        []))
)).
