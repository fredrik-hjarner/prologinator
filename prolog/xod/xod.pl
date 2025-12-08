% ================================================
% VALIDATION SYSTEM - Phase 1
% Target: Scryer Prolog (ISO Prolog)
% ================================================
%
% CRITICAL FIXES APPLIED:
% 1. Module context threaded through recursion
%    (enables cross-module schema lookup)
% 2. Integer range crash fixed
%    (var check before range check)
% 3. Optimization: arg/3 instead of =..
%    (zero-allocation struct checking)
%
% RATIONALE & DESIGN
% ==================
%
% This is a schema-driven validator that interprets
% data structures to validate game state and
% objects. It replaces hand-written validation
% predicates with declarative schemas.
%
% Key Design Decisions:
%
% 1. SCHEMA-AS-DATA
%    Schemas are Prolog terms (facts), not code.
%    Example: game_state_schema(struct(...)).
%    This keeps data and logic separate.
%
% 2. EXPLICIT FUNCTOR CHECKING
%    Each struct includes its expected functor.
%    Example: struct(game_state, [fields...]).
%    This prevents type confusion. game_state(1,2)
%    and enemy_state(1,2) are now distinct.
%
% 3. EXPLICIT SCHEMA REFERENCES
%    Named schema lookups wrapped: schema(name).
%    Example: objects(list(schema(
%             game_object_schema))).
%    This removes ambiguity. Parser knows: "go
%    look up game_object_schema."
%
% 4. IMPLICIT OPTIONALITY (var(V) -> pass)
%    Every field rule: If unbound, pass. If
%    bound, validate. No explicit optional() needed.
%    Example: integer(0, 100) accepts unbound,
%    or if bound must be 0..100.
%    This matches game state reality: partial
%    structures (e.g., game(_, _, X, _, _)) are
%    fine mid-computation.
%
% 5. FIELD-LEVEL VALIDATION
%    No top-level ground/1 guard. Each field
%    validates independently. Unbound C in
%    game(a, b, C, d, e) does NOT stop validation
%    of d and e.
%
% 6. STRUCTURED ERROR REPORTING WITH PATH REVERSAL
%    Errors include Path (field nesting), stored
%    left-to-right for human readability.
%    Example: validation_error(
%             [game_state, objects, [2]],
%             expected_integer, not_int).
%    This tells you exactly where validation broke,
%    in outer-to-inner order.
%
% 7. CROSS-MODULE SCHEMA LOOKUP
%    Uses meta_predicate directive + Module
%    threading so schemas can be defined in
%    caller's module, not just here.
%    Library "just works" when imported elsewhere.
%
% 8. NO SPECIAL SYNTAX
%    Schemas are pure Prolog terms. No DSLs,
%    no code generation. Homoiconic: data can
%    be inspected, printed, stored in files.
%
% USAGE
% =====
%
% 1. Define schema in your module:
%    game_state_schema(struct(game_state, [
%        frame(integer(0, 1000000)),
%        status(enum([playing, won, lost])),
%        ...
%    ])).
%
% 2. Import validation:
%    :- use_module(xod).
%
% 3. Call validator:
%    validate(MyState, game_state_schema)
%    -> Looks up schema in YOUR module
%    -> Validates recursively with module
%    -> Throws on error with full path
%    -> If MyState has unbound fields, they pass
%    -> If bound, they're checked
%
% 4. Handle errors (optional):
%    catch(validate(State, Schema),
%          validation_error(Path, Reason, Value),
%          handle_error(Path, Reason, Value))
%    % Path is now [Outer, ..., Inner] readable
%
% SCHEMA PRIMITIVES
% =================
%
% any                   Always passes (any value)
% integer               Bound: must be integer
% integer(Min, Max)     Bound: in range [Min,Max]
%                       Use _ for unbounded
% atom                  Bound: must be atom
% enum([a,b,c])         Bound: one of list
% list(Schema)          Bound: list of Schema items
% struct(Functor, [...]) Bound: compound term
%                       with correct functor
% schema(Name)          Bound: lookup named schema
% union([S1, S2, ...])  Bound: matches first
%                       succeeding schema
%
% All pass if unbound.
%
% ================================================

:- module(xod, [validate/2]).

:- use_module(library(lists)).
:- use_module(library(iso_ext), [forall/2]).
:- use_module(library(format)).
:- use_module(library(os), [getenv/2]).

% Enable cross-module schema lookups.
% The 2nd arg of validate/2 is a goal in the
% caller's module context. Module is threaded
% through all recursive calls.
:- meta_predicate(validate(+, :)).

% ================================================
% ENTRY POINT
% ================================================

% validate(Term, SchemaName)
% Looks up SchemaName in caller's context,
% validates Term against it.
% Throws validation_error if invalid.
% Passes automatically if Term unbound.
% Threads Module through to enable
% cross-module schema lookup.

validate(Term, Module:SchemaName) :-
    call(Module:SchemaName, Schema),
    validates_against(Term, Schema, [],
                     Module).

% ================================================
% CORE SCHEMA INTERPRETER
% ================================================
% All clauses now take Module as 4th arg
% to enable schema lookup in caller's module.

% ANY: Always passes
validates_against(_, any, _, _) :- !.

% INTEGER: Must be integer if bound
validates_against(V, integer, Path, _) :-
    check(V, integer(V), expected_integer,
          Path).

% INTEGER(Min, Max): Range check if bound
% CRITICAL FIX: Check var BEFORE range check
% to avoid instantiation error
validates_against(V, integer(Min, Max),
                  Path, _) :-
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

% ATOM: Must be atom if bound
validates_against(V, atom, Path, _) :-
    check(V, atom(V), expected_atom, Path).

% ENUM: Must be in options if bound
validates_against(V, enum(Opts), Path, _) :-
    (ground(V) ->
        check(V, memberchk(V, Opts),
              invalid_enum_option, Path)
    ;
        true
    ).

% LIST(Schema): List of items matching
% Schema, if bound. Thread Module through.
validates_against(V, list(Schema),
                  Path, Module) :-
    check(V, is_list(V), expected_list,
          Path),
    check_list_items(V, Schema, Path, 0,
                    Module).

% STRUCT(Functor, Fields): Compound term
% with correct functor and field schemas.
% Thread Module through.
validates_against(V, struct(Functor,
                           Fields), Path,
                  Module) :-
    (var(V) ->
        true
    ;
        check_struct(V, Functor, Fields,
                    Path, Module)
    ).

% SCHEMA(Name): Named reference lookup
% in caller's module. Thread Module through.
validates_against(V, schema(SchemaName),
                  Path, Module) :-
    (var(V) ->
        true
    ;
        (atom(SchemaName) ->
            (call(Module:SchemaName,
                 Definition) ->
                validates_against(V,
                    Definition, Path,
                    Module)
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

% UNION(Schemas): One of multiple schemas
% Tries each schema in order until one
% matches. If V is unbound, pass. If bound,
% try each; throw if none match.
% Example: union([wait_schema,
%                 move_to_schema, ...])
validates_against(V, union(Schemas),
                  Path, Module) :-
    (var(V) ->
        true
    ;
        try_union_schemas(V, Schemas, Path, Module)
    ).

% try_union_schemas/4: Try each schema, catching
% exceptions. If any succeeds, we're done. If all
% fail or throw, throw no_matching_union error.
try_union_schemas(V, [], Path, _Module) :-
    throw_error(Path, no_matching_union, V).
try_union_schemas(V, [Schema|Rest], Path, Module) :-
    ( catch(validates_against(V, Schema, Path, Module),
            validation_error(_, _, _),
            fail) ->
        true  % Schema matched, succeed
    ;
        try_union_schemas(V, Rest, Path, Module)
    ).

% ================================================
% GENERIC HELPER: Boilerplate Reduction
% ================================================

% check(V, Goal, ErrorReason, Path)
% If V is unbound: pass.
% If V is bound: execute Goal.
%   If Goal succeeds: pass.
%   If Goal fails: throw with error.
% Reduces repetitive if-then-else pattern.

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

% Helper: Check if term is a list
% Succeeds if V is [] or [H|T] where T is a list
is_list([]).
is_list([_|T]) :- is_list(T).

% ================================================
% TEST HELPER: Exception Expectation
% ================================================

% expect_exception/1: Succeeds if Goal throws an exception,
% fails otherwise (if Goal succeeds or fails without
% throwing).
expect_exception(Goal) :-
    catch((Goal, fail), _, true).

% ================================================
% ERROR HELPER: Conditional format output
% ================================================

% format_/2: Outputs format message only if
% VALIDATION_ERR_MSG is not set to "false"
format_(Format, Args) :-
    ( getenv("VALIDATION_ERR_MSG", "false") ->
        true  % Suppress output
    ;
        format(Format, Args)  % Output normally
    ).

% ================================================
% ERROR HELPER: Path Reversal and Formatting
% ================================================

% throw_error(RevPath, Reason, Value)
% Reverses the path (built backwards during
% recursion) and throws with correct order.
% Formats and prints error message before throwing.
% Result: validation_error([Outer, Inner],
%         Reason, Value) readable left-to-right.

throw_error(RevPath, Reason, Value) :-
    reverse(RevPath, Path),
    format_error_message(Path, Reason, Value),
    throw(validation_error(Path, Reason,
                          Value)).

% format_error_message/3: Formats and prints
% validation error with path, reason, and value
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

% ================================================
% HELPERS: Integer Range
% ================================================

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

% ================================================
% HELPERS: Struct Validation
% OPTIMIZATION: Use functor/3 + arg/3
% instead of =.. (zero-allocation)
% ================================================

check_struct(V, ExpectedFunctor, Fields,
            Path, Module) :-
    (compound(V) ->
        functor(V, ActualFunctor, Arity),
        (ActualFunctor == ExpectedFunctor ->
            check_struct_arity(V, Arity,
                Fields, Path, Module)
        ;
            throw_error(Path,
                  wrong_functor,
                  ExpectedFunctor)
        )
    ;
        throw_error(Path, expected_struct, V)
    ).

check_struct_arity(V, Arity, Fields,
                   Path, Module) :-
    length(Fields, FLen),
    (Arity == FLen ->
        check_struct_args(V, 1, Fields,
                         Path, Module)
    ;
        throw_error(Path,
              struct_arity_mismatch(FLen),
              Arity)
    ).

% Iterate through struct arguments using arg/3
check_struct_args(_, _, [], _, _).
check_struct_args(V, Idx, [F|Fs],
                 Path, Module) :-
    arg(Idx, V, Val),
    F =.. [FieldName, Schema],
    validates_against(Val, Schema,
        [FieldName|Path], Module),
    NextIdx is Idx + 1,
    check_struct_args(V, NextIdx, Fs,
                     Path, Module).

% ================================================
% HELPERS: List Validation
% Thread Module through.
% ================================================

check_list_items([], _, _, _, _).
check_list_items([Item|Rest], Schema,
                 Path, Index, Module) :-
    validates_against(Item, Schema,
        [[Index]|Path], Module),
    NextIndex is Index + 1,
    check_list_items(Rest, Schema, Path,
                    NextIndex, Module).

% ================================================
% SCHEMA DEFINITIONS
% ================================================

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

% ================================================
% TEST RUNNER
% ================================================

:- discontiguous(test/2).

% Test: valid game_state passes
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

% Test: unbound fields in game_state pass
test("game_state with unbound fields", (
    State = game_state(0, _, playing, 0,
                      1, _, _),
    validate(State, game_state_schema)
)).

% Test: complex valid state
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

% Test: invalid status throws (path correct)
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

% Test: non-integer frame throws
test("non-integer frame throws", (
    State = game_state(not_int, [],
                      playing, 0, 1,
                      [], []),
    expect_exception(validate(State,
                             game_state_schema))
)).

% Test: wrong functor throws
test("wrong functor throws", (
    State = enemy_state(0, [],
                       playing, 0, 1,
                       [], []),
    expect_exception(validate(State,
                             game_state_schema))
)).

% Test: nested error path (objects->index)
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

% Test: valid game_object
test("valid game_object", (
    Obj = game_object(0, static,
                     [], [], []),
    validate(Obj, game_object_schema)
)).

% Test: invalid object type throws
test("invalid object type throws", (
    Obj = game_object(0, invalid_type,
                     [], [], []),
    expect_exception(validate(Obj,
                             game_object_schema))
)).

% Test: integer range - in bounds
test("integer range in bounds", (
    validates_against(50,
                     integer(0, 100),
                     [], user)
)).

% Test: integer range - zero lower
test("integer range zero lower bound", (
    validates_against(0,
                     integer(0, 100),
                     [], user)
)).

% Test: integer range - unbounded upper
test("integer range unbounded upper", (
    validates_against(1000000,
                     integer(0, _),
                     [], user)
)).

% Test: integer range - unbound passes
test("integer range unbound passes", (
    validates_against(_,
                     integer(0, 100),
                     [], user)
)).

% Test: integer range - too high throws
test("integer range too high throws", (
    expect_exception(validates_against(150,
                                     integer(0, 100),
                                     [], user))
)).

% Test: empty list passes
test("empty list passes", (
    validates_against([], list(integer),
                     [], user)
)).

% Test: valid integer list
test("valid integer list", (
    validates_against([1, 2, 3, 4],
                     list(integer),
                     [], user)
)).

% Test: list with invalid item throws
test("list with invalid item throws", (
    expect_exception(validates_against([1, not_int, 3],
                                      list(integer),
                                      [], user))
)).

% Test: enum valid option
test("enum valid option", (
    validates_against(playing,
                     enum([playing, won,
                          lost]),
                     [], user)
)).

% Test: enum invalid option throws
test("enum invalid option throws", (
    expect_exception(validates_against(invalid,
                                      enum([a, b, c]),
                                      [], user))
)).

% Test: any always passes
test("any always passes", (
    validates_against(anything, any, [],
                     user)
)).

% Test: any with unbound passes
test("any with unbound passes", (
    validates_against(_, any, [], user)
)).

% Test: union - first schema matches
test("union first schema matches", (
    validates_against(50,
        union([integer(0, 100), atom]),
        [], user)
)).

% Test: union - second schema matches
test("union second schema matches", (
    validates_against(my_atom,
        union([integer, atom]),
        [], user)
)).

% Test: union - unbound passes
test("union with unbound passes", (
    validates_against(_,
        union([integer, atom]),
        [], user)
)).

% Test: union - no schema matches throws
test("union no schema matches throws", (
    expect_exception(validates_against(3.14,
        union([integer, atom]),
        [], user))
)).

% run_tests :-
%     writeln('====== VALIDATION TESTS ======'),
%     forall(
%         test(Name, Goal),
%         run_single_test(Name, Goal)
%     ),
%     nl,
%     writeln('==============================').

% run_single_test(Name, Goal) :-
%     (call(Goal) ->
%         format('✓ ~w~n', [Name])
%     ;
%         format('✗ ~w~n', [Name])
%     ).

% :- initialization(run_tests).