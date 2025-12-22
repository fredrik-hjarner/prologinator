% test_action_sequence
% 
% I think `test_action_sequence` really should be able to
% test most actions sequences and such. I mean yea it
% only allows setting start attributes on object id 0 and
% end attributes on object id 0 but we just have to be smart
% for example if we are testing something more complicated
% such as one object spawning another spawning another
% with forks and parallel_all: n stuff we can "register"
% stuff on object with id 0! If we write tests and such a
% way then the number of lines of code they takes up can be
% reduced substantially.

:- module(test_action_sequence, [
    test_action_sequence/4
]).

:- use_module('../../build/prologinator').

:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(library(format)).

% ==========================================================
% 1. Setup / Fixture
% ==========================================================

% test_fixture(+Actions, +StartAttrs, -Ctx)
% Creates a valid context with:
% - One object (ID 1)
% - The provided list of Actions (queued for execution)
% - The provided list of attrs (e.g., [x-0, y-0, hp-100])
test_fixture(Actions, AttrListKV, Ctx) :-
    % 1. Create Empty Context
    empty_ctx(C0),
    
    % 2. Setup Object 1
    TargetID = 0,
    ctx_set_objs([object(id(TargetID))], C0, C1),
    
    % 3. Setup Attributes
    % Convert friendly [k-v, ...] list to internal
    % [attr(k,v), ...]
    maplist(kv_to_attr, AttrListKV, InternalAttrs),
    empty_assoc(EmptyA),
    put_assoc(TargetID, EmptyA, InternalAttrs, AttrStore),
    ctx_set_attrs(AttrStore, C1, C2),
    
    % 4. Setup Actions
    % Wrap actions in a list because the engine expects a
    % list of action streams
    % i.e. [[Action1, Action2]]
    empty_assoc(EmptyAct),
    put_assoc(TargetID, EmptyAct, [Actions], ActStore),
    ctx_set_actionstore(ActStore, C2, Ctx).

% Helper: k-v pair to internal attr/2 term
kv_to_attr(Key-Value, attr(Key, Value)).

% ==========================================================
% 2. Execution Helpers
% ==========================================================

% run_ticks(+N, +CtxIn, -CtxOut)
% Runs the engine for N frames
run_ticks(0, Ctx, Ctx) :- !.
run_ticks(N, CtxIn, CtxOut) :-
    N > 0,
    tick(CtxIn, CtxMid),
    N1 is N - 1,
    run_ticks(N1, CtxMid, CtxOut).


% ==========================================================
% 3. Assertion Helpers
% ==========================================================

% assert_attrs(+Ctx, +ExpectedKV)
% Verifies Object 1 has expected attribute values.
% ExpectedKV format: [x-10, hp-90]
% Order independent. Fails with debug info on mismatch.
assert_attrs(Ctx, ExpectedKV) :-
    TargetID = 0,
    ctx_attrs(AttrStore, Ctx, _),
    ( get_assoc(TargetID, AttrStore, ActualAttrsList) ->
        check_expectations(ExpectedKV, ActualAttrsList)
    ;
        format(
          "   FAIL: Object ~w missing from attr store.~n~n",
          [TargetID]
        ),
        fail
    ).

check_expectations([], _).
check_expectations([Key-ExpectedVal|Rest], ActualList) :-
    % 1. Find attribute by Key (ignore value for now)
    % This makes it order-independent
    ( member(attr(Key, ActualVal), ActualList) ->
        % 2. Check Value match
        ( ActualVal = ExpectedVal ->
            check_expectations(Rest, ActualList)
        ;
            format("   FAIL: Attr '~w' mismatch.~n", [Key]),
            format("      Expected: ~w~n", [ExpectedVal]),
            format("      Actual:   ~w~n~n", [ActualVal]),
            fail
        )
    ;
        % 3. Key not found in the actual list
        format("   FAIL: Attr '~w' missing.~n~n", [Key]),
        fail
    ).

% ==========================================================
% 3. The Test Wrapper
% ==========================================================

% CASE A: Expect Success and Check Attributes
% 1. Sets up the world with Object 1.
% 2. Runs for `Ticks` frames.
% 3. Asserts that Object 1 has `EndAttrs`.
test_action_sequence(
    start_attrs(StartAttrs),
    actions(Actions),
    ticks(Ticks),
    end_attrs(EndAttrs)
) :-
    % Run normally. If this throws, the test fails (good).
    test_fixture(Actions, StartAttrs, Ctx0),
    run_ticks(Ticks, Ctx0, CtxResult),
    assert_attrs(CtxResult, EndAttrs).

% CASE B: Expect Exception
test_action_sequence(
    start_attrs(StartAttrs),
    actions(Actions),
    ticks(Ticks),
    error(ExpectedError)
) :-
    % Wrap entire execution in catch to verify error
    catch(
        (
            test_fixture(Actions, StartAttrs, Ctx0),
            run_ticks(Ticks, Ctx0, _),
            Result = success
        ),
        ActualError,
        Result = error(ActualError)
    ),
    verify_error_result(Result, ExpectedError).

% Helper to check if the error matched
verify_error_result(success, Expected) :-
    format(
        "   FAIL: Expected exception, but succeeded.~n",
        []
    ),
    format("      Expected: ~w~n~n", [Expected]),
    fail.

verify_error_result(error(Actual), Expected) :-
    ( subsumes_term(Expected, Actual) ->
        true
    ;
        format("   FAIL: Wrong exception thrown.~n"),
        format("      Expected: ~w~n", [Expected]),
        format("      Actual:   ~w~n~n", [Actual]),
        fail
    ).