

test_fixture(Actions, AttrListKV, Ctx) :-
    empty_ctx(C0),
    
    TargetID = 0,
    ctx_set_objs([object(id(TargetID))], C0, C1),
    
    maplist(kv_to_attr, AttrListKV, InternalAttrs),
    empty_assoc(EmptyA),
    put_assoc(TargetID, EmptyA, InternalAttrs, AttrStore),
    ctx_set_attrs(AttrStore, C1, C2),
    
    empty_assoc(EmptyAct),
    put_assoc(TargetID, EmptyAct, [Actions], ActStore),
    ctx_set_actionstore(ActStore, C2, Ctx).

kv_to_attr(Key-Value, attr(Key, Value)).


run_ticks(0, Ctx, Ctx) :- !.
run_ticks(N, CtxIn, CtxOut) :-
    N > 0,
    tick(CtxIn, CtxMid),
    N1 is N - 1,
    run_ticks(N1, CtxMid, CtxOut).



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
    ( member(attr(Key, ActualVal), ActualList) ->
        ( ActualVal = ExpectedVal ->
            check_expectations(Rest, ActualList)
        ;
            format("   FAIL: Attr '~w' mismatch.~n", [Key]),
            format("      Expected: ~w~n", [ExpectedVal]),
            format("      Actual:   ~w~n~n", [ActualVal]),
            fail
        )
    ;
        format("   FAIL: Attr '~w' missing.~n~n", [Key]),
        fail
    ).


test_action_sequence(
    start_attrs(StartAttrs),
    actions(Actions),
    ticks(Ticks),
    end_attrs(EndAttrs)
) :-
    test_fixture(Actions, StartAttrs, Ctx0),
    run_ticks(Ticks, Ctx0, CtxResult),
    assert_attrs(CtxResult, EndAttrs).

test_action_sequence(
    start_attrs(StartAttrs),
    actions(Actions),
    ticks(Ticks),
    error(ExpectedError)
) :-
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