:- module(attr_if_fwd_test, []).

:- use_module('../../../build/prologinator').
:- use_module('../../test_utils/test_action_sequence').

test("attr_if on attr that exists and condition is true", (
    test_action_sequence(
        start_attrs([
            x-0
        ]),
        actions([
            attr_if(
                .x = 0, % this condition is fulfilled
                [set_attr(.report, 1)], % should do this
                [set_attr(.report, 2)] % shouldnt do this
            )
        ]),
        ticks(1),
        end_attrs([
            x-0,
            report-1
        ])
    )
)).

test("attr_if on attr that exists and condition is false", (
    test_action_sequence(
        start_attrs([
            x-0
        ]),
        actions([
            attr_if(
                .x = 1, % this condition is not fulfilled
                [set_attr(.report, 1)], % should do this
                [set_attr(.report, 2)] % shouldnt do this
            )
        ]),
        ticks(1),
        end_attrs([
            x-0,
            report-2
        ])
    )
)).