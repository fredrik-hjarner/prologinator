:- module(list_fwd_test, []).

#include "./build/prologinator.pl"
#include "./prolog/test_utils/test_action_sequence.pl"

test("list: blocks outer actions until inner finishes", (
    test_action_sequence(
        start_attrs([
            inner-0,
            outer-0
        ]),
        actions([
            % Inner list yields due to wait(1)
            list([
                wait(1),
                set_attr(inner, 1)
            ]),
            % This should NOT run until list completes
            set_attr(outer, 1)
        ]),
        % Tick 1: list yields (waiting). No attrs set.
        % Tick 2: list resumes, sets inner, finishes.
        %         Then outer runs.
        ticks(2),
        end_attrs([
            inner-1,
            outer-1
        ])
    )
)).

test("list: empty list completes immediately", (
    test_action_sequence(
        start_attrs([
            check-0
        ]),
        actions([
            % Empty list should vanish instantly
            list([]),
            set_attr(check, 1)
        ]),
        ticks(1),
        end_attrs([
            check-1
        ])
    )
)).