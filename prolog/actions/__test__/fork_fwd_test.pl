:- module(fork_fwd_test, []).

#include "./build/prologinator.pl"
#include "./prolog/test_utils/test_action_sequence.pl"

test("fork: executes new stream in same frame", (
    test_action_sequence(
        start_attrs([
            probe-0
        ]),
        actions([
            % Fork an action that sets probe immediately.
            % If fork works, this runs in the current tick.
            fork([
                set_attr(probe, 1)
            ])
        ]),
        ticks(1),
        end_attrs([
            probe-1
        ])
    )
)).

test("fork: runs in parallel with original stream", (
    test_action_sequence(
        start_attrs([
            forked-0,
            orig-0
        ]),
        actions([
            % Stream 1: Wait 1, then set 'forked'
            fork([
                wait(1),
                set_attr(forked, 1)
            ]),
            % Stream 0: Wait 1, then set 'orig'
            wait(1),
            set_attr(orig, 1)
        ]),
        % Both streams wait 1 tick, then execute setters
        % in the 2nd tick.
        ticks(2),
        end_attrs([
            forked-1,
            orig-1
        ])
    )
)).