:- module(repeat_fwd_test, []).

#include "./build/prologinator.pl"
#include "./prolog/test_utils/test_action_sequence.pl"

test("repeat: runs N times immediately if non-blocking", (
    test_action_sequence(
        start_attrs([
            count-0
        ]),
        actions([
            % incr is instant, so repeat(3) should happen
            % entirely within a single tick.
            repeat(3, [
                incr(count, 1)
            ])
        ]),
        ticks(1),
        end_attrs([
            count-3
        ])
    )
)).

test("repeat: respects yielding in body", (
    test_action_sequence(
        start_attrs([
            count-0
        ]),
        actions([
            repeat(2, [
                incr(count, 1),
                wait(1)
            ])
        ]),
        % Tick 1: incr->1, wait(1) yields. Loop pauses.
        % Tick 2: wait finishes. Loop restarts.
        %         incr->2, wait(1) yields. Loop pauses.
        ticks(2),
        end_attrs([
            count-2
        ])
    )
)).