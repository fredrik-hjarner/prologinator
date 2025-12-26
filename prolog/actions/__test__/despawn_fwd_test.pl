:- module(despawn_fwd_test, []).

#include "./build/prologinator.pl"
#include "./prolog/test_utils/test_action_sequence.pl"

test("despawn: stops execution of remaining actions", (
    test_action_sequence(
        start_attrs([
            probe-0
        ]),
        actions([
            % Spawn a sacrificial object to test despawn
            % behavior
            spawn([
                % 1. Set probe to 1 (Proof of life)
                set_attr(parent_id.probe, 1),
                % 2. Despawn self
                despawn,
                % 3. Set probe to 2 (Should never happen)
                set_attr(parent_id.probe, 2)
            ])
            % Parent waits to allow child to tick
        ]),
        ticks(3),
        end_attrs([
            probe-1
        ])
    )
)).