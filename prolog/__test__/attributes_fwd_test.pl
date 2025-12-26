:- module(attributes_fwd_test, []).

#include "./build/prologinator.pl"
#include "./prolog/test_utils/test_action_sequence.pl"

test("simply set an attribute verify that it was set",(
    test_action_sequence(
        start_attrs(
            [hp-0]
        ),
        actions(
            [set_attr(.hp, 100)]
        ),
        ticks(
            1
        ),
        end_attrs(
            [hp-100]
        )
    )
)).

% TODO: Make this fail then enable the test.
% test("set attribute with invalid syntax. should fail!.",(
%     test_action_sequence(
%         start_attrs(
%             [hp-0]
%         ),
%         actions(
%             [set_attr(hp, 100)]
%         ),
%         ticks(
%             1
%         ),
%         error(
%             _
%         )
%     )
% )).

test("set attribute then copy.",(
    test_action_sequence(
        start_attrs([
            hp-0
        ]),
        actions([
            set_attr(.hp, 100),
            copy_attr(.hp, .hp2)
        ]),
        ticks(
            1
        ),
        end_attrs([
            hp-100,
            hp2-100
        ])
    )
)).

test("default should be utilized for non-existing stuff.",(
    test_action_sequence(
        start_attrs([
        ]),
        actions([
            wait_until(default(.parent_id.hp, -1) < 1),
            set_attr(.yup, true)
        ]),
        ticks(
            1
        ),
        end_attrs([
            yup-true
        ])
    )
)).

test("default should be utilized for non-existing stuff. \
In this case a non-existing object is referenced.",(
    test_action_sequence(
        start_attrs([
            % pointer to object with id 5 that doesn't exist
            ptr-5
        ]),
        actions([
            wait_until(default(.ptr.hp, -1) < 1),
            set_attr(.yup, true)
        ]),
        ticks(
            1
        ),
        end_attrs([
            yup-true
        ])
    )
)).