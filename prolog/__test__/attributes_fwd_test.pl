:- module(attributes_fwd_test, []).

:- use_module('../../build/prologinator').
:- use_module('../test_utils/test_action_sequence').

:- use_module(library(assoc), [
    empty_assoc/1,
    put_assoc/4
]).

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