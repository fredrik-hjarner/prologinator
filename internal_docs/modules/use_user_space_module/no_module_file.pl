% This file has NO module declaration
% Predicates should go into user namespace

test_pred1(X) :-
    X = no_module_pred1.

test_pred2(X, Y) :-
    X = first,
    Y = second.

test_fact(hello).
test_fact(world).

