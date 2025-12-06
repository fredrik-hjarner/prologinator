% Base module file - defines the module and some predicates
:- module(extend_test, [pred1/1, pred2/1]).

% First predicate defined in base file
pred1(X) :-
    X = base_pred1.

% Second predicate defined in base file
pred2(X) :-
    X = base_pred2.

