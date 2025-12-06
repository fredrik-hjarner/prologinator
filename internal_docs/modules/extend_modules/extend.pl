% Extension file - extends the same module
% Declares the same module name to add predicates to it
:- module(extend_test, [pred3/1, pred4/1]).

% Third predicate - added to extend_test module
pred3(X) :-
    X = extended_pred3.

% Fourth predicate - added to extend_test module
pred4(X) :-
    X = extended_pred4.

