% Test reverse order: load extend first, then base
:- module(test_extend_reverse, []).

% Load extend first, then base - base.pl is loaded last
:- use_module('./extend', []).
:- use_module('./base', []).

% Test: After loading extend then base, base predicates should NOT be callable
% This demonstrates that you cannot extend a module by redeclaring it
test("load extend then base - base predicates NOT accessible", (
    catch(extend_test:pred1(_), Error, 
        Error = error(existence_error(procedure, _), _)
    )
)).

% Test: pred3 and pred4 from extend.pl should also NOT be callable
% The module redefinition breaks everything
test("extend predicates also not callable after base loads", (
    catch(extend_test:pred3(_), Error, 
        Error = error(existence_error(procedure, _), _)
    )
)).

:- discontiguous(test/2).

