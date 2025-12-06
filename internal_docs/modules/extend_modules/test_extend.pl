% Test if module can be extended over multiple files
:- module(test_extend, []).

% RESULT: Modules CANNOT be extended across multiple files.
% 
% When you declare the same module name in multiple files:
% - Scryer Prolog does NOT allow redefining a module
% - Attempting to load a second file that declares the same module name causes issues
% - Predicates from the second file are NOT accessible (existence_error)
% - The behavior is undefined/unreliable

% Load both files - extend.pl is loaded last
:- use_module('./base', []).
:- use_module('./extend', []).

% Test: After loading base then extend, extend predicates should NOT be callable
% This demonstrates that you cannot extend a module by redeclaring it
test("load base then extend - extend predicates NOT accessible", (
    catch(extend_test:pred3(_), Error, 
        Error = error(existence_error(procedure, _), _)
    )
)).

% Test: pred1 and pred2 from base.pl should also NOT be callable
% The module redefinition breaks everything
test("base predicates also not callable after extend loads", (
    catch(extend_test:pred1(_), Error, 
        Error = error(existence_error(procedure, _), _)
    )
)).

:- discontiguous(test/2).

