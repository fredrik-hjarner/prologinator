% run with:
% scryer-prolog internal_docs/modules/use_user_space_module/test_user_space.pl 

% Test if files without module declarations go into user namespace
:- module(test_user_space, []).

% Load the file without module declaration
:- use_module('./no_module_file').


run_tests :-
    test_fact(X).

:- initialization(run_tests).