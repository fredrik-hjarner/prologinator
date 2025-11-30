:- module(run_tests, [], []).

:- use_module(library(unittest), [run_tests_in_module/1]).
:- use_module('game').

:- initialization(run_tests_in_module('game.pl')).

