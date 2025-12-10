:- module(yields_test, []).
:- use_module('./yields', [yields/2]).

% ==========================================================
% Tests for yields/2
% ==========================================================

test("yields: wait with positive number should \
yield", (
    A = wait(5),
    yields(A, true)
)).

test("yields: wait with zero should not yield", (
    A = wait(0),
    yields(A, false)
)).

test("yields: move_to with positive frames should yield", (
    A = move_to(10, 20, 3),
    yields(A, true)
)).

test("yields: can generate yielding actions \
bidirectionally", (
    yields(A, true),
    ( A = wait(_)
    ; A = move_to(_, _, _)
    ; A = parallel_all_running(_) )
)).

