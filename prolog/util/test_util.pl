% ==========================================================
% Test Utility Functions
% ==========================================================
% Test-related utilities for assertions and error reporting

% ==========================================================
% Compiler Hooks
% ==========================================================
% Prolog is the best programming language ever...
% ...but Prolog's module system is inarguable the worst
% thing ever coded.
% This goal expansion thing only exists to fix it's horrible
% DX.

% NOTE: You only need to import this module but not expect
%       specifically because it's goal expansion stuff.
% Rewrite expect(Goal) to inline the check.
% We unwrap 'call(Goal)' to 'Goal' so the compiler sees
% imports.
% We use 'true' instead of '!' because '->' automatically
% commits.
% Must hook into user module for the compiler to pick it up
% TODO: Remove `multifile`.
% TODO: I can change `expect` into a normal predicate now
%       since I am no longer using Prolog's crappy module
%       system.
:- multifile(user:goal_expansion/2).

user:goal_expansion(expect(Goal, Message), Expanded) :-
    Expanded = (
        Goal ->
            true
        ;
            write(user_output, 'ERROR: Assertion failed: '),
            write(user_output, Message),
            nl,
            halt(1)
    ).
user:goal_expansion(expect(Goal), Expanded) :-
    Expanded = (
        Goal ->
            true
        ;
            write(user_output, 'ERROR: Assertion failed: '),
            write_term(user_output, Goal,
                [quoted(true), max_depth(3)]),
            nl,
            halt(1)
    ).
