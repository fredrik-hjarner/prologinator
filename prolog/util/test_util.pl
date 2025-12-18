:- module(test_util, [err_write/1, err_format/2]).

% ==========================================================
% Test Utility Functions
% ==========================================================
% Test-related utilities for assertions and error reporting

% ==========================================================
% Error Output Helpers
% ==========================================================
% err_write(+Message)
% Writes Message and then fails. Useful for error messages
%   in test assertions.
%
% err_format(+Format, +Args)
% Formats message using Format and Args, then fails.
%   Useful for error messages in test assertions.

err_write(Msg) :-
    write(user_output, 'ERROR: '),
    write(user_output, Msg),
    nl,
    halt(1).

err_format(Fmt, Args) :-
    format(Fmt, Args),
    nl,
    halt(1).

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

% ==========================================================
% Pretty Print Helper
% ==========================================================
% pretty_print(+Term)
% Pretty prints Term with indentation (like JSON.stringify with 2 spaces)
% Makes complex Prolog terms much easier to read
%
% Example:
%   pretty_print([a, b, [c, d], e(f, g)])
%   Output:
%   [
%     a,
%     b,
%     [
%       c,
%       d
%     ],
%     e(
%       f,
%       g
%     )
%   ]
%
% Note: This function does NOT handle cyclic terms (will cause
%   infinite recursion). For cyclic terms, use write_term/2 with
%   max_depth option instead.
%
% Note: Operators like 1 + 2 will be printed in canonical form
%   as +(1, 2). This is intentional for structural clarity.

pretty_print(Term) :-
    pretty_print(Term, 0).

% pretty_print(+Term, +IndentLevel)
% Pretty prints Term with IndentLevel spaces of indentation

% 1. Variables
pretty_print(Term, Indent) :-
    var(Term), !,
    print_indent(Indent),
    write(Term).

% 2. Atoms
pretty_print(Term, Indent) :-
    atom(Term), !,
    print_indent(Indent),
    writeq(Term).

% 3. Numbers
pretty_print(Term, Indent) :-
    number(Term), !,
    print_indent(Indent),
    write(Term).

% 4. Empty List
pretty_print([], Indent) :- !,
    print_indent(Indent),
    write('[]').

% 5. Non-Empty Lists
pretty_print([H|T], Indent) :- !,
    print_indent(Indent),
    write('['),
    nl,
    NextIndent is Indent + 1,
    pretty_print(H, NextIndent),
    pretty_print_list_tail(T, NextIndent),
    nl,
    print_indent(Indent),
    write(']').

% 6. Compounds (and Dicts/Strings if supported by dialect)
% Note: Args = [] check is technically dead code (compound/1 fails
%   for zero-arity terms), but kept for safety/clarity
pretty_print(Term, Indent) :-
    compound(Term), !,
    Term =.. [Functor|Args],
    print_indent(Indent),
    writeq(Functor),
    (   Args = [] -> true
    ;   write('('),
        nl,
        NextIndent is Indent + 1,
        pretty_print_args(Args, NextIndent),
        nl,
        print_indent(Indent),
        write(')')
    ).

% 7. Catch-all (Strings, Blobs, etc.)
pretty_print(Term, Indent) :-
    print_indent(Indent),
    writeq(Term).

% ----------------------------------------------------------
% Helpers
% ----------------------------------------------------------

% Efficient Indentation (2 spaces per level)
print_indent(0).
print_indent(Level) :-
    Level > 0,
    write('  '),
    NextLevel is Level - 1,
    print_indent(NextLevel).

% Handle List Tails (including improper lists [a|b])
pretty_print_list_tail([], _).
pretty_print_list_tail([H|T], Indent) :-
    !, % Cut ensures we don't fall through to improper list handler
    write(','),
    nl,
    pretty_print(H, Indent),
    pretty_print_list_tail(T, Indent).
pretty_print_list_tail(Tail, Indent) :-
    % Handle improper list (dotted pair)
    write(','),
    nl,
    print_indent(Indent),
    write('| '),
    writeq(Tail).

% Handle Compound Arguments
% Note: Indent here is the *Inner* indentation level (already calculated)
pretty_print_args([Arg], Indent) :-
    pretty_print(Arg, Indent).
pretty_print_args([Arg|Args], Indent) :-
    pretty_print(Arg, Indent),
    write(','),
    nl,
    pretty_print_args(Args, Indent).

