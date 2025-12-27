% Utility functions

catch_dcg(Goal, Catcher, Handler, S0, S) :-
    catch(
        call(Goal, S0, S),
        Catcher,
        Handler
    ).

% ==========================================================
% Partition helper
% ==========================================================
% partition(+Pred, +List, -Yes, -No)
% Splits List into Yes (elements where Pred succeeds)
%   and No (elements where Pred fails)

partition(_Pred, [], [], []).
partition(Pred, [X|Xs], Yes, No) :-
    ( call(Pred, X) ->
        Yes = [X|YesRest],
        partition(Pred, Xs, YesRest, No)
    ;
        No = [X|NoRest],
        partition(Pred, Xs, Yes, NoRest)
    ).

% ==========================================================
% Select Many helper
% ==========================================================
% select_many(+Patterns, +List, -Remaining)
% Selects multiple items from List matching Patterns,
% returning Remaining list without those items.
% Patterns is a list of terms to match (e.g., [attr(x, X),
%   attr(y, Y)])
%
% Example:
%   select_many([attr(x, CurrX), attr(y, CurrY)],
%               [attr(x, 5), attr(y, 10), attr(z, 3)],
%               Remaining)
%   binds CurrX=5, CurrY=10, Remaining=[attr(z, 3)]

select_many([], List, List).
select_many([Pattern|Patterns], List, Remaining) :-
    select(Pattern, List, ListWithoutPattern),
    select_many(Patterns, ListWithoutPattern, Remaining).

% ==========================================================
% List Check Helper
% ==========================================================
% is_list(+Term)
% Succeeds if Term is a list ([] or [H|T] where T is a list)

is_list([]).
is_list([_|T]) :- is_list(T).

% ==========================================================
% Pretty Print Helper
% ==========================================================
% pretty_print(+Term)
% Pretty prints Term with indentation (like
% JSON.stringify with 2 spaces)
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
% Note: This function does NOT handle cyclic terms
% (will cause infinite recursion). For cyclic
% terms,
% use write_term/2 with max_depth option instead.
%
% Note: Operators like 1 + 2 will be printed in
% canonical form as +(1, 2). This is intentional
% for structural clarity.

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
% Note: Args = [] check is technically dead code
% (compound/1 fails for zero-arity terms), but kept
% for safety/clarity
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
    !, % Cut ensures we don't fall through to
    % improper list handler
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
% Note: Indent here is the *Inner* indentation
% level (already calculated)
pretty_print_args([Arg], Indent) :-
    pretty_print(Arg, Indent).
pretty_print_args([Arg|Args], Indent) :-
    pretty_print(Arg, Indent),
    write(','),
    nl,
    pretty_print_args(Args, Indent).
