% Utility functions

:- module(util, [
    partition/4,
    flatten/2,
    select_many/3,
    err_write/1,
    err_format/2,
    is_list/1
]).

:- use_module(library(lists), [append/2, select/3]).
:- use_module(library(format)).
:- meta_predicate(partition(1, ?, ?, ?)).

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
% Flatten helper
% ==========================================================
% flatten(+ListOfLists, -FlatList)
% Note: flattening is shallow!
% Flattens a list of lists into a single list
% This is an alias for append/2 from library(lists)

flatten(ListOfLists, FlatList) :-
    append(ListOfLists, FlatList).

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
% List Check Helper
% ==========================================================
% is_list(+Term)
% Succeeds if Term is a list ([] or [H|T] where T is a list)

is_list([]).
is_list([_|T]) :- is_list(T).

% ==========================================================
% Compiler Hooks
% ==========================================================
% Prolog is the best programming language ever...
% ...but Prolog's module system is inarguable the worst
% thing ever coded.
% This goal expansion thing only exists to fix it's horrible
% DX.

:- multifile(user:goal_expansion/2).

% NOTE: You only need to import this module but not expect
%       specifically because it's goal expansion stuff.
% Rewrite expect(Goal) to inline the check.
% We unwrap 'call(Goal)' to 'Goal' so the compiler sees
% imports.
% We use 'true' instead of '!' because '->' automatically
% commits.
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