% Utility functions

:- module(util, [
    partition/4,
    flatten/2,
    select_many/3,
    err_write/1,
    err_format/2
]).

:- use_module(library(lists), [append/2, select/3]).
:- meta_predicate(partition(1, ?, ?, ?)).

% ==========================================================
% Partition helper
% ==========================================================
% partition(+Pred, +List, -Yes, -No)
% Splits List into Yes (elements where Pred succeeds) and No (elements where Pred fails)

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
    write(user_output, Msg),
    nl,
    fail.

err_format(Fmt, Args) :-
    format(Fmt, Args),
    nl,
    fail.

