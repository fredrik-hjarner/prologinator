% Utility functions

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
% List Check Helper
% ==========================================================
% is_list(+Term)
% Succeeds if Term is a list ([] or [H|T] where T is a list)

is_list([]).
is_list([_|T]) :- is_list(T).
