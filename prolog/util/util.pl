% Utility functions

:- module(util, [partition/4, flatten/2]).

:- use_module(library(lists), [append/2]).
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

