/**
 * list_to_set/2 - Convert a list to a set (remove duplicates)
 * 
 * Adapted from Scryer Prolog
 * 
 * Copyright (C): 2016-2024 Markus Triska
 * License: MIT-style (see lists.pl for full license text)
 * 
 * Source: https://github.com/mthom/scryer-prolog/blob/master/src/lib/lists.pl
 * 
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files, to deal
 * in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software.
 */

:- module(list_to_set, [list_to_set/2]).

:- use_module(library(hiordlib), [maplist/3, foldl/4]).
:- use_module(library(sort), [keysort/2]).

%% list_to_set(+Ls0, -Set).
%
% Takes a list Ls0 and returns a list Set that doesn't contain any repeated element
%
% ```
% ?- list_to_set([2,3,4,4,1,2], Set).
%    Set = [2,3,4,1].
% ```
list_to_set(Ls0, Ls) :-
        maplist(with_var, Ls0, LVs0),
        keysort(LVs0, LVs),
        same_elements(LVs),
        pick_firsts(LVs0, Ls).

pick_firsts([], []).
pick_firsts([E-V|EVs], Fs0) :-
        (   V == visited ->
            Fs0 = Fs
        ;   V = visited,
            Fs0 = [E|Fs]
        ),
        pick_firsts(EVs, Fs).

with_var(E, E-_).

same_elements([]).
same_elements([EV|EVs]) :-
        foldl(unify_same, EVs, EV, _).

unify_same(E-V, Prev-Var, E-V) :-
        (   Prev == E ->
            Var = V
        ;   true
        ).

