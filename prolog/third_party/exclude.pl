/**
 * exclude/3 - Filter elements from a list
 * 
 * Adapted from Scryer Prolog
 * 
 * Copyright (C): 2016-2024 Markus Triska
 * License: MIT-style (see clpz.pl for full license text)
 * 
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files, to deal
 * in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software.
 */

:- module(exclude, [exclude/3]).

:- use_module(engine(hiord_rt), [call/2]).

:- meta_predicate exclude(pred(1), ?, ?).

%% exclude(+Goal, +List, -Filtered).
%
% Filter elements from List where Goal succeeds, returning Filtered.
% Goal is called as call(Goal, Element).
%
% ```
% ?- exclude(>(3), [1,2,3,4,5], Filtered).
%    Filtered = [1,2,3].
% ```
exclude(_, [], []).
exclude(Goal, [L|Ls0], Ls) :-
        (   call(Goal, L) ->
            Ls = Rest
        ;   Ls = [L|Rest]
        ),
        exclude(Goal, Ls0, Rest).