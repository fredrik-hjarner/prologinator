% exclude/3 - Filter elements from a list
% 
% Adapted from Scryer Prolog
% 
% Copyright (C): 2016-2024 Markus Triska
% License: MIT-style (see clpz.pl for full license text)

%% exclude(+Goal, +List, -Filtered).
%
% Filter elements from List where Goal succeeds,
% returning Filtered.
% Goal is called as call(Goal, Element).
%
% Example:
% ?- exclude(>(3), [1,2,3,4,5], Filtered).
%    Filtered = [1,2,3].
exclude(_, [], []).
exclude(Goal, [L|Ls0], Ls) :-
        (   call(Goal, L) ->
            Ls = Rest
        ;   Ls = [L|Rest]
        ),
        exclude(Goal, Ls0, Rest).