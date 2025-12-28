
exclude(_, [], []).
exclude(Goal, [L|Ls0], Ls) :-
        (   call(Goal, L) ->
            Ls = Rest
        ;   Ls = [L|Rest]
        ),
        exclude(Goal, Ls0, Rest).