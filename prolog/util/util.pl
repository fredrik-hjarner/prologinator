
catch_dcg(Goal, Catcher, Handler, S0, S) :-
    catch(
        call(Goal, S0, S),
        Catcher,
        Handler
    ).


partition(_Pred, [], [], []).
partition(Pred, [X|Xs], Yes, No) :-
    ( call(Pred, X) ->
        Yes = [X|YesRest],
        partition(Pred, Xs, YesRest, No)
    ;
        No = [X|NoRest],
        partition(Pred, Xs, Yes, NoRest)
    ).


select_many([], List, List).
select_many([Pattern|Patterns], List, Remaining) :-
    select(Pattern, List, ListWithoutPattern),
    select_many(Patterns, ListWithoutPattern, Remaining).


is_list([]).
is_list([_|T]) :- is_list(T).
