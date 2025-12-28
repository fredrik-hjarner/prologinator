
catch_dcg(Goal, Catcher, Handler, S0, S) :-
    catch(
        call(Goal, S0, S),
        Catcher,
        Handler
    ).

is_list([]).
is_list([_|T]) :- is_list(T).
