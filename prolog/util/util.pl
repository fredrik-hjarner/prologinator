
catch_dcg(Goal, Catcher, Handler, S0, S) :-
    catch(
        call(Goal, S0, S),
        Catcher,
        Handler
    ).
