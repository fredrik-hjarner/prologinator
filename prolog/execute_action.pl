




execute_action(
    actions_old([Action|Rest]),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    resolve_action(ID, Action, ResolvedAction),
    execute_action_resolved(
        actions_old([ResolvedAction|Rest]),
        obj_id(ID),
        result(Status, actions_new(ActionsOut))
    ).

execute_action_resolved(
    actions_old([Action|Rest]),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    ( {builtin_action(Action)} ->
#ifdef ENABLE_LOG_ACTIONS
        {functor(Action, Functor, _)},
        {format("executing `~w`~n", [Functor])},
#endif
        {findall(
            F/Arity,
            (
                clause(
                    execute_action_impl(
                        actions_old([ActionPattern|_]),
                        _,
                        _,
                        _,
                        _
                    ),
                    _
                ),
                functor(ActionPattern, F, Arity)
            ),
            FunctorArities
        )},
        {format("Available actions: ~w~n", [FunctorArities])},
        catch_dcg(
            execute_action_impl(
                actions_old([Action|Rest]),
                obj_id(ID),
                result(Status, actions_new(ActionsOut))
            ),
            Error,
            ( write('Error during execute_action_impl: '),
              write(Error), nl,
              halt(1)
            )
        )
    ; {user_action(Action, Body)} ->
        execute_action(
            actions_old([Body|Rest]),
            obj_id(ID),
            result(Status, actions_new(ActionsOut))
        )
    ;
        {throw(unknown_action(Action))}
    ).


