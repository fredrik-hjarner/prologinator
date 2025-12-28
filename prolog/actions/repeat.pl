builtin_action(repeat(_, _)).
builtin_action(repeat(_, _, _)). % repeat continuation


execute_action_impl(
    actions_old([repeat(0, _)|Rest]),
    _,
    result(completed, actions_new(Rest))
) --> [].

execute_action_impl(
    actions_old([repeat(Times, Actions)|Rest]),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    {Times #> 0},
    execute_repeat_managed(
        Times,
        Actions,
        Actions,
        Rest,
        ID,
        Status,
        ActionsOut
    ).

execute_action_impl(
    actions_old([repeat(Times, Running, Original)|Rest]),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    execute_repeat_managed(
        Times,
        Running,
        Original,
        Rest,
        ID,
        Status,
        ActionsOut
    ).

execute_repeat_managed(
    Times, Running, Original, Rest, ID, Status, ActionsOut
) -->
    tick_object(
        actions_old(Running),
        obj_id(ID),
        result(RunStatus, actions_new(RunRemaining))
    ),
    handle_repeat_result(
        RunStatus,
        RunRemaining,
        Times,
        Original,
        Rest,
        ID,
        Status,
        ActionsOut
    ).

handle_repeat_result(
    despawned, _, _, _, _, _, despawned, []
) --> [].

handle_repeat_result(
    yielded,
    RunRemaining,
    Times, Original,
    Rest,
    _,
    yielded, [repeat(Times, RunRemaining, Original)|Rest]
) --> [].

handle_repeat_result(
    completed,
    _,
    Times,
    Original,
    Rest,
    ID,
    Status,
    ActionsOut
) -->
    {Times1 #= Times - 1},
    ( {Times1 #> 0} ->
        execute_repeat_managed(
            Times1,
            Original,
            Original,
            Rest,
            ID,
            Status,
            ActionsOut
        )
    ;
        {Status = completed, ActionsOut = Rest}
    ).
