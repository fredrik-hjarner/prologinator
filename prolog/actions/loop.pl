builtin_action(loop(_)).
builtin_action(loop(_, _)). % loop continuation


execute_action_impl(
    actions_old([loop(Actions)|Rest]),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    execute_loop_managed(
        Actions, Actions, Rest, ID, Status, ActionsOut
    ).

execute_action_impl(
    actions_old([loop(Running, Original)|Rest]),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    execute_loop_managed(
        Running, Original, Rest, ID, Status, ActionsOut
    ).

execute_loop_managed(
    Running, Original, Rest, ID, Status, ActionsOut
) -->
    tick_object(
        actions_old(Running),
        obj_id(ID),
        result(RunStatus, actions_new(RunRemaining))
    ),
    handle_loop_result(
        RunStatus,
        RunRemaining,
        Original,
        Rest,
        ID,
        Status,
        ActionsOut
    ).

handle_loop_result(
    despawned, _, _, _, _, despawned, []
) --> !, [].

handle_loop_result(
    yielded,
    RunRemaining,
    Original,
    Rest,
    _,
    yielded,
    [loop(RunRemaining, Original)|Rest]
) --> !, [].

handle_loop_result(
    completed, _, Original, Rest, ID, Status, ActionsOut
) -->
    execute_loop_managed(
        Original, Original, Rest, ID, Status, ActionsOut
    ).
