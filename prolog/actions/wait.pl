builtin_action(wait(_)).

execute_action_impl(
    actions_old([wait(N)|Rest]),
    obj_id(_ID),
    result(Status, actions_new(ActionsOut))
) -->
    execute_wait(N, Rest, Status, ActionsOut).

execute_wait(N, Rest, Status, ActionsOut) -->
    {
        ( N = 0 ->
            % wait(0): noop, removes itself, completes
            ActionsOut = Rest,
            Status = completed
        ; N = 1 ->
            % wait(1): yields but also removes itself
            ActionsOut = Rest,
            Status = yielded
        ; N #> 1 ->
            % wait(N>1): decrement and yield
            N1 #= N - 1,
            ActionsOut = [wait(N1)|Rest],
            Status = yielded
        )
    }.
