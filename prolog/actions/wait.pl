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
            ActionsOut = Rest,
            Status = completed
        ; N = 1 ->
            ActionsOut = Rest,
            Status = yielded
        ; N #> 1 ->
            N1 #= N - 1,
            ActionsOut = [wait(N1)|Rest],
            Status = yielded
        )
    }.
