builtin_action(wait). % wait 1 frame.
builtin_action(wait(_)). % wait N frames.

execute_action_impl(
    actions_old([wait|Rest]),
    obj(_Obj),
    result(yielded, actions_new(Rest))
) --> !, [].

execute_action_impl(
    actions_old([wait(N)|Rest]),
    obj(Obj),
    result(Status, actions_new(ActionsOut))
) -->
    % N should always "resolve" to a value.
    resolve_arg(Obj, N, ResolvedN),
    execute_wait(ResolvedN, Rest, Status, ActionsOut).

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
