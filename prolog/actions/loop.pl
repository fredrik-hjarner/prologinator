execute_action_impl(
    actions_old([loop(Actions)|Rest]),
    obj_id(_ID),
    result(completed, actions_new(NewActions))
) -->
    execute_loop(Actions, Rest, NewActions).

execute_loop(Actions, Rest, NewActions) -->
    {
        append(Actions, [loop(Actions)], Expanded),
        append(Expanded, Rest, NewActions)
    }.
