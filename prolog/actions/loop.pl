execute_action_impl(
    action(loop(Actions)),
    actions_old([_|Rest]),
    obj_id(_ID),
    result(completed, actions_new(NewActions))
) -->
    execute_loop(Actions, Rest, NewActions).

execute_loop(Actions, Rest, NewActions) -->
    {
        append(Actions, [loop(Actions)], Expanded),
        append(Expanded, Rest, NewActions)
    }.


