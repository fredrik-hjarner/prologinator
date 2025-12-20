execute_action_impl(
    action(noop),
    actions_old([_|Rest]),
    obj_id(_ID),
    result(completed, actions_new(Rest))
) -->
    execute_noop.

execute_noop --> [].

