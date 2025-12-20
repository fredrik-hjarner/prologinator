execute_action_impl(
    actions_old([noop|Rest]),
    obj_id(_ID),
    result(completed, actions_new(Rest))
) -->
    execute_noop.

execute_noop --> [].
