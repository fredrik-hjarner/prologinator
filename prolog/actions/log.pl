builtin_action(log(_)).

execute_action_impl(
    actions_old([log(Msg)|Rest]),
    obj_id(_ID),
    result(completed, actions_new(Rest))
) -->
    execute_log(Msg).

execute_log(Msg) -->
    {format("~s~n", [Msg])}.


