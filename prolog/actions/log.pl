% log action implementation

execute_action_impl(
    action(log(Msg)),
    actions_old([_|Rest]),
    obj_id(_ID),
    result(completed, actions_new(Rest))
) -->
    execute_log(Msg).

% ==========================================================
% execute_log/3
% ==========================================================
execute_log(Msg) -->
    {format("~s~n", [Msg])}.


