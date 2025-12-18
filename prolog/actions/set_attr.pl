% set_attr action implementation

execute_action_impl(
    action(set_attr(Key, Value)),
    actions_old([_|Rest]),
    obj_id(ID),
    result(completed, actions_new(Rest))
) -->
    execute_set_attr(ID, Key, Value).

execute_action_impl(
    action(set_attr(TargetID, Key, Value)),
    actions_old([_|Rest]),
    obj_id(_ID),
    result(completed, actions_new(Rest))
) -->
    execute_set_attr(TargetID, Key, Value).

% ==========================================================
% execute_set_attr/6
% ==========================================================
execute_set_attr(TargetID, Key, Value) -->
    ctx_set_attr_val(TargetID/Key, Value).


