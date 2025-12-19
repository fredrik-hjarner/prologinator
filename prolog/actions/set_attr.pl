% set_attr action implementation

execute_action_impl(
    action(set_attr(Path, Value)),
    actions_old([_|Rest]),
    obj_id(MyID),
    result(completed, actions_new(Rest))
) -->
    execute_set_attr(MyID, Path, Value).

% ==========================================================
% execute_set_attr/6
% ==========================================================
execute_set_attr(MyID, Path, Value) -->
    resolve_path_to_attr(MyID, Path, TargetID/Key),
    ctx_set_attr_val(TargetID/Key, Value).


