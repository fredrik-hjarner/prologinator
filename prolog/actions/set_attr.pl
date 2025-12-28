builtin_action(set_attr(_, _)).

execute_action_impl(
    actions_old([set_attr(Path, Value)|Rest]),
    obj_id(MyID),
    result(completed, actions_new(Rest))
) -->
    % Path is always a path.
    % Value should always "resolve" to a value.
    resolve_arg(MyID, Value, ResolvedValue),
    execute_set_attr(MyID, Path, ResolvedValue).

execute_set_attr(MyID, Path, Value) -->
    resolve_path_to_attr(MyID, Path, TargetID/Key),
    ctx_set_attr_val(TargetID/Key, Value).


