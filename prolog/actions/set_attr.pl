builtin_action(set_attr(_, _)).

execute_action_impl(
    actions_old([set_attr(Path, Value)|Rest]),
    obj(Obj),
    result(completed, actions_new(Rest))
) -->
    % Path is always a path.
    % Value should always "resolve" to a value.
    resolve_arg(Obj, Value, ResolvedValue),
    execute_set_attr(Obj, Path, ResolvedValue).

execute_set_attr(Obj, Path, Value) -->
    resolve_path_to_attr(Obj, Path, TargetID/Key),
    ctx_set_attr_val(TargetID/Key, Value).


