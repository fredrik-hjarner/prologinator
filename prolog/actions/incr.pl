builtin_action(incr(_)). % increment by 1.
builtin_action(incr(_, _)). % increment by amount.

% increment by 1.
execute_action_impl(
    actions_old([incr(Path)|Rest]),
    obj(Obj),
    result(completed, actions_new(Rest))
) -->
    execute_incr(Obj, Path, 1).

% increment by amount.
execute_action_impl(
    actions_old([incr(Path, Amount)|Rest]),
    obj(Obj),
    result(completed, actions_new(Rest))
) -->
    % Path is always a path.
    % Amount should always "resolve" to a value.
    resolve_arg(Obj, Amount, ResolvedAmount),
    execute_incr(Obj, Path, ResolvedAmount).

execute_incr(Obj, Path, Amount) -->
    resolve_path_to_attr(Obj, Path, TargetID/Key),
    ( ctx_attr_val(TargetID/Key, CurrentValue) ->
        {NewValue #= CurrentValue + Amount}
    ;
        {NewValue = Amount}
    ),
    ctx_set_attr_val(TargetID/Key, NewValue).
