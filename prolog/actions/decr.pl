builtin_action(decr(_)). % decrement by 1.
builtin_action(decr(_, _)). % decrement by amount.

% decrement by 1.
execute_action_impl(
    actions_old([decr(Path)|Rest]),
    obj(Obj),
    result(completed, actions_new(Rest))
) -->
    {obj_id(Obj, MyID)},
    execute_decr(MyID, Path, 1).

% decrement by amount.
execute_action_impl(
    actions_old([decr(Path, Amount)|Rest]),
    obj(Obj),
    result(completed, actions_new(Rest))
) -->
    {obj_id(Obj, MyID)},
    % Path is always a path.
    % Amount should always "resolve" to a value.
    resolve_arg(MyID, Amount, ResolvedAmount),
    execute_decr(MyID, Path, ResolvedAmount).

execute_decr(MyID, Path, Amount) -->
    resolve_path_to_attr(MyID, Path, TargetID/Key),
    ( ctx_attr_val(TargetID/Key, CurrentValue) ->
        {NewValue #= CurrentValue - Amount}
    ;
        {NewValue #= 0 - Amount}
    ),
    ctx_set_attr_val(TargetID/Key, NewValue).
