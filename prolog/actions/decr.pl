builtin_action(decr(_, _)).

execute_action_impl(
    actions_old([decr(Path, Amount)|Rest]),
    obj_id(MyID),
    result(completed, actions_new(Rest))
) -->
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
