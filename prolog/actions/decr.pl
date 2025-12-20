execute_action_impl(
    action(decr(Path, Amount)),
    actions_old([_|Rest]),
    obj_id(MyID),
    result(completed, actions_new(Rest))
) -->
    execute_decr(MyID, Path, Amount).

execute_decr(MyID, Path, Amount) -->
    resolve_path_to_attr(MyID, Path, TargetID/Key),
    ( ctx_attr_val(TargetID/Key, CurrentValue) ->
        {NewValue #= CurrentValue - Amount}
    ;
        {NewValue #= 0 - Amount}
    ),
    ctx_set_attr_val(TargetID/Key, NewValue).


