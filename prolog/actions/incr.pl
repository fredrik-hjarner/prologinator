execute_action_impl(
    action(incr(Path, Amount)),
    actions_old([_|Rest]),
    obj_id(MyID),
    result(completed, actions_new(Rest))
) -->
    execute_incr(MyID, Path, Amount).

execute_incr(MyID, Path, Amount) -->
    resolve_path_to_attr(MyID, Path, TargetID/Key),
    ( ctx_attr_val(TargetID/Key, CurrentValue) ->
        {NewValue #= CurrentValue + Amount}
    ;
        {NewValue = Amount}
    ),
    ctx_set_attr_val(TargetID/Key, NewValue).


