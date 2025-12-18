% incr action implementation

execute_action_impl(
    action(incr(Key, Amount)),
    actions_old([_|Rest]),
    obj_id(ID),
    result(completed, actions_new(Rest))
) -->
    execute_incr(ID, Key, Amount).

execute_action_impl(
    action(incr(TargetID, Key, Amount)),
    actions_old([_|Rest]),
    obj_id(_ID),
    result(completed, actions_new(Rest))
) -->
    execute_incr(TargetID, Key, Amount).

% ==========================================================
% execute_incr/6
% ==========================================================
execute_incr(TargetID, Key, Amount) -->
    ( ctx_attr_val(TargetID/Key, CurrentValue) ->
        {NewValue #= CurrentValue + Amount}
    ;
        {NewValue = Amount}
    ),
    ctx_set_attr_val(TargetID/Key, NewValue).


