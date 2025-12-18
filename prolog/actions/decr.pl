% decr action implementation

execute_action_impl(
    action(decr(Key, Amount)),
    actions_old([_|Rest]),
    obj_id(ID),
    result(completed, actions_new(Rest))
) -->
    execute_decr(ID, Key, Amount).

execute_action_impl(
    action(decr(TargetID, Key, Amount)),
    actions_old([_|Rest]),
    obj_id(_ID),
    result(completed, actions_new(Rest))
) -->
    execute_decr(TargetID, Key, Amount).

% ==========================================================
% execute_decr/6
% ==========================================================
execute_decr(TargetID, Key, Amount) -->
    ( ctx_attr_val(TargetID/Key, CurrentValue) ->
        {NewValue #= CurrentValue - Amount}
    ;
        {NewValue #= 0 - Amount}
    ),
    ctx_set_attr_val(TargetID/Key, NewValue).


