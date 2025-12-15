% decr action implementation


% decr/2 - Decrement attribute on self
execute_action_impl(
    action(decr(Key, Amount)),
    obj_old(ObjIn),
    result(completed, ObjOut)
) -->
    execute_decr(Key, Amount, ObjIn, ObjOut).

% decr/3 - Decrement attribute on specific object
execute_action_impl(
    action(decr(TargetID, Key, Amount)),
    obj_old(ObjIn),
    result(completed, ObjOut)
) -->
    execute_decr(TargetID, Key, Amount, ObjIn, ObjOut).

% ==========================================================
% execute_decr/6 (for decr/2 - self)
% ==========================================================
execute_decr(Key, Amount, ObjIn, ObjOut) -->
    {
        obj_id(ObjIn, MyID),
        obj_acns(ObjIn, [_|Rest]),
        obj_acns_obj(ObjIn, Rest, ObjOut)
    },
    ( ctx_attr_val(MyID/Key, CurrentValue) ->
        {NewValue #= CurrentValue - Amount}
    ;
        {NewValue #= 0 - Amount}
    ),
    ctx_set_attr_val(MyID/Key, NewValue).

% ==========================================================
% execute_decr/7 (for decr/3 - target)
% ==========================================================
execute_decr(
    TargetID,
    Key,
    Amount,
    ObjIn,
    ObjOut
) -->
    {obj_acns(ObjIn, [_|Rest]),
     obj_acns_obj(ObjIn, Rest, ObjOut)},
    ( ctx_attr_val(TargetID/Key, CurrentValue) ->
        {NewValue #= CurrentValue - Amount}
    ;
        {NewValue #= 0 - Amount}
    ),
    ctx_set_attr_val(TargetID/Key, NewValue).

