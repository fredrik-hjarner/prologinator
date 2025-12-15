% set_attr action implementation


% set_attr/2 - Set attribute on self
execute_action_impl(
    action(set_attr(Key, Value)),
    obj_old(ObjIn),
    result(completed, ObjOut)
) -->
    execute_set_attr(Key, Value, ObjIn, ObjOut).

% set_attr/3 - Set attribute on specific object
execute_action_impl(
    action(set_attr(TargetID, Key, Value)),
    obj_old(ObjIn),
    result(completed, ObjOut)
) -->
    execute_set_attr(
        TargetID, Key, Value, ObjIn, ObjOut
    ).

% ==========================================================
% execute_set_attr/6 (for set_attr/2 - self)
% ==========================================================
execute_set_attr(Key, Value, ObjIn, ObjOut) -->
    {
        obj_id(ObjIn, MyID),
        obj_acns(ObjIn, [_|Rest]),
        obj_acns_obj(ObjIn, Rest, ObjOut)
    },
    ctx_set_attr_val(MyID/Key, Value).

% ==========================================================
% execute_set_attr/7 (for set_attr/3 - target)
% ==========================================================
execute_set_attr(TargetID, Key, Value, ObjIn, ObjOut) -->
    {
        obj_acns(ObjIn, [_|Rest]),
        obj_acns_obj(ObjIn, Rest, ObjOut)
    },
    ctx_set_attr_val(TargetID/Key, Value).

