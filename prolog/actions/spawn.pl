% spawn action implementation


% IMMEDIATELY spawns the object into the context
execute_action_impl(
    action(spawn(Type, X, Y, Actions)),
    obj_old(ObjIn),
    result(completed, ObjOut)
) -->
    execute_spawn(Type, X, Y, Actions, ObjIn, ObjOut).

% ==========================================================
% execute_spawn/8
% ==========================================================
execute_spawn(
    Type,
    X,
    Y,
    Actions,
    ObjIn,
    ObjOut
) -->
    {obj_acns(ObjIn, [_|Rest])},
    {obj_acns_obj(ObjIn, Rest, ObjOut)},
    % 1. Generate ID
    ctx_nextid(ID),
    {NextID #= ID + 1},
    ctx_set_nextid(NextID),
    % 2. Create Object (no attributes - stored separately)
    {NewObj = object(
        id(ID), type(Type),
        actions(Actions)
    )},
    % 3. Initialize attributes in store
    ctx_set_attr_val(ID/x, X),
    ctx_set_attr_val(ID/y, Y),
    % 4. Append to Context
    % Since ID is increasing, appending to end keeps the
    % list sorted.
    % Note: append/3 is O(N), but for typical game sizes
    % (<1000 objects) this is acceptable. For larger scales,
    % consider difference lists or reverse-order storage.
    ctx_objs(CurrentSpawns),
    {append(CurrentSpawns, [NewObj], NewSpawns)},
    ctx_set_objs(NewSpawns).

