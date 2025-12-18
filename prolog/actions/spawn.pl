% spawn action implementation

execute_action_impl(
    action(spawn(Type, X, Y, Actions)),
    actions_old([_|Rest]),
    obj_id(_ID),
    result(completed, actions_new(Rest))
) -->
    execute_spawn(Type, X, Y, Actions).

% ==========================================================
% execute_spawn/8
% ==========================================================
execute_spawn(Type, X, Y, Actions) -->
    % 1. Generate ID
    ctx_nextid(NewID),
    {NextID #= NewID + 1},
    ctx_set_nextid(NextID),
    % 2. Create Object (no attributes - stored separately)
    {NewObj = object(
        id(NewID),
        actions(Actions)
    )},
    % 3. Initialize attributes in store
    ctx_set_attr_val(NewID/type, Type),
    ctx_set_attr_val(NewID/x, X),
    ctx_set_attr_val(NewID/y, Y),
    % 4. Append to Context
    ctx_objs(CurrentSpawns),
    {append(CurrentSpawns, [NewObj], NewSpawns)},
    ctx_set_objs(NewSpawns).


