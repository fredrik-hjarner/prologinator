% spawn action implementation


% IMMEDIATELY spawns the object into the context
execute_action_impl(
    action(spawn(Type, X, Y, Actions)),
    obj_old(ObjIn),
    result(completed, ObjOut),
    CtxIn,
    CtxOut
) :-
    execute_spawn(
        Type,
        X,
        Y,
        Actions,
        ObjIn,
        ObjOut,
        CtxIn,
        CtxOut
    ).

% ==========================================================
% execute_spawn/8
% ==========================================================
execute_spawn(
    Type,
    X,
    Y,
    Actions,
    ObjIn,
    ObjOut,
    CtxIn,
    CtxOut
) :-
    obj_acns(ObjIn, [_|Rest]),
    obj_acns_obj(ObjIn, Rest, ObjOut),
    
    % 1. Generate ID
    ctx_nextid(ID, CtxIn),
    NextID #= ID + 1,
    ctx_set_nextid(NextID, CtxIn, CtxTemp1),
    
    % 2. Create Object (no attributes - stored separately)
    NewObj = object(
        id(ID), type(Type),
        actions(Actions), collisions([])
    ),
    
    % 3. Initialize attributes in store
    ctx_set_attr_val(ID/x, X, CtxTemp1, CtxTemp2a),
    ctx_set_attr_val(ID/y, Y, CtxTemp2a, CtxTemp2),
    
    % 4. Append to Context
    % Since ID is increasing, appending to end keeps the
    % list sorted.
    % Note: append/3 is O(N), but for typical game sizes
    % (<1000 objects) this is acceptable. For larger scales,
    % consider difference lists or reverse-order storage.
    ctx_objs(CurrentSpawns, CtxTemp2),
    append(CurrentSpawns, [NewObj], NewSpawns),
    ctx_set_objs(NewSpawns, CtxTemp2, CtxOut).

