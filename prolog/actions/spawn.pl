% spawn action implementation


% IMMEDIATELY spawns the object into the context
execute_action_impl(
    ctx_old(CtxIn),
    ctx_new(CtxOut),
    action(spawn(Type, X, Y, Actions)),
    obj_old(ObjIn),
    result(completed, ObjOut)
) :-
    execute_spawn(
        CtxIn,
        CtxOut,
        Type,
        X,
        Y,
        Actions,
        ObjIn,
        ObjOut
    ).

% ==========================================================
% execute_spawn/7
% ==========================================================
execute_spawn(
    CtxIn,
    CtxOut,
    Type,
    X,
    Y,
    Actions,
    ObjIn,
    ObjOut
) :-
    obj_acns(ObjIn, [_|Rest]),
    obj_acns_obj(ObjIn, Rest, ObjOut),
    
    % 1. Generate ID
    ctx_nextid(CtxIn, ID),
    NextID #= ID + 1,
    ctx_nextid_ctx(CtxIn, NextID, CtxTemp1),
    
    % 2. Create Object (no attributes - stored separately)
    NewObj = object(
        id(ID), type(Type),
        actions(Actions), collisions([])
    ),
    
    % 3. Initialize attributes in store
    ctx_attr_val_ctx(CtxTemp1, ID/x, X, CtxTemp2a),
    ctx_attr_val_ctx(CtxTemp2a, ID/y, Y, CtxTemp2),
    
    % 4. Append to Context
    % Since ID is increasing, appending to end keeps the
    % list sorted.
    % Note: append/3 is O(N), but for typical game sizes
    % (<1000 objects) this is acceptable. For larger scales,
    % consider difference lists or reverse-order storage.
    ctx_objs(CtxTemp2, CurrentSpawns),
    append(CurrentSpawns, [NewObj], NewSpawns),
    ctx_objs_ctx(CtxTemp2, NewSpawns, CtxOut).

