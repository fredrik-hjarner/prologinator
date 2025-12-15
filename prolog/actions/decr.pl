% decr action implementation


% decr/2 - Decrement attribute on self
execute_action_impl(
    action(decr(Key, Amount)),
    obj_old(ObjIn),
    result(completed, ObjOut),
    Ctx,
    CtxOut
) :-
    execute_decr(Key, Amount, ObjIn, ObjOut, Ctx, CtxOut).

% decr/3 - Decrement attribute on specific object
execute_action_impl(
    action(decr(TargetID, Key, Amount)),
    obj_old(ObjIn),
    result(completed, ObjOut),
    Ctx,
    CtxOut
) :-
    execute_decr(
        TargetID,
        Key,
        Amount,
        ObjIn,
        ObjOut,
        Ctx,
        CtxOut
    ).

% ==========================================================
% execute_decr/6 (for decr/2 - self)
% ==========================================================
execute_decr(Key, Amount, ObjIn, ObjOut, Ctx, CtxOut) :-
    obj_id(ObjIn, MyID),
    obj_acns(ObjIn, [_|Rest]),
    obj_acns_obj(ObjIn, Rest, ObjOut),
    ( ctx_attr_val(Ctx, MyID/Key, CurrentValue) ->
        NewValue #= CurrentValue - Amount
    ;
        NewValue #= 0 - Amount
    ),
    ctx_attr_val_ctx(Ctx, MyID/Key, NewValue, CtxOut).

% ==========================================================
% execute_decr/7 (for decr/3 - target)
% ==========================================================
execute_decr(
    TargetID,
    Key,
    Amount,
    ObjIn,
    ObjOut,
    Ctx,
    CtxOut
) :-
    obj_acns(ObjIn, [_|Rest]),
    obj_acns_obj(ObjIn, Rest, ObjOut),
    ( ctx_attr_val(Ctx, TargetID/Key,
                   CurrentValue) ->
        NewValue #= CurrentValue - Amount
    ;
        NewValue #= 0 - Amount
    ),
    ctx_attr_val_ctx(Ctx, TargetID/Key, NewValue,
                     CtxOut).

