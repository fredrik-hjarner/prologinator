% incr action implementation

:- module(execute_action_incr, []).

:- use_module(library(clpz)).
:- use_module('../types/accessors').
:- use_module('../types/adv_accessors').

:- multifile(execute_action:execute_action_impl/5).
:- discontiguous(execute_action:execute_action_impl/5).

% incr/2 - Increment attribute on self
execute_action:execute_action_impl(
    ctx_old(Ctx),
    ctx_new(CtxOut),
    action(incr(Key, Amount)),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    execute_incr(Ctx, CtxOut, Key, Amount, ObjIn, ObjOut).

% incr/3 - Increment attribute on specific object
execute_action:execute_action_impl(
    ctx_old(Ctx),
    ctx_new(CtxOut),
    action(incr(TargetID, Key, Amount)),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    execute_incr(
        Ctx,
        CtxOut,
        TargetID,
        Key,
        Amount,
        ObjIn,
        ObjOut
    ).

% ==========================================================
% execute_incr/6 (for incr/2 - self)
% ==========================================================
execute_incr(Ctx, CtxOut, Key, Amount, ObjIn, ObjOut) :-
    obj_id(ObjIn, MyID),
    obj_acns(ObjIn, [_|Rest]),
    obj_acns_obj(ObjIn, Rest, ObjOut),
    ( ctx_attr_val(Ctx, MyID/Key, CurrentValue) ->
        NewValue #= CurrentValue + Amount
    ;
        NewValue = Amount
    ),
    ctx_attr_val_ctx(Ctx, MyID/Key, NewValue, CtxOut).

% ==========================================================
% execute_incr/7 (for incr/3 - target)
% ==========================================================
execute_incr(
    Ctx,
    CtxOut,
    TargetID,
    Key,
    Amount,
    ObjIn,
    ObjOut
) :-
    obj_acns(ObjIn, [_|Rest]),
    obj_acns_obj(ObjIn, Rest, ObjOut),
    ( ctx_attr_val(Ctx, TargetID/Key,
                   CurrentValue) ->
        NewValue #= CurrentValue + Amount
    ;
        NewValue = Amount
    ),
    ctx_attr_val_ctx(Ctx, TargetID/Key, NewValue,
                     CtxOut).

