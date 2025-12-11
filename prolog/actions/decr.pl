% decr action implementation

:- module(execute_action_decr, []).

:- use_module(library(clpz)).
:- use_module('../types/accessors').
:- use_module('../types/adv_accessors').

:- multifile(execute_action:execute_action_impl/5).
:- discontiguous(execute_action:execute_action_impl/5).

% decr/2 - Decrement attribute on self
execute_action:execute_action_impl(
    ctx_old(Ctx),
    ctx_new(CtxOut),
    action(decr(Key, Amount)),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    execute_decr(Ctx, CtxOut, Key, Amount, ObjIn, ObjOut).

% decr/3 - Decrement attribute on specific object
execute_action:execute_action_impl(
    ctx_old(Ctx),
    ctx_new(CtxOut),
    action(decr(TargetID, Key, Amount)),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    execute_decr(
        Ctx,
        CtxOut,
        TargetID,
        Key,
        Amount,
        ObjIn,
        ObjOut
    ).

% ==========================================================
% execute_decr/6 (for decr/2 - self)
% ==========================================================
execute_decr(Ctx, CtxOut, Key, Amount, ObjIn, ObjOut) :-
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
        NewValue #= CurrentValue - Amount
    ;
        NewValue #= 0 - Amount
    ),
    ctx_attr_val_ctx(Ctx, TargetID/Key, NewValue,
                     CtxOut).

