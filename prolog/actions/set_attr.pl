% set_attr action implementation

:- module(execute_action_set_attr, []).

:- use_module('../types/accessors').
:- use_module('../types/adv_accessors').

:- multifile(execute_action:execute_action_impl/5).
:- discontiguous(execute_action:execute_action_impl/5).

% set_attr/2 - Set attribute on self
execute_action:execute_action_impl(
    ctx_old(Ctx),
    ctx_new(CtxOut),
    action(set_attr(Key, Value)),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    obj_id(ObjIn, MyID),
    obj_acns(ObjIn, [_|Rest]),
    obj_acns_obj(ObjIn, Rest, ObjOut),
    ctx_attr_val_ctx(Ctx, MyID/Key, Value, CtxOut).

% set_attr/3 - Set attribute on specific object
execute_action:execute_action_impl(
    ctx_old(Ctx),
    ctx_new(CtxOut),
    action(set_attr(TargetID, Key, Value)),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    obj_acns(ObjIn, [_|Rest]),
    obj_acns_obj(ObjIn, Rest, ObjOut),
    ctx_attr_val_ctx(Ctx, TargetID/Key, Value, CtxOut).

