% noop action implementation

:- module(execute_action_noop, []).

:- use_module('../types/accessors').
:- use_module('../types/adv_accessors').

:- multifile(execute_action:execute_action_impl/5).
:- discontiguous(execute_action:execute_action_impl/5).

execute_action:execute_action_impl(
    ctx_old(Ctx),
    ctx_new(Ctx),
    action(noop),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    obj_acns(ObjIn, [_|Rest]),
    obj_acns_obj(ObjIn, Rest, ObjOut).

