% list action implementation

:- module(execute_action_list, []).

:- use_module(library(lists), [
    append/3
]).
:- use_module('../types/accessors').
:- use_module('../types/adv_accessors').

:- multifile(execute_action:execute_action_impl/5).
:- discontiguous(execute_action:execute_action_impl/5).

execute_action:execute_action_impl(
    ctx_old(Ctx),
    ctx_new(Ctx),
    action(list(Actions)),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    obj_acns(ObjIn, [_|Rest]),
    append(Actions, Rest, NewActions),
    obj_acns_obj(ObjIn, NewActions, ObjOut).

