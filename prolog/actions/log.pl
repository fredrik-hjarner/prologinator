% log action implementation

:- module(execute_action_log, []).

:- use_module(library(format)).
:- use_module('../types/accessors').
:- use_module('../types/adv_accessors').

:- multifile(execute_action:execute_action_impl/5).
:- discontiguous(execute_action:execute_action_impl/5).

% Confirmed that it works! Don't worry!
execute_action:execute_action_impl(
    ctx_old(Ctx),
    ctx_new(Ctx), % Context unchanged
    action(log(Msg)),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    obj_acns(ObjIn, [_|Rest]),
    format("~s~n", [Msg]),
    obj_acns_obj(ObjIn, Rest, ObjOut).

