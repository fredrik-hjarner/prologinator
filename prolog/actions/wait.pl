% wait action implementation

:- module(execute_action_wait, []).

:- use_module(library(clpz)).
:- use_module('../types/accessors').
:- use_module('../types/adv_accessors').

:- multifile(execute_action:execute_action_impl/5).
:- discontiguous(execute_action:execute_action_impl/5).

execute_action:execute_action_impl(
    ctx_old(Ctx),
    ctx_new(Ctx), % Context unchanged
    action(wait(N)),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    obj_acns(ObjIn, [_|Rest]),
    wait_continue(N, Rest, NewActions),
    obj_acns_obj(ObjIn, NewActions, ObjOut).

% Helper: Continue or finish wait action based on remaining
% frames
wait_continue(N, Rest, [wait(N1)|Rest]) :-
    N #> 1,
    N1 #= N - 1.
wait_continue(N, Rest, Rest) :- N #=< 1.

