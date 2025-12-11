% define_action action implementation

:- module(execute_action_define_action, []).

:- use_module('../types/accessors').
:- use_module('../types/adv_accessors').

:- multifile(execute_action:execute_action_impl/5).
:- discontiguous(execute_action:execute_action_impl/5).

% Defines a custom action macro at runtime. Stores the
% Signature->Body mapping in user_action/2. When Signature
% is called later, execute_action/5 will expand it with
% parameter substitution and execute the Body.
execute_action:execute_action_impl(
    ctx_old(Ctx),
    ctx_new(Ctx),  % Context unchanged
    action(define_action(Signature, Body)),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    execute_define_action(
        Ctx,
        Signature,
        Body,
        ObjIn,
        ObjOut
    ).

% ==========================================================
% execute_define_action/5
% ==========================================================
execute_define_action(
    _Ctx,
    Signature,
    Body,
    ObjIn,
    ObjOut
) :-
    % Store the macro definition
    assertz(execute_action:user_action(Signature, Body)),
    % Remove this action from queue
    obj_acns(ObjIn, [_|Rest]),
    obj_acns_obj(ObjIn, Rest, ObjOut).

