% wait_key_held action implementation

:- module(execute_action_wait_key_held, []).

:- use_module('../types/accessors').
:- use_module('../types/adv_accessors').
:- use_module('../input_helpers', [key_held/2]).

:- multifile(execute_action:execute_action_impl/5).
:- discontiguous(execute_action:execute_action_impl/5).

% wait_key_held(+KeyCode)
% Mode: wait_key_held(+KeyCode)
% Description: Yields every frame while key is held
%   Use in loop: loop([wait_key_held(39), move(...)])
% Yields: true when key is held, false otherwise

execute_action:execute_action_impl(
    ctx_old(Ctx),
    ctx_new(Ctx),  % Context unchanged
    action(wait_key_held(KeyCode)),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    obj_acns(ObjIn, [_|Rest]),
    
    ( key_held(Ctx, KeyCode) ->
        % Key held: yield (action complete)
        obj_acns_obj(ObjIn, Rest, ObjOut)
    ;
        % Key not held: keep waiting
        obj_acns_obj(
          ObjIn,
          [wait_key_held(KeyCode)|Rest],
          ObjOut
        )
    ).

