% wait_key_up action implementation

:- module(execute_action_wait_key_up, []).

:- use_module('../types/accessors').
:- use_module('../types/adv_accessors').
:- use_module('../input_helpers', [key_up/2]).

:- multifile(execute_action:execute_action_impl/5).
:- discontiguous(execute_action:execute_action_impl/5).

% wait_key_up(+KeyCode)
% Mode: wait_key_up(+KeyCode)
% Description: Waits until specified key is released
%   (detects 'up' event for the key)
% Yields: false (checks each frame)

execute_action:execute_action_impl(
    ctx_old(Ctx),
    ctx_new(Ctx),  % Context unchanged
    action(wait_key_up(KeyCode)),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    obj_acns(ObjIn, [_|Rest]),
    
    % Check if key released THIS frame
    ( key_up(Ctx, KeyCode) ->
        % Key released: action complete
        obj_acns_obj(ObjIn, Rest, ObjOut)
    ;
        % Key still pressed: keep waiting
        obj_acns_obj(
          ObjIn,
          [wait_key_up(KeyCode)|Rest],
          ObjOut
        )
    ).

