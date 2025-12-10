% wait_key_down action implementation

:- module(execute_action_wait_key_down, []).

:- use_module('../types/accessors').
:- use_module('../types/adv_accessors').
:- use_module('../input_helpers', [key_down/2]).

:- multifile(execute_action:execute_action_impl/5).
:- discontiguous(execute_action:execute_action_impl/5).

% wait_key_down(+KeyCode)
% Mode: wait_key_down(+KeyCode)
% Description: Waits until specified key is pressed
%   (detects 'down' event for the key)
% Yields: false (checks each frame)

execute_action:execute_action_impl(
    ctx_old(Ctx),
    ctx_new(Ctx),  % Context unchanged
    action(wait_key_down(KeyCode)),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    obj_acns(ObjIn, [_|Rest]),
    
    % Check if key was pressed THIS frame
    ( key_down(Ctx, KeyCode) ->
        % Key pressed: action complete
        obj_acns_obj(ObjIn, Rest, ObjOut)
    ;
        % Key not pressed: keep waiting
        obj_acns_obj(
          ObjIn,
          [wait_key_down(KeyCode)|Rest],
          ObjOut
        )
    ).

