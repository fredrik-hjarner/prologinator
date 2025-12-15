% wait_key_up action implementation


% wait_key_up(+KeyCode)
% Mode: wait_key_up(+KeyCode)
% Description: Waits until specified key is released
%   (detects 'up' event for the key)
% Yields: false (checks each frame)

execute_action_impl(
    action(wait_key_up(KeyCode)),
    obj_old(ObjIn),
    result(Status, ObjOut),
    CtxOld,
    CtxNew
) :-
    execute_wait_key_up(
        KeyCode, ObjIn, Status, ObjOut, CtxOld, CtxNew
    ).

% ==========================================================
% execute_wait_key_up/6
% ==========================================================
execute_wait_key_up(
    KeyCode, ObjIn, Status, ObjOut, Ctx, Ctx
) :-
    obj_acns(ObjIn, [_|Rest]),
    
    % Check if key released THIS frame
    ( key_up(Ctx, KeyCode) ->
        % Key released: action complete
        obj_acns_obj(ObjIn, Rest, ObjOut),
        Status = completed
    ;
        % Key still pressed: keep waiting
        obj_acns_obj(
          ObjIn,
          [wait_key_up(KeyCode)|Rest],
          ObjOut
        ),
        Status = yielded
    ).

