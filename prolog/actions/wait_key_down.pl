% wait_key_down action implementation


% wait_key_down(+KeyCode)
% Mode: wait_key_down(+KeyCode)
% Description: Waits until specified key is pressed
%   (detects 'down' event for the key)
% Yields: false (checks each frame)

execute_action_impl(
    action(wait_key_down(KeyCode)),
    obj_old(ObjIn),
    result(Status, ObjOut),
    Ctx,
    Ctx  % Context unchanged
) :-
    execute_wait_key_down(
        Ctx, KeyCode, ObjIn, Status, ObjOut
    ).

% ==========================================================
% execute_wait_key_down/5
% ==========================================================
execute_wait_key_down(
    Ctx, KeyCode, ObjIn, Status, ObjOut
) :-
    obj_acns(ObjIn, [_|Rest]),
    
    % Check if key was pressed THIS frame
    ( key_down(Ctx, KeyCode) ->
        % Key pressed: action complete
        obj_acns_obj(ObjIn, Rest, ObjOut),
        Status = completed
    ;
        % Key not pressed: keep waiting
        obj_acns_obj(
          ObjIn,
          [wait_key_down(KeyCode)|Rest],
          ObjOut
        ),
        Status = yielded
    ).

