% wait_key_held action implementation


% wait_key_held(+KeyCode)
% Mode: wait_key_held(+KeyCode)
% Description: Yields every frame while key is held
%   Use in loop: loop([wait_key_held(39), move(...)])
% Yields: true when key is held, false otherwise

execute_action_impl(
    ctx_old(Ctx),
    ctx_new(Ctx),  % Context unchanged
    action(wait_key_held(KeyCode)),
    obj_old(ObjIn),
    result(Status, ObjOut)
) :-
    execute_wait_key_held(
        Ctx, KeyCode, ObjIn, Status, ObjOut
    ).

% ==========================================================
% execute_wait_key_held/5
% ==========================================================
execute_wait_key_held(
    Ctx, KeyCode, ObjIn, Status, ObjOut
) :-
    obj_acns(ObjIn, [_|Rest]),
    
    ( key_held(Ctx, KeyCode) ->
        % Key held: yield (action complete)
        obj_acns_obj(ObjIn, Rest, ObjOut),
        Status = completed
    ;
        % Key not held: keep waiting
        obj_acns_obj(
          ObjIn,
          [wait_key_held(KeyCode)|Rest],
          ObjOut
        ),
        Status = yielded
    ).

