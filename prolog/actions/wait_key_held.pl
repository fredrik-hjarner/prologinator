% wait_key_held action implementation


% wait_key_held(+KeyCode)
% Mode: wait_key_held(+KeyCode)
% Description: Yields every frame while key is held
%   Use in loop: loop([wait_key_held(39), move(...)])
% Yields: true when key is held, false otherwise

execute_action_impl(
    action(wait_key_held(KeyCode)),
    obj_old(ObjIn),
    result(Status, ObjOut),
    CtxOld,
    CtxNew
) :-
    execute_wait_key_held(
        KeyCode, ObjIn, Status, ObjOut, CtxOld, CtxNew
    ).

% ==========================================================
% execute_wait_key_held/6
% ==========================================================
execute_wait_key_held(
    KeyCode, ObjIn, Status, ObjOut, Ctx, Ctx
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

