% move_to action implementation

execute_action_impl(
    action(move_to(TargetX, TargetY, Frames)),
    obj_old(object(
        id(ID),
        type(Type),
        actions([_|Rest]),
        Colls
    )),
    result(Status, object(
        id(ID),
        type(Type),
        actions(NewActions),
        Colls
    )),
    Ctx,
    CtxOut  % Context may change due to attrs
) :-
    execute_move_to(
        TargetX,
        TargetY,
        Frames,
        ID,
        Rest,
        Status,
        NewActions,
        Ctx,
        CtxOut
    ).

% ==========================================================
% execute_move_to/9
% ==========================================================
% 0 frames: teleport instantly to target
execute_move_to(
    TargetX,
    TargetY,
    0,
    ID,
    Rest,
    completed,
    Rest,
    Ctx,
    CtxOut
) :-
    ctx_attr_val_ctx(Ctx, ID/x, TargetX, Ctx1),
    ctx_attr_val_ctx(Ctx1, ID/y, TargetY, CtxOut).

% 1+ frames: compute step and continue or finish
execute_move_to(
    TargetX,
    TargetY,
    Frames,
    ID,
    Rest,
    Status,
    NewActions,
    Ctx,
    CtxOut
) :-
    Frames #> 0,
    % Get current position from attribute store
    % Fails if object doesn't have x/y attributes
    ctx_attr_val(Ctx, ID/x, CurrX),
    ctx_attr_val(Ctx, ID/y, CurrY),
    % Compute step using integer division
    DX #= (TargetX - CurrX) // Frames,
    DY #= (TargetY - CurrY) // Frames,
    NewX #= CurrX + DX,
    NewY #= CurrY + DY,
    % Label at the boundary where we need ground values for
    % game objects
    (ground(TargetX), ground(TargetY), ground(CurrX),
     ground(CurrY), ground(Frames) ->
        labeling([], [NewX, NewY])
    ;
        true
    ),
    % Update position in attribute store
    ctx_attr_val_ctx(Ctx, ID/x, NewX, Ctx1),
    ctx_attr_val_ctx(Ctx1, ID/y, NewY, CtxOut),
    ( Frames #> 1 ->
        Frames1 #= Frames - 1,
        NewActions = [
            move_to(TargetX, TargetY, Frames1)|Rest],
        Status = yielded
    ;
        NewActions = Rest,  % Arrived
        % We arrived, but this movement took 1 frame of
        % time.
        % We must yield execution to the next frame.
        Status = yielded
    ).

