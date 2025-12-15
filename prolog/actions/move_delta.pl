% move_delta action implementation


% move_delta(+Frames, +DX, +DY)
% Mode: move_delta(+Frames, +DX, +DY)
% Description: Relative movement: move by (DX, DY)
%   each frame for Frames frames
% Yields: true when Frames > 0

execute_action_impl(
    action(move_delta(Frames, DX, DY)),
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
    CtxOut
) :-
    execute_move_delta(
        Frames,
        DX,
        DY,
        ID,
        Rest,
        Status,
        NewActions,
        Ctx,
        CtxOut
    ).

% ==========================================================
% execute_move_delta/9
% ==========================================================
% 0 frames: teleport instantly by delta
execute_move_delta(
    0,
    DX,
    DY,
    ID,
    Rest,
    completed,
    Rest,
    Ctx,
    CtxOut
) :-
    % Get current position from attribute store
    ( ctx_attr_val(ID/x, CurrX, Ctx, Ctx),
      ctx_attr_val(ID/y, CurrY, Ctx, Ctx) ->
        true
    ;
        % Default to 0,0 if not set
        CurrX = 0,
        CurrY = 0
    ),
    % Apply delta instantly
    NewX #= CurrX + DX,
    NewY #= CurrY + DY,
    % Label for grounding
    ( ground(CurrX), ground(CurrY), ground(DX), ground(DY)
    ->
        labeling([], [NewX, NewY])
    ;
        true
    ),
    % Update position in attribute store
    ctx_set_attr_val(ID/x, NewX, Ctx, Ctx1),
    ctx_set_attr_val(ID/y, NewY, Ctx1, CtxOut).

% 1+ frames: apply delta and continue or finish
execute_move_delta(
    Frames,
    DX,
    DY,
    ID,
    Rest,
    Status,
    NewActions,
    Ctx,
    CtxOut
) :-
    Frames #> 0,
    % Get current position from attribute store
    ( ctx_attr_val(ID/x, CurrX, Ctx, Ctx),
      ctx_attr_val(ID/y, CurrY, Ctx, Ctx) ->
        true
    ;
        % Default to 0,0 if not set
        CurrX = 0,
        CurrY = 0
    ),
    % Apply delta
    NewX #= CurrX + DX,
    NewY #= CurrY + DY,
    % Label for grounding
    ( ground(CurrX), ground(CurrY), ground(DX),
      ground(DY) ->
        labeling([], [NewX, NewY])
    ;
        true
    ),
    % Update position in attribute store
    ctx_set_attr_val(ID/x, NewX, Ctx, Ctx1),
    ctx_set_attr_val(ID/y, NewY, Ctx1, CtxOut),
    % Continue or arrive
    ( Frames #> 1 ->
        Frames1 #= Frames - 1,
        NewActions = [move_delta(Frames1, DX, DY)|Rest],
        Status = yielded
    ;
        NewActions = Rest,
        % We arrived, but this movement took 1 frame of
        % time.
        % We must yield execution to the next frame.
        Status = yielded
    ).

