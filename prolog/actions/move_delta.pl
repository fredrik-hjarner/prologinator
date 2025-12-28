builtin_action(move_delta(_, _, _)).

% move_delta(+Frames, +DX, +DY)
% Mode: move_delta(+Frames, +DX, +DY)
% Description: Relative movement: move by (DX, DY)
%   each frame for Frames frames
% Yields: true when Frames > 0

execute_action_impl(
    actions_old([move_delta(Frames, DX, DY)|Rest]),
    obj_id(ID),
    result(Status, actions_new(NewActions))
) -->
    resolve_arg(ID, Frames, ResolvedFrames),
    resolve_arg(ID, DX, ResolvedDX),
    resolve_arg(ID, DY, ResolvedDY),
    execute_move_delta(
        ResolvedFrames,
        ResolvedDX,
        ResolvedDY,
        ID,
        Rest,
        Status,
        NewActions
    ).

% 0 frames: teleport instantly by delta
execute_move_delta(
    0,
    DX,
    DY,
    ID,
    Rest,
    completed,
    Rest
) -->
    % Get current position from attribute store
    ( ctx_attr_val(ID/x, CurrX),
      ctx_attr_val(ID/y, CurrY) ->
        []
    ;
        % Default to 0,0 if not set
        {CurrX = 0, CurrY = 0}
    ),
    % Apply delta instantly
    {NewX #= CurrX + DX,
     NewY #= CurrY + DY},
    % Label for grounding
    {( ground(CurrX), ground(CurrY), ground(DX), ground(DY)
    ->
        labeling([], [NewX, NewY])
    ;
        true
    )},
    % Update position in attribute store
    ctx_set_attr_val(ID/x, NewX),
    ctx_set_attr_val(ID/y, NewY).

% 1+ frames: apply delta and continue or finish
execute_move_delta(
    Frames,
    DX,
    DY,
    ID,
    Rest,
    Status,
    NewActions
) -->
    % Get current position from attribute store
    ( ctx_attr_val(ID/x, CurrX),
      ctx_attr_val(ID/y, CurrY) ->
        []
    ;
        % Default to 0,0 if not set
        {CurrX = 0, CurrY = 0}
    ),
    % Apply delta
    {NewX #= CurrX + DX,
     NewY #= CurrY + DY},
    % Label for grounding
    {( ground(CurrX), ground(CurrY), ground(DX),
      ground(DY) ->
        labeling([], [NewX, NewY])
    ;
        true
    )},
    % Update position in attribute store
    ctx_set_attr_val(ID/x, NewX),
    ctx_set_attr_val(ID/y, NewY),
    % Continue or arrive
    {( Frames #> 1 ->
        Frames1 #= Frames - 1,
        NewActions = [move_delta(Frames1, DX, DY)|Rest],
        Status = yielded
    ;
        NewActions = Rest,
        % We arrived, but this movement took 1 frame of
        % time.
        % We must yield execution to the next frame.
        Status = yielded
    )}.
