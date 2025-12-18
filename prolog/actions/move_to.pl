% move_to action implementation

execute_action_impl(
    action(move_to(TargetX, TargetY, Frames)),
    actions_old([_|Rest]),
    obj_id(ID),
    result(Status, actions_new(NewActions))
) -->
    execute_move_to(
        TargetX,
        TargetY,
        Frames,
        ID,
        Rest,
        Status,
        NewActions
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
    Rest
) -->
    ctx_set_attr_val(ID/x, TargetX),
    ctx_set_attr_val(ID/y, TargetY).

% 1+ frames: compute step and continue or finish
execute_move_to(
    TargetX,
    TargetY,
    Frames,
    ID,
    Rest,
    Status,
    NewActions
) -->
    {Frames #> 0},
    % Get current position from attribute store
    % Fails if object doesn't have x/y attributes
    ctx_attr_val(ID/x, CurrX),
    ctx_attr_val(ID/y, CurrY),
    % Compute step using integer division
    {DX #= (TargetX - CurrX) // Frames,
     DY #= (TargetY - CurrY) // Frames,
     NewX #= CurrX + DX,
     NewY #= CurrY + DY},
    % Label at the boundary where we need ground values for
    % game objects
    {(ground(TargetX), ground(TargetY), ground(CurrX),
      ground(CurrY), ground(Frames) ->
        labeling([], [NewX, NewY])
    ;
        true
    )},
    % Update position in attribute store
    ctx_set_attr_val(ID/x, NewX),
    ctx_set_attr_val(ID/y, NewY),
    {( Frames #> 1 ->
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
    )}.


