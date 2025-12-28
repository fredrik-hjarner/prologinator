builtin_action(move_to(_, _, _)).

execute_action_impl(
    actions_old([move_to(TargetX, TargetY, Frames)|Rest]),
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

execute_move_to(
    TargetX,
    TargetY,
    Frames,
    ID,
    Rest,
    Status,
    NewActions
) -->
    ctx_attr_val(ID/x, CurrX),
    ctx_attr_val(ID/y, CurrY),
    {DX #= (TargetX - CurrX) // Frames,
     DY #= (TargetY - CurrY) // Frames,
     NewX #= CurrX + DX,
     NewY #= CurrY + DY},
    {(ground(TargetX), ground(TargetY), ground(CurrX),
      ground(CurrY), ground(Frames) ->
        labeling([], [NewX, NewY])
    ;
        true
    )},
    ctx_set_attr_val(ID/x, NewX),
    ctx_set_attr_val(ID/y, NewY),
    {( Frames #> 1 ->
        Frames1 #= Frames - 1,
        NewActions = [
            move_to(TargetX, TargetY, Frames1)|Rest],
        Status = yielded
    ;
        NewActions = Rest,  % Arrived
        Status = yielded
    )}.
