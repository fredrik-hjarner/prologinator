builtin_action(move_delta(_, _, _)).


execute_action_impl(
    actions_old([move_delta(Frames, DX, DY)|Rest]),
    obj_id(ID),
    result(Status, actions_new(NewActions))
) -->
    execute_move_delta(
        Frames,
        DX,
        DY,
        ID,
        Rest,
        Status,
        NewActions
    ).

execute_move_delta(
    0,
    DX,
    DY,
    ID,
    Rest,
    completed,
    Rest
) -->
    ( ctx_attr_val(ID/x, CurrX),
      ctx_attr_val(ID/y, CurrY) ->
        []
    ;
        {CurrX = 0, CurrY = 0}
    ),
    {NewX #= CurrX + DX,
     NewY #= CurrY + DY},
    {( ground(CurrX), ground(CurrY), ground(DX), ground(DY)
    ->
        labeling([], [NewX, NewY])
    ;
        true
    )},
    ctx_set_attr_val(ID/x, NewX),
    ctx_set_attr_val(ID/y, NewY).

execute_move_delta(
    Frames,
    DX,
    DY,
    ID,
    Rest,
    Status,
    NewActions
) -->
    ( ctx_attr_val(ID/x, CurrX),
      ctx_attr_val(ID/y, CurrY) ->
        []
    ;
        {CurrX = 0, CurrY = 0}
    ),
    {NewX #= CurrX + DX,
     NewY #= CurrY + DY},
    {( ground(CurrX), ground(CurrY), ground(DX),
      ground(DY) ->
        labeling([], [NewX, NewY])
    ;
        true
    )},
    ctx_set_attr_val(ID/x, NewX),
    ctx_set_attr_val(ID/y, NewY),
    {( Frames #> 1 ->
        Frames1 #= Frames - 1,
        NewActions = [move_delta(Frames1, DX, DY)|Rest],
        Status = yielded
    ;
        NewActions = Rest,
        Status = yielded
    )}.
