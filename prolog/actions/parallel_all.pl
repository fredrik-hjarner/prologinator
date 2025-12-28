builtin_action(parallel_all(_)).


execute_action_impl(
    actions_old([parallel_all(Children)|RestActions]),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    tick_children(Children, ID, Result),
    {( Result = despawned ->
        Status = despawned,
        ActionsOut = []
    ; Result = remaining(RemainingChildren) ->
        ( RemainingChildren = [] ->
            Status = completed,
            ActionsOut = RestActions
        ;
            Status = yielded,
            ActionsOut = [parallel_all(
                RemainingChildren)|RestActions]
        )
    )}.



tick_children([], _ID, remaining([])) --> [].

tick_children(
    [FirstTopLayerAcn|TopLayerAcnsRest],
    ID,
    Result
) -->
    {( FirstTopLayerAcn = [_|_] ->
        FirstTopLayerAcnList = FirstTopLayerAcn
    ;
        FirstTopLayerAcnList = [FirstTopLayerAcn]
    )},
    tick_object(
        actions_old(FirstTopLayerAcnList),
        obj_id(ID),
        result(
            ChildStatusAfterTick,
            actions_new(RemainingAcnsAfterTick)
        )
    ),
    ( {ChildStatusAfterTick = despawned} ->
        {Result = despawned}
    ;
        tick_children(
            TopLayerAcnsRest,
            ID,
            RestResult
        ),
        {( RestResult = despawned ->
            Result = despawned
        ; RestResult = remaining(RestRemaining) ->
            ( RemainingAcnsAfterTick = [] ->
                Result = remaining(RestRemaining)
            ;
                append(
                    RemainingAcnsAfterTick,
                    RestRemaining,
                    AllRemaining
                ),
                Result = remaining(AllRemaining)
            )
        )}
    ).


