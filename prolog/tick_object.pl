
tick_object(
    actions_old([]),
    obj_id(_ID),
    result(completed, actions_new([]))
) --> [].

tick_object(
    actions_old([Act|Rest]),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    execute_action(
        actions_old([Act|Rest]),
        obj_id(ID),
        result(ActStatus, actions_new(ActionsTemp))
    ),
    ( {ActStatus = despawned} ->
        {Status = despawned, ActionsOut = []}
    ; {ActStatus = yielded} ->
        {Status = yielded, ActionsOut = ActionsTemp}
    ; % ActStatus = completed
        tick_object(
            actions_old(ActionsTemp),
            obj_id(ID),
            result(Status, actions_new(ActionsOut))
        )
    ).

