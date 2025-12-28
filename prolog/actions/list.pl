builtin_action(list(_)).

execute_action_impl(
    actions_old([list(ListActions)|RestActions]),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    execute_list(
        ListActions,
        RestActions,
        ID,
        Status,
        ActionsOut
    ).

execute_list(
    ListActions,
    RestActions,
    ID,
    Status,
    ActionsOut
) -->
    tick_object(
        actions_old(ListActions),
        obj_id(ID),
        result(ListStatus, actions_new(RemainingInner))
    ),
    ( {ListStatus = despawned} ->
        {Status = despawned, ActionsOut = []}
    ; {ListStatus = yielded} ->
        {ActionsOut = [list(RemainingInner)|RestActions],
         Status = yielded}
    ; % ListStatus = completed
        {ActionsOut = RestActions,
         Status = completed}
    ).


