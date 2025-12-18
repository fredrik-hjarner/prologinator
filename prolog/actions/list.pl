% list action implementation

execute_action_impl(
    action(list(ListActions)),
    actions_old([_ListAction|RestActions]),
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

% ==========================================================
% execute_list/6
% ==========================================================
execute_list(
    ListActions,
    RestActions,
    ID,
    Status,
    ActionsOut
) -->
    % Execute the inner list until it yields, completes,
    % or despawns
    tick_object(
        actions_old(ListActions),
        obj_id(ID),
        result(ListStatus, actions_new(RemainingInner))
    ),
    % Handle the result
    ( {ListStatus = despawned} ->
        {Status = despawned, ActionsOut = []}
    ; {ListStatus = yielded} ->
        % The inner list paused. Wrap remaining actions back
        % in list() and keep them at the head of the queue.
        {ActionsOut = [list(RemainingInner)|RestActions],
         Status = yielded}
    ; % ListStatus = completed
        % The inner list finished. Continue with the rest of
        % the original queue.
        {ActionsOut = RestActions,
         Status = completed}
    ).


