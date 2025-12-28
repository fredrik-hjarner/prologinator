builtin_action(wait_until(_)).


execute_action_impl(
    actions_old([wait_until(Condition)|Rest]),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    execute_wait_until(
        ID, Condition, Rest, Status, ActionsOut
    ).

execute_wait_until(
    ID, Condition, Rest, Status, ActionsOut
) -->
    ( check_condition(ID, Condition) ->
        {ActionsOut = Rest, Status = completed}
    ;
        {ActionsOut = [wait_until(Condition)|Rest],
         Status = yielded}
    ).
