builtin_action(wait_key_up(_)).


execute_action_impl(
    actions_old([wait_key_up(KeyCode)|Rest]),
    obj_id(_ID),
    result(Status, actions_new(ActionsOut))
) -->
    execute_wait_key_up(KeyCode, Rest, Status, ActionsOut).

execute_wait_key_up(KeyCode, Rest, Status, ActionsOut) -->
    ( key_up(KeyCode) ->
        {ActionsOut = Rest, Status = completed}
    ;
        {ActionsOut = [wait_key_up(KeyCode)|Rest],
         Status = yielded}
    ).


