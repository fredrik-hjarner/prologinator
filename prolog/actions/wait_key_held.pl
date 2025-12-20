% wait_key_held(+KeyCode)
% Mode: wait_key_held(+KeyCode)
% Description: Yields every frame while key is held
%   Use in loop: loop([wait_key_held(39), move(...)])
% Yields: true when key is held, false otherwise

execute_action_impl(
    actions_old([wait_key_held(KeyCode)|Rest]),
    obj_id(_ID),
    result(Status, actions_new(ActionsOut))
) -->
    execute_wait_key_held(
        KeyCode,
        Rest,
        Status,
        ActionsOut
    ).

execute_wait_key_held(KeyCode, Rest, Status, ActionsOut) -->
    ( key_held(KeyCode) ->
        % {format("Key held: ~w~n", [KeyCode])},
        % Key held: yield (action complete)
        {ActionsOut = Rest, Status = completed}
    ;
        % {format("Key not held: ~w~n", [KeyCode])},
        % Key not held: keep waiting
        {ActionsOut = [wait_key_held(KeyCode)|Rest],
         Status = yielded}
    ).


