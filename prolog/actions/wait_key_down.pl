builtin_action(wait_key_down(_)).

% wait_key_down(+KeyCode)
% Mode: wait_key_down(+KeyCode)
% Description: Waits until specified key is pressed
%   (detects 'down' event for the key)
% Yields: false (checks each frame)

execute_action_impl(
    actions_old([wait_key_down(KeyCode)|Rest]),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    resolve_arg(ID, KeyCode, ResolvedKeyCode),
    execute_wait_key_down(
        ResolvedKeyCode,
        Rest,
        Status,
        ActionsOut
    ).

execute_wait_key_down(KeyCode, Rest, Status, ActionsOut) -->
    % Check if key was pressed THIS frame
    ( key_down(KeyCode) ->
        % Key pressed: action complete
        {ActionsOut = Rest, Status = completed}
    ;
        % Key not pressed: keep waiting
        {ActionsOut = [wait_key_down(KeyCode)|Rest],
         Status = yielded}
    ).


