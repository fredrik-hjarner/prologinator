builtin_action(wait_key_up(_)).

% wait_key_up(+KeyCode)
% Mode: wait_key_up(+KeyCode)
% Description: Waits until specified key is released
%   (detects 'up' event for the key)
% Yields: false (checks each frame)

execute_action_impl(
    actions_old([wait_key_up(KeyCode)|Rest]),
    obj(Obj),
    result(Status, actions_new(ActionsOut))
) -->
    resolve_arg(Obj, KeyCode, ResolvedKeyCode),
    execute_wait_key_up(
        ResolvedKeyCode, Rest, Status, ActionsOut
    ).

execute_wait_key_up(KeyCode, Rest, Status, ActionsOut) -->
    % Check if key released THIS frame
    ( key_up(KeyCode) ->
        % Key released: action complete
        {ActionsOut = Rest, Status = completed}
    ;
        % Key still pressed: keep waiting
        {ActionsOut = [wait_key_up(KeyCode)|Rest],
         Status = yielded}
    ).


