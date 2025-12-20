% ==========================================================
% Execution Model: tick_object
% ==========================================================

% tick_object threads the context via DCG.
% Returns result(Status, actions_new(ActionsOut)) where
% Status is completed, yielded, or despawned.
% TODO: Change name because it kinda does not tick an object
%       any more but rather executes streams until a "stop"
%       state.
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
    % Call execute_action with actions passed separately
    execute_action(
        action(Act),
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

