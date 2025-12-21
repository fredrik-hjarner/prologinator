% repeat(+Times, +Actions)
% Mode: repeat(+Times, +Actions)
% Description: Execute action list N times.
%   Self-managed: threads the state of the repetition across
%   frames.
%   If the body finishes in a tick, it proceeds to the next
%   repetition immediately.

% TODO: I prefer no implementation in execute_action_impl
% but to have execute_action_impl simply pass stuff
% to execute_${actionName} to do the implementation.
execute_action_impl(
    actions_old([repeat(0, _)|Rest]),
    _,
    result(completed, actions_new(Rest))
) --> [].

% Initial call: repeat(Times, Actions)
execute_action_impl(
    actions_old([repeat(Times, Actions)|Rest]),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    % Constraint from original: Times > 0
    {Times #> 0},
    execute_repeat_managed(
        Times,
        Actions,
        Actions,
        Rest,
        ID,
        Status,
        ActionsOut
    ).

% Continuation: repeat(RemainingTimes, Running, Original)
execute_action_impl(
    actions_old([repeat(Times, Running, Original)|Rest]),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    execute_repeat_managed(
        Times,
        Running,
        Original,
        Rest,
        ID,
        Status,
        ActionsOut
    ).

execute_repeat_managed(
    Times, Running, Original, Rest, ID, Status, ActionsOut
) -->
    % Execute the current running actions (threads context)
    tick_object(
        actions_old(Running),
        obj_id(ID),
        result(RunStatus, actions_new(RunRemaining))
    ),
    handle_repeat_result(
        RunStatus,
        RunRemaining,
        Times,
        Original,
        Rest,
        ID,
        Status,
        ActionsOut
    ).

% Case 1: Despawned
handle_repeat_result(
    despawned, _, _, _, _, _, despawned, []
) --> [].

% Case 2: Yielded - Body yielded, update state
handle_repeat_result(
    yielded,
    RunRemaining,
    Times, Original,
    Rest,
    _,
    yielded, [repeat(Times, RunRemaining, Original)|Rest]
) --> [].

% Case 3: Completed - Body finished
handle_repeat_result(
    completed,
    _,
    Times,
    Original,
    Rest,
    ID,
    Status,
    ActionsOut
) -->
    {Times1 #= Times - 1},
    ( {Times1 #> 0} ->
        % More repetitions needed, recurse immediately
        execute_repeat_managed(
            Times1,
            Original,
            Original,
            Rest,
            ID,
            Status,
            ActionsOut
        )
    ;
        % All repetitions done
        {Status = completed, ActionsOut = Rest}
    ).
