builtin_action(loop(_)).
builtin_action(loop(_, _)). % loop continuation

% loop(+Actions)
% Mode: loop(+Actions)
% Description: Executes Actions repeatedly.
%   Self-managed: threads the state of the loop body across
%   frames.
%   If the body finishes in a tick, it restarts immediately
%   in the same tick.

% Initial call: loop(Actions)
execute_action_impl(
    actions_old([loop(Actions)|Rest]),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    % Start the loop with Actions as both Running and
    %Original
    execute_loop_managed(
        Actions, Actions, Rest, ID, Status, ActionsOut
    ).

% Continuation: loop(Running, Original)
execute_action_impl(
    actions_old([loop(Running, Original)|Rest]),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    execute_loop_managed(
        Running, Original, Rest, ID, Status, ActionsOut
    ).

execute_loop_managed(
    Running, Original, Rest, ID, Status, ActionsOut
) -->
    % Execute the current running actions (threads context)
    tick_object(
        actions_old(Running),
        obj_id(ID),
        result(RunStatus, actions_new(RunRemaining))
    ),
    handle_loop_result(
        RunStatus,
        RunRemaining,
        Original,
        Rest,
        ID,
        Status,
        ActionsOut
    ).

% Case 1: Despawned
handle_loop_result(
    despawned, _, _, _, _, despawned, []
) --> [].

% Case 2: Yielded - Body yielded, so we yield the loop state
handle_loop_result(
    yielded,
    RunRemaining,
    Original,
    Rest,
    _,
    yielded,
    [loop(RunRemaining, Original)|Rest]
) --> [].

% Case 3: Completed - Body finished, restart loop
% immediately
handle_loop_result(
    completed, _, Original, Rest, ID, Status, ActionsOut
) -->
    execute_loop_managed(
        Original, Original, Rest, ID, Status, ActionsOut
    ).
