% repeat action implementation

% repeat(+Times, +Actions)
% Mode: repeat(+Times, +Actions)
% Description: Execute action list N times, then
%   continue
% Yields: false (expands immediately)

execute_action_impl(
    action(repeat(Times, Acts)),
    actions_old([_|Rest]),
    obj_id(_ID),
    result(completed, actions_new(NewActions))
) -->
    execute_repeat(Times, Acts, Rest, NewActions).

% ==========================================================
% execute_repeat/4
% ==========================================================
execute_repeat(Times, Acts, Rest, NewActions) -->
    {
        Times #> 0,
        Times1 #= Times - 1,
        % Build next action(s)
        ( Times1 #> 0 ->
            % More reps: add another repeat
            NextRepeat = [repeat(Times1, Acts)]
        ;
            % Last rep: no more repeat
            NextRepeat = []
        ),
        % Queue: Actions + NextRepeat + Rest
        append(Acts, NextRepeat, Tail),
        append(Tail, Rest, NewActions)
    }.


