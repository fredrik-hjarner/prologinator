% wait_until(+Condition)
% Description: Waits until the specified condition is
%   satisfied.
%   Supports comparisons, logical composition, and list
%   membership.
%   Yields until the condition is true, then completes.
%
% Examples:
%   wait_until(hp < 0)
%   wait_until(parent_id/hp <= 0)
%   wait_until(sword in inventory)
%   wait_until(and([hp < 0, not(invulnerable = 1)]))

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
    % Try to check condition
    ( check_condition(ID, Condition) ->
        % Condition satisfied - proceed
        {ActionsOut = Rest, Status = completed}
    ;
        % Condition not satisfied - wait (keep action in
        % queue)
        {ActionsOut = [wait_until(Condition)|Rest],
         Status = yielded}
    ).
