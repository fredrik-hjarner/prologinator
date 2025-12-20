% wait_until(+Path)
% Description: Waits until the specified path exists and
%   resolves. Uses resolve_path to check if the path can be
%   resolved
%   Yields until the path exists, then completes
% Example: wait_until(collision_id) waits until object has
%   collision_id

execute_action_impl(
    actions_old([wait_until(Path)|Rest]),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    execute_wait_until(ID, Path, Rest, Status, ActionsOut).

execute_wait_until(ID, Path, Rest, Status, ActionsOut) -->
    % Try to resolve path (fails if path doesn't exist)
    ( resolve_path(ID, Path, _Val) ->
        % Path exists - proceed
        {ActionsOut = Rest, Status = completed}
    ;
        % Path doesn't exist - wait (keep action in queue)
        {ActionsOut = [wait_until(Path)|Rest],
         Status = yielded}
    ).


