builtin_action(fork(_)).

execute_action_impl(
    actions_old([fork(Actions)|Rest]),
    obj_id(ID),
    result(completed, actions_new(Rest))
) -->
    execute_fork(ID, Actions).

% NOTE: Important to notice that fork_cmd:s are added in
%       reverse for performance reasons.
execute_fork(ObjID, Actions) -->
    % Add fork command instead of directly modifying
    % actionstore
    ctx_forkCmds(ForkCmdsOld),
    {ForkCmdsNew = [
        fork_cmd(obj_id(ObjID), actions(Actions))
        | ForkCmdsOld
    ]},
    ctx_set_forkCmds(ForkCmdsNew).

