builtin_action(fork(_)).

execute_action_impl(
    actions_old([fork(Actions)|Rest]),
    obj(Obj),
    result(completed, actions_new(Rest))
) -->
    {obj_id(Obj, ID)},
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

