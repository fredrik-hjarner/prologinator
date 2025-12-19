% fork action implementation

execute_action_impl(
    action(fork(Actions)),
    actions_old([_|Rest]),
    obj_id(ID),
    result(completed, actions_new(Rest))
) -->
    execute_fork(ID, Actions).

% ==========================================================
% execute_fork/4
% ==========================================================
execute_fork(ObjID, Actions) -->
    % Add fork command instead of directly modifying
    % actionstore
    ctx_forkCmds(ForkCmdsOld),
    {append(ForkCmdsOld,
            [fork_cmd(obj_id(ObjID), actions(Actions))],
            ForkCmdsNew)},
    ctx_set_forkCmds(ForkCmdsNew).

