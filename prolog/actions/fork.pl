builtin_action(fork(_)).

execute_action_impl(
    actions_old([fork(Actions)|Rest]),
    obj_id(ID),
    result(completed, actions_new(Rest))
) -->
    execute_fork(ID, Actions).

execute_fork(ObjID, Actions) -->
    ctx_forkCmds(ForkCmdsOld),
    {ForkCmdsNew = [
        fork_cmd(obj_id(ObjID), actions(Actions))
        | ForkCmdsOld
    ]},
    ctx_set_forkCmds(ForkCmdsNew).

