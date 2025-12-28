builtin_action(spawn(_)).

execute_action_impl(
    actions_old([spawn(Actions)|Rest]),
    obj_id(MyID),
    result(completed, actions_new(Rest))
) -->
    execute_spawn(MyID, Actions).

execute_spawn(ParentID, Actions) -->
    {SpawnActions = [
        set_attr(parent_id, ParentID)
        | Actions
    ]},
    ctx_spawnCmds(SpawnCmdsOld),
    {SpawnCmdsNew = [
        spawn_cmd(actions(SpawnActions))
        | SpawnCmdsOld
    ]},
    ctx_set_spawnCmds(SpawnCmdsNew).


