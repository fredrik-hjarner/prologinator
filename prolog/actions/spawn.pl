execute_action_impl(
    action(spawn(Actions)),
    actions_old([_|Rest]),
    obj_id(MyID),
    result(completed, actions_new(Rest))
) -->
    execute_spawn(MyID, Actions).

execute_spawn(ParentID, Actions) -->
    % Build actions list with parent_id automatically added
    {SpawnActions = [
        set_attr(parent_id, ParentID)
        | Actions
    ]},
    % Add spawn command instead of directly modifying
    % actionstore
    ctx_spawnCmds(SpawnCmdsOld),
    {append(SpawnCmdsOld,
            [spawn_cmd(actions(SpawnActions))],
            SpawnCmdsNew)},
    ctx_set_spawnCmds(SpawnCmdsNew).


