builtin_action(spawn(_)).

execute_action_impl(
    actions_old([spawn(Actions)|Rest]),
    obj(Obj),
    result(completed, actions_new(Rest))
) -->
    {obj_id(Obj, MyID)},
    execute_spawn(MyID, Actions).

% NOTE: Important to notice that spawn_cmd:s are added in
%       reverse for performance reasons.
execute_spawn(ParentID, Actions) -->
    % Build actions list with parent_id automatically added
    {SpawnActions = [
        set_attr(parent_id, ParentID)
        | Actions
    ]},
    % Add spawn command instead of directly modifying
    % actionstore
    ctx_spawnCmds(SpawnCmdsOld),
    {SpawnCmdsNew = [
        spawn_cmd(actions(SpawnActions))
        | SpawnCmdsOld
    ]},
    ctx_set_spawnCmds(SpawnCmdsNew).


