% spawn action implementation

execute_action_impl(
    action(spawn(Type, X, Y, Actions)),
    actions_old([_|Rest]),
    obj_id(MyID),
    result(completed, actions_new(Rest))
) -->
    execute_spawn(MyID, Type, X, Y, Actions).

% ==========================================================
% execute_spawn/9
% ==========================================================
execute_spawn(ParentID, Type, X, Y, Actions) -->
    % Build actions list with set_attr for type, x, y,
    % parent_id
    {SpawnActions = [
        set_attr(type, Type),
        set_attr(x, X),
        set_attr(y, Y),
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


