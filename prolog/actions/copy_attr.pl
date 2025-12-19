% copy_attr action implementation

execute_action_impl(
    action(copy_attr(SourcePath, DestPath)),
    actions_old([_|Rest]),
    obj_id(MyID),
    result(completed, actions_new(Rest))
) -->
    execute_copy_attr(MyID, SourcePath, DestPath).

% ==========================================================
% execute_copy_attr/6
% ==========================================================
execute_copy_attr(MyID, SourcePath, DestPath) -->
    resolve_path_to_attr(MyID, SourcePath,
                         SourceID/SourceKey),
    resolve_path_to_attr(MyID, DestPath,
                         DestID/DestKey),
    ctx_attr_val(SourceID/SourceKey, Value),
    ctx_set_attr_val(DestID/DestKey, Value).


