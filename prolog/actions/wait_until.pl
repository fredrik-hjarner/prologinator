% wait_until action implementation

% wait_until(+Path)
% Description: Waits until the specified path exists and
%   resolves. Uses resolve_path to check if the path can be
%   resolved
%   Yields until the path exists, then completes
% Example: wait_until(collision_id) waits until object has
%   collision_id

execute_action_impl(
    ctx_old(Ctx),
    ctx_new(Ctx),  % Context unchanged
    action(wait_until(Path)),
    obj_old(ObjIn),
    result(Status, ObjOut)
) :-
    execute_wait_until(
        Ctx, Path, ObjIn, Status, ObjOut
    ).

% ==========================================================
% execute_wait_until/5
% ==========================================================
execute_wait_until(Ctx, Path, ObjIn, 
                   Status, ObjOut) :-
    obj_id(ObjIn, MyID),
    obj_acns(ObjIn, [_|Rest]),
    
    % Try to resolve path (fails if path doesn't exist)
    ( resolve_path(Ctx, MyID, Path, _Val) ->
        % Path exists - proceed
        obj_acns_obj(ObjIn, Rest, ObjOut),
        Status = completed
    ;
        % Path doesn't exist - wait (keep action in queue)
        obj_acns_obj(
            ObjIn,
            [wait_until(Path)|Rest],
            ObjOut
        ),
        Status = yielded
    ).

