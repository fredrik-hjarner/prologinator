% noop action implementation


execute_action_impl(
    action(noop),
    obj_old(ObjIn),
    result(completed, ObjOut)
) -->
    {execute_noop(ObjIn, ObjOut)}.

% ==========================================================
% execute_noop/2
% ==========================================================
execute_noop(ObjIn, ObjOut) :-
    obj_acns(ObjIn, [_|Rest]),
    obj_acns_obj(ObjIn, Rest, ObjOut).

