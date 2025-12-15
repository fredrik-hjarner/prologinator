% noop action implementation


execute_action_impl(
    action(noop),
    obj_old(ObjIn),
    result(completed, ObjOut),
    Ctx,
    Ctx
) :-
    execute_noop(Ctx, ObjIn, ObjOut).

% ==========================================================
% execute_noop/3
% ==========================================================
execute_noop(_Ctx, ObjIn, ObjOut) :-
    obj_acns(ObjIn, [_|Rest]),
    obj_acns_obj(ObjIn, Rest, ObjOut).

