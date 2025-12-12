% noop action implementation


execute_action_impl(
    ctx_old(Ctx),
    ctx_new(Ctx),
    action(noop),
    obj_old(ObjIn),
    result(completed, ObjOut)
) :-
    execute_noop(Ctx, ObjIn, ObjOut).

% ==========================================================
% execute_noop/3
% ==========================================================
execute_noop(_Ctx, ObjIn, ObjOut) :-
    obj_acns(ObjIn, [_|Rest]),
    obj_acns_obj(ObjIn, Rest, ObjOut).

