% list action implementation

execute_action_impl(
    ctx_old(Ctx),
    ctx_new(Ctx),
    action(list(Actions)),
    obj_old(ObjIn),
    result(completed, ObjOut)
) :-
    execute_list(Ctx, Actions, ObjIn, ObjOut).

% ==========================================================
% execute_list/4
% ==========================================================
execute_list(_Ctx, Actions, ObjIn, ObjOut) :-
    obj_acns(ObjIn, [_|Rest]),
    append(Actions, Rest, NewActions),
    obj_acns_obj(ObjIn, NewActions, ObjOut).

