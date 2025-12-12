% loop action implementation


execute_action_impl(
    ctx_old(Ctx),
    ctx_new(Ctx),
    action(loop(Actions)),
    obj_old(ObjIn),
    result(completed, ObjOut)
) :-
    execute_loop(Ctx, Actions, ObjIn, ObjOut).

% ==========================================================
% execute_loop/4
% ==========================================================
execute_loop(_Ctx, Actions, ObjIn, ObjOut) :-
    obj_acns(ObjIn, [_|Rest]),
    append(Actions, [loop(Actions)], Expanded),
    append(Expanded, Rest, NewActions),
    obj_acns_obj(ObjIn, NewActions, ObjOut).

