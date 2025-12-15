% loop action implementation


execute_action_impl(
    action(loop(Actions)),
    obj_old(ObjIn),
    result(completed, ObjOut),
    Ctx,
    Ctx
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

