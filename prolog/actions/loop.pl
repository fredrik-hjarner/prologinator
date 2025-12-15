% loop action implementation


execute_action_impl(
    action(loop(Actions)),
    obj_old(ObjIn),
    result(completed, ObjOut)
) -->
    {execute_loop(Actions, ObjIn, ObjOut)}.

% ==========================================================
% execute_loop/3
% ==========================================================
execute_loop(Actions, ObjIn, ObjOut) :-
    obj_acns(ObjIn, [_|Rest]),
    append(Actions, [loop(Actions)], Expanded),
    append(Expanded, Rest, NewActions),
    obj_acns_obj(ObjIn, NewActions, ObjOut).

