% log action implementation


% Confirmed that it works! Don't worry!
execute_action_impl(
    ctx_old(Ctx),
    ctx_new(Ctx), % Context unchanged
    action(log(Msg)),
    obj_old(ObjIn),
    result(completed, ObjOut)
) :-
    execute_log(Ctx, Msg, ObjIn, ObjOut).

% ==========================================================
% execute_log/4
% ==========================================================
execute_log(_Ctx, Msg, ObjIn, ObjOut) :-
    obj_acns(ObjIn, [_|Rest]),
    format("~s~n", [Msg]),
    obj_acns_obj(ObjIn, Rest, ObjOut).

