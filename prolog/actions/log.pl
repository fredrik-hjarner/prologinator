% log action implementation


% Confirmed that it works! Don't worry!
execute_action_impl(
    action(log(Msg)),
    obj_old(ObjIn),
    result(completed, ObjOut),
    Ctx,
    Ctx  % Context unchanged
) :-
    execute_log(Msg, ObjIn, ObjOut).

% ==========================================================
% execute_log/3
% ==========================================================
execute_log(Msg, ObjIn, ObjOut) :-
    obj_acns(ObjIn, [_|Rest]),
    format("~s~n", [Msg]),
    obj_acns_obj(ObjIn, Rest, ObjOut).

