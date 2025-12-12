% trigger_state_change action implementation


% IMMEDIATELY updates status in context
execute_action_impl(
    ctx_old(CtxIn),
    ctx_new(CtxOut),
    action(trigger_state_change(Change)),
    obj_old(ObjIn),
    result(completed, ObjOut)
) :-
    execute_trigger_state_change(
        CtxIn, CtxOut, Change, ObjIn, ObjOut
    ).

% ==========================================================
% execute_trigger_state_change/5
% ==========================================================
execute_trigger_state_change(
    CtxIn, CtxOut, Change, ObjIn, ObjOut
) :-
    obj_acns(ObjIn, [_|Rest]),
    obj_acns_obj(ObjIn, Rest, ObjOut),
    
    ctx_status(CtxIn, CurrentStatus),
    update_status(Change, CurrentStatus, NewStatus),
    ctx_status_ctx(CtxIn, NewStatus, CtxOut).

update_status(game_over(lost), _, lost).
update_status(game_over(won), lost, lost).
update_status(game_over(won), _, won).
% update_status(_, S, S). % fallback

