% trigger_state_change action implementation


% IMMEDIATELY updates status in context
execute_action_impl(
    action(trigger_state_change(Change)),
    obj_old(ObjIn),
    result(completed, ObjOut),
    CtxIn,
    CtxOut
) :-
    execute_trigger_state_change(
        Change, ObjIn, ObjOut, CtxIn, CtxOut
    ).

% ==========================================================
% execute_trigger_state_change/5
% ==========================================================
execute_trigger_state_change(
    Change, ObjIn, ObjOut, CtxIn, CtxOut
) :-
    obj_acns(ObjIn, [_|Rest]),
    obj_acns_obj(ObjIn, Rest, ObjOut),
    
    ctx_status(CurrentStatus, CtxIn, CtxIn),
    update_status(Change, CurrentStatus, NewStatus),
    ctx_set_status(NewStatus, CtxIn, CtxOut).

update_status(game_over(lost), _, lost).
update_status(game_over(won), lost, lost).
update_status(game_over(won), _, won).
% update_status(_, S, S). % fallback

