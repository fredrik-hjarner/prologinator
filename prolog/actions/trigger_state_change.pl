% trigger_state_change action implementation

execute_action_impl(
    action(trigger_state_change(Change)),
    actions_old([_|Rest]),
    obj_id(_ID),
    result(completed, actions_new(Rest))
) -->
    execute_trigger_state_change(Change).

% ==========================================================
% execute_trigger_state_change/5
% ==========================================================
execute_trigger_state_change(Change) -->
    ctx_status(CurrentStatus),
    {update_status(Change, CurrentStatus, NewStatus)},
    ctx_set_status(NewStatus).

update_status(game_over(lost), _, lost).
update_status(game_over(won), lost, lost).
update_status(game_over(won), _, won).


