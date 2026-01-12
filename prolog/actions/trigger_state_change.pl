builtin_action(trigger_state_change(_)).

execute_action_impl(
    actions_old([trigger_state_change(Change)|Rest]),
    obj(_Obj),
    result(completed, actions_new(Rest))
) -->
    execute_trigger_state_change(Change).

execute_trigger_state_change(Change) -->
    ctx_status(CurrentStatus),
    {update_status(Change, CurrentStatus, NewStatus)},
    ctx_set_status(NewStatus).

update_status(game_over(lost), _, lost).
update_status(game_over(won), lost, lost).
update_status(game_over(won), _, won).


