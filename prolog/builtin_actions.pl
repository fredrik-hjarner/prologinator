% Built-in Actions Module
% Defines which actions are built-in (with arity checking)

% Check if an action is a built-in action (with arity
% checking)
builtin_action(wait(_)).
builtin_action(move_to(_, _, _)).
builtin_action(move_delta(_, _, _)).
builtin_action(despawn).
builtin_action(spawn(_, _, _, _)).
builtin_action(set_attr(_, _)).
builtin_action(set_attr(_, _, _)).
builtin_action(incr(_, _)).
builtin_action(incr(_, _, _)).
builtin_action(decr(_, _)).
builtin_action(decr(_, _, _)).
builtin_action(loop(_)).
builtin_action(list(_)).
builtin_action(repeat(_, _)).
builtin_action(noop).
builtin_action(parallel_all(_)).
builtin_action(parallel_race(_)).
builtin_action(parallel_all_running(_)).
builtin_action(trigger_state_change(_)).
builtin_action(define_action(_, _)).
builtin_action(load(_)).
builtin_action(log(_)).
builtin_action(wait_key_down(_)).
builtin_action(wait_key_up(_)).
builtin_action(wait_key_held(_)).

