builtin_action(define_action(_, _)).

execute_action_impl(
    actions_old([define_action(Signature, Body)|Rest]),
    obj_id(_ID),
    result(completed, actions_new(Rest))
) -->
    execute_define_action(Signature, Body).

execute_define_action(Signature, Body) -->
    {assertz(user_action(Signature, Body))}.


