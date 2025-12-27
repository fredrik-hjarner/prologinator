builtin_action(define_action(_, _)).

% Defines a custom action macro at runtime. Stores the
% Signature->Body mapping in user_action/2. When Signature
% is called later, execute_action/5 will expand it with
% parameter substitution and execute the Body.
execute_action_impl(
    actions_old([define_action(Signature, Body)|Rest]),
    obj_id(_ID),
    result(completed, actions_new(Rest))
) -->
    execute_define_action(Signature, Body).

execute_define_action(Signature, Body) -->
    {assertz(user_action(Signature, Body))}.


