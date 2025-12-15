% define_action action implementation


% Defines a custom action macro at runtime. Stores the
% Signature->Body mapping in user_action/2. When Signature
% is called later, execute_action/5 will expand it with
% parameter substitution and execute the Body.
execute_action_impl(
    action(define_action(Signature, Body)),
    obj_old(ObjIn),
    result(completed, ObjOut)
) -->
    {execute_define_action(
        Signature, Body, ObjIn, ObjOut
    )}.

% ==========================================================
% execute_define_action/4
% ==========================================================
execute_define_action(
    Signature,
    Body,
    ObjIn,
    ObjOut
) :-
    % Store the macro definition
    assertz(user_action(Signature, Body)),
    % Remove this action from queue
    obj_acns(ObjIn, [_|Rest]),
    obj_acns_obj(ObjIn, Rest, ObjOut).

