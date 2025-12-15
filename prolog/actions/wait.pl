% wait action implementation

execute_action_impl(
    action(wait(N)),
    obj_old(ObjIn),
    result(Status, ObjOut)
) -->
    {execute_wait(N, ObjIn, Status, ObjOut)}.

% ==========================================================
% execute_wait/4
% ==========================================================
execute_wait(N, ObjIn, Status, ObjOut) :-
    obj_acns(ObjIn, [_|Rest]),
    ( N = 0 ->
        % wait(0): noop, removes itself, completes
        obj_acns_obj(ObjIn, Rest, ObjOut),
        Status = completed
    ; N = 1 ->
        % wait(1): yields but also removes itself
        obj_acns_obj(ObjIn, Rest, ObjOut),
        Status = yielded
    ; N #> 1 ->
        % wait(N>1): decrement and yield
        N1 #= N - 1,
        obj_acns_obj(ObjIn, [wait(N1)|Rest], ObjOut),
        Status = yielded
    ).

