% wait action implementation

execute_action_impl(
    ctx_old(Ctx),
    ctx_new(Ctx), % Context unchanged
    action(wait(N)),
    obj_old(ObjIn),
    result(Status, ObjOut)
) :-
    execute_wait(Ctx, N, ObjIn, Status, ObjOut).

% ==========================================================
% execute_wait/5
% ==========================================================
execute_wait(_Ctx, N, ObjIn, Status, ObjOut) :-
    obj_acns(ObjIn, [_|Rest]),
    ( N #> 0 ->
        N1 #= N - 1,
        ( N1 #> 0 ->
            obj_acns_obj(ObjIn, [wait(N1)|Rest], ObjOut)
        ;
            obj_acns_obj(ObjIn, Rest, ObjOut)
        ),
        Status = yielded
    ;
        obj_acns_obj(ObjIn, Rest, ObjOut),
        Status = completed
    ).

