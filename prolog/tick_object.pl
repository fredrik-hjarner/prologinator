% ==========================================================
% Execution Model: tick_object
% ==========================================================

% tick_object/4 threads the context.
% Returns result(Status, Obj) where Status is completed,
% yielded, or despawned.
tick_object(
    obj_old(Obj),
    result(completed, Obj),
    Ctx,
    Ctx
) :-
    obj_acns(Obj, []), % guard.
    !. % TODO: Can this cut hurt bidirectionality?

tick_object(
    obj_old(ObjOld),
    result(Status, ObjOut),
    CtxOld,
    CtxNew
) :-
    object_validation(ObjOld),
    obj_acns(ObjOld, [Act|_Rest]),
    execute_action(
        action(Act),
        obj_old(ObjOld),
        result(ActStatus, ObjTemp),
        CtxOld,
        CtxTemp
    ),
    ( ActStatus = despawned ->
        Status = despawned,
        ObjOut = _,
        CtxNew = CtxTemp
    ; ActStatus = yielded ->
        Status = yielded,
        ObjOut = ObjTemp,
        CtxNew = CtxTemp
    ; % ActStatus = completed
        tick_object(
            obj_old(ObjTemp),
            result(Status, ObjOut),
            CtxTemp,
            CtxNew
        )
    ).

