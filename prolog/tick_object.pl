% ==========================================================
% Execution Model: tick_object
% ==========================================================

% tick_object threads the context via DCG.
% Returns result(Status, Obj) where Status is completed,
% yielded, or despawned.
tick_object(obj_old(Obj), result(completed, Obj)) -->
    {obj_acns(Obj, [])}, % guard.
    !. % TODO: Can this cut hurt bidirectionality?

tick_object(obj_old(ObjOld), result(Status, ObjOut)) -->
    {object_validation(ObjOld)},
    {obj_acns(ObjOld, [Act|_Rest])},
    execute_action(
        action(Act),
        obj_old(ObjOld),
        result(ActStatus, ObjTemp)
    ),
    ( {ActStatus = despawned} ->
        {Status = despawned, ObjOut = _}
    ; {ActStatus = yielded} ->
        {Status = yielded, ObjOut = ObjTemp}
    ; % ActStatus = completed
        tick_object(
            obj_old(ObjTemp), result(Status, ObjOut)
        )
    ).

