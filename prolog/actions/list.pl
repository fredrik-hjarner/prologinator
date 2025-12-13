% Delegate to implementation
execute_action_impl(
    ctx_old(CtxOld),
    ctx_new(CtxNew),
    action(list(Actions)),
    obj_old(ObjIn),
    result(Status, ObjOut)
) :-
    execute_list(
        CtxOld, CtxNew, Actions, ObjIn, Status, ObjOut
    ).

% ==========================================================
% execute_list/6
% ==========================================================
execute_list(
    CtxOld, CtxNew, ListActions, ObjIn, Status, ObjOut
) :-
    % 1. Separate the current list action from the rest of
    % the queue
    obj_acns(ObjIn, [_ListAction|RestActions]),

    % 2. Create a temporary object context for the inner
    %    list
    %    This isolates the list's execution scope
    obj_acns_obj(ObjIn, ListActions, TempObj),

    % 3. Execute the inner list until it yields, completes,
    % or despawns
    tick_object(
        ctx_old(CtxOld),
        ctx_new(CtxTemp),
        obj_old(TempObj),
        result(ListStatus, ObjResult)
    ),

    % 4. Handle the result to determine how to reconstruct
    % the main object
    ( ListStatus = despawned ->
        Status = despawned,
        ObjOut = _,
        CtxNew = CtxTemp
    ; ListStatus = yielded ->
        % The inner list paused. Wrap remaining actions back
        %  in list()
        % and keep them at the head of the queue.
        Status = yielded,
        obj_acns(ObjResult, RemainingInner),
        obj_acns_obj(
            ObjResult,
            [list(RemainingInner)|RestActions],
            ObjOut
        ),
        CtxNew = CtxTemp
    ; % ListStatus = completed
        % The inner list finished. We are done with this
        % scope.
        % Continue with the rest of the original queue.
        Status = completed,
        obj_acns_obj(ObjResult, RestActions, ObjOut),
        CtxNew = CtxTemp
    ).