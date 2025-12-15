% Delegate to implementation
execute_action_impl(
    action(list(Actions)),
    obj_old(ObjIn),
    result(Status, ObjOut),
    CtxOld,
    CtxNew
) :-
    execute_list(
        Actions, ObjIn, Status, ObjOut, CtxOld, CtxNew
    ).

% ==========================================================
% execute_list/6
% ==========================================================
execute_list(
    ListActions, ObjIn, Status, ObjOut, CtxOld, CtxNew
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
        obj_old(TempObj),
        result(ListStatus, ObjResult),
        CtxOld,
        CtxTemp
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