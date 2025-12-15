execute_action_impl(
    action(parallel_race(Children)),
    obj_old(ObjIn),
    result(Status, ObjOut),
    CtxOld,
    CtxNew
) :-
    execute_parallel_race(
        Children,
        ObjIn,
        result(Status, ObjOut),
        CtxOld,
        CtxNew
    ).

% ==========================================================
% Implementation
% ==========================================================

execute_parallel_race(
    Children, ObjIn, result(Status, ObjOut), CtxOld, CtxNew
) :-
    obj_acns(ObjIn, [_ParentAction|RestActions]),
    
    % Execute children sequentially
    % (stop immediately if one finishes).
    tick_children_race(
        Children, ObjIn, Result, CtxOld, CtxTemp
    ),
    
    ( Result = despawned ->
        Status = despawned,
        ObjOut = _,
        CtxNew = CtxTemp
    ; Result = race_won(FinalObj) ->
        % A child finished! The race is won.
        % We discard losers' actions and restore parent
        % actions.
        Status = completed,
        obj_acns_obj(FinalObj, RestActions, ObjOut),
        CtxNew = CtxTemp
    ; Result = ongoing(RemainingChildren, FinalObj) ->
        % Everyone yielded.
        % We yield and update the parallel_race state.
        Status = yielded,
        obj_acns_obj(
            FinalObj,
            [parallel_race(RemainingChildren)|RestActions],
            ObjOut
        ),
        CtxNew = CtxTemp
    ).

% ==========================================================
% Helpers
% ==========================================================

% tick_children_race(+Children, +ObjIn, -Result, +CtxIn,
%     -CtxOut)
% Result is one of:
%   - despawned
%   - race_won(FinalObj)       <-- At least one finished
%   - ongoing(List, FinalObj)  <-- All children yielded
tick_children_race([], Obj, ongoing([], Obj), Ctx, Ctx).

tick_children_race(
    [ChildAction|RestChildren],
    ObjIn,
    Result,
    CtxIn,
    CtxOut
) :-
    % 1. Normalize child to list
    ( ChildAction = [_|_] ->
        ChildList = ChildAction
    ;
        ChildList = [ChildAction]
    ),
    
    % 2. Prepare temp object
    obj_acns_obj(ObjIn, ChildList, TempObj),
    
    % 3. Tick the child
    tick_object(
        obj_old(TempObj),
        result(ChildStatus, ObjAfterTick),
        CtxIn,
        CtxTemp
    ),
    
    ( ChildStatus = despawned ->
        Result = despawned,
        CtxOut = CtxTemp
        
    ; ChildStatus = completed ->
        % WINNER: Stop immediately (Skip fairness).
        % Subsequent children are NOT executed.
        Result = race_won(ObjAfterTick),
        CtxOut = CtxTemp
        
    ; % ChildStatus = yielded
        obj_acns(ObjAfterTick, RemainingActions),
        
        % Child yielded, check the next racer
        tick_children_race(
            RestChildren,
            ObjAfterTick, % Thread state
            RestResult,
            CtxTemp,
            CtxOut
        ),
        
        ( RestResult = despawned ->
            Result = despawned
        ; RestResult = race_won(FinalObj) ->
            % Someone downstream won. Propagate the win.
            Result = race_won(FinalObj)
        ; RestResult = ongoing(RestRemaining, FinalObj) ->
            % No one won. Flatten the list
            % (Fix nested list bug).
            append(
                RemainingActions,
                RestRemaining,
                AllRemaining
            ),
            Result = ongoing(AllRemaining, FinalObj)
        )
    ).