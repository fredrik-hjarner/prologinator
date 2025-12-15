execute_action_impl(
    action(parallel_race(Children)),
    obj_old(ObjIn),
    result(Status, ObjOut),
    CtxOld,
    CtxNew
) :-
    execute_parallel_race(
        CtxOld, CtxNew,
        Children,
        ObjIn,
        result(Status, ObjOut)
    ).

% ==========================================================
% Implementation
% ==========================================================

execute_parallel_race(
    CtxOld, CtxNew, Children, ObjIn, result(Status, ObjOut)
) :-
    obj_acns(ObjIn, [_ParentAction|RestActions]),
    
    % Execute children sequentially
    % (stop immediately if one finishes).
    tick_children_race(
        CtxOld, CtxTemp, Children, ObjIn, Result
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

% tick_children_race(+CtxIn, -CtxOut, +Children, +ObjIn,
%     -Result)
% Result is one of:
%   - despawned
%   - race_won(FinalObj)       <-- At least one finished
%   - ongoing(List, FinalObj)  <-- All children yielded
tick_children_race(Ctx, Ctx, [], Obj, ongoing([], Obj)).

tick_children_race(
    CtxIn, CtxOut,
    [ChildAction|RestChildren],
    ObjIn,
    Result
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
        ctx_old(CtxIn),
        ctx_new(CtxTemp),
        obj_old(TempObj),
        result(ChildStatus, ObjAfterTick)
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
            CtxTemp, CtxOut,
            RestChildren,
            ObjAfterTick, % Thread state
            RestResult
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