% ==========================================================
% Interface
% ==========================================================

% TODO: Clause added just get things through.
%       Remove entire clause when solution to module
%       dependency issue is found.
% This clause just skips the action and returns completed.
% execute_action_impl(
%     ctx_old(Ctx),
%     ctx_new(Ctx),
%     action(parallel_all(_Children)),
%     obj_old(ObjIn),
%     result(completed, ObjOut)
% ) :-
%     obj_acns(ObjIn, [_|Rest]),
%     obj_acns_obj(ObjIn, Rest, ObjOut).

execute_action_impl(
    ctx_old(CtxOld),
    ctx_new(CtxNew),
    action(parallel_all(Children)),
    obj_old(ObjIn),
    result(Status, ObjOut)
) :-
    execute_parallel_all(
        CtxOld, CtxNew,
        Children,
        ObjIn,
        result(Status, ObjOut)
    ).

% ==========================================================
% Implementation
% ==========================================================

execute_parallel_all(
    CtxOld, CtxNew, Children, ObjIn, result(Status, ObjOut)
) :-
    obj_acns(ObjIn, [_ParentAction|RestActions]),
    
    % Execute children sequentially, threading the object
    % state.
    % Stop immediately if any child despawns.
    tick_children(CtxOld, CtxTemp, Children, ObjIn, Result),
    
    ( Result = despawned ->
        Status = despawned,
        ObjOut = _,
        CtxNew = CtxTemp
    ; Result = remaining(RemainingChildren, FinalObj) ->
        ( RemainingChildren = [] ->
            % All children finished actions.
            % Restore parent actions to the final object
            % state.
            Status = completed,
            obj_acns_obj(FinalObj, RestActions, ObjOut),
            CtxNew = CtxTemp
        ;
            % Some children yielded or are still running.
            % We yield the parallel_all action itself to
            % continue next tick.
            % Update actions on the final object state.
            Status = yielded,
            obj_acns_obj(
                FinalObj,
                [parallel_all(
                    RemainingChildren)|RestActions
                ],
                ObjOut
            ),
            CtxNew = CtxTemp
        )
    ).

% ==========================================================
% Helpers
% ==========================================================

% tick_children(+CtxIn, -CtxOut, +Children, +ObjIn, -Result)
% Result's either 'despawned' or 'remaining(List, FinalObj)'
% Threads the object state (attributes) through each child
% execution.

tick_children(Ctx, Ctx, [], Obj, remaining([], Obj)).

% NOTE: Only the upper-/outer-most actions are executed in
% parallel so to speak. If those are composite actions then
% the composite actions tend to be executed "as normal"
% (unless of course if such an action happens to be a
% parallel_all, parallel_race or similar)
tick_children(
    CtxIn, CtxOut,
    [FirstTopLayerAcn|TopLayerAcnsRest],
    ObjIn,
    Result
) :-
    % Normalize child to a list of actions (handle single
    % action atoms)
    ( FirstTopLayerAcn = [_|_] ->
        FirstTopLayerAcnList = FirstTopLayerAcn
    ;
        FirstTopLayerAcnList = [FirstTopLayerAcn]
    ),
    
    % Prepare temp object with child actions (preserving
    % ObjIn attributes)
    obj_acns_obj(ObjIn, FirstTopLayerAcnList, ObjWithAcn),
    
    % Tick the object (executes until yield, despawn, or
    % complete)
    tick_object(
        ctx_old(CtxIn),
        ctx_new(CtxAfterTIck),
        obj_old(ObjWithAcn),
        result(ChildStatusAfterTick, ObjChildAfterTick)
    ),
    % So after tick the `remaining actions` in
    % ChildStatusAfterTick.
    
    ( ChildStatusAfterTick = despawned ->
        % Immediate exit on despawn
        Result = despawned,
        CtxOut = CtxAfterTIck
    ;
        % FirstTopLayerAcn yielded or completed.
        % ObjChildAfterTick contains the updated attributes
        % and remaining actions.
        % We extract actions to store them, but pass the
        % object state to the next child.
        obj_acns(ObjChildAfterTick, RemainingAcnsAfterTick),
        
        tick_children(
            CtxAfterTIck, CtxOut,
            TopLayerAcnsRest,
            ObjChildAfterTick,
            % RestResult contains all remaining actions of
            % the top level actions (except the first which
            % we already executed above) have each been
            % ticked, having been built up by the code
            % beneath.
            RestResult
        ),
        
        ( RestResult = despawned ->
            Result = despawned
        ; RestResult = remaining(RestRemaining, FinalObj) ->
            ( RemainingAcnsAfterTick = [] ->
                Result = remaining(RestRemaining, FinalObj)
            ;
                append(
                    RemainingAcnsAfterTick,
                    RestRemaining,
                    AllRemaining
                ),
                Result = remaining(AllRemaining, FinalObj)
            )
        )
    ).