builtin_action(parallel_all(_)).

% ==========================================================
% Interface
% ==========================================================

execute_action_impl(
    actions_old([parallel_all(Children)|RestActions]),
    obj(Obj),
    result(Status, actions_new(ActionsOut))
) -->
    % {obj_id(Obj, ID)},
    % Execute children sequentially, threading the object
    % state. Stop immediately if any child despawns.
    tick_children(Children, Obj, Result),
    {( Result = despawned ->
        Status = despawned,
        ActionsOut = []
    ; Result = remaining(RemainingChildren) ->
        ( RemainingChildren = [] ->
            % All children finished actions.
            Status = completed,
            ActionsOut = RestActions
        ;
            % Some children yielded or are still running.
            % We yield the parallel_all action itself to
            % continue next tick.
            Status = yielded,
            ActionsOut = [parallel_all(
                RemainingChildren)|RestActions]
        )
    )}.

% ==========================================================
% Helpers
% ==========================================================

% tick_children(+Children, +Obj, -Result, +CtxIn, -CtxOut)
% Result's either 'despawned' or 'remaining(List)'
% Threads the object state (attributes) through each child
% execution via context.

tick_children([], _Obj, remaining([])) --> [].

% NOTE: Only the upper-/outer-most actions are executed in
% parallel so to speak. If those are composite actions then
% the composite actions tend to be executed "as normal"
% (unless of course if such an action happens to be a
% parallel_all, parallel_race or similar)
tick_children(
    [FirstTopLayerAcn|TopLayerAcnsRest],
    Obj,
    Result
) -->
    % Normalize child to a list of actions (handle single
    % action atoms)
    {( FirstTopLayerAcn = [_|_] ->
        FirstTopLayerAcnList = FirstTopLayerAcn
    ;
        FirstTopLayerAcnList = [FirstTopLayerAcn]
    )},
    % Tick the actions (executes until yield, despawn, or
    % complete)
    tick_object(
        actions_old(FirstTopLayerAcnList),
        obj(Obj),
        result(
            ChildStatusAfterTick,
            actions_new(RemainingAcnsAfterTick)
        )
    ),
    % So after tick the `remaining actions` in
    % RemainingAcnsAfterTick.
    ( {ChildStatusAfterTick = despawned} ->
        % Immediate exit on despawn
        {Result = despawned}
    ;
        % FirstTopLayerAcn yielded or completed.
        % Attributes are updated in context automatically.
        % We collect remaining actions and continue with
        % next child.
        tick_children(
            TopLayerAcnsRest,
            Obj,
            % RestResult contains all remaining actions
            % of the top level actions (except the first
            % which we already executed above) have each
            % been ticked, having been built up by the
            % code beneath.
            RestResult
        ),
        {( RestResult = despawned ->
            Result = despawned
        ; RestResult = remaining(RestRemaining) ->
            ( RemainingAcnsAfterTick = [] ->
                Result = remaining(RestRemaining)
            ;
                append(
                    RemainingAcnsAfterTick,
                    RestRemaining,
                    AllRemaining
                ),
                Result = remaining(AllRemaining)
            )
        )}
    ).


