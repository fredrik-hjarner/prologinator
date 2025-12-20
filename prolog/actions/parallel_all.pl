% ==========================================================
% Interface
% ==========================================================

execute_action_impl(
    actions_old([parallel_all(Children)|RestActions]),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    % Execute children sequentially, threading the object
    % state. Stop immediately if any child despawns.
    tick_children(Children, ID, Result),
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

% tick_children(+Children, +ID, -Result, +CtxIn, -CtxOut)
% Result's either 'despawned' or 'remaining(List)'
% Threads the object state (attributes) through each child
% execution via context.

tick_children([], _ID, remaining([])) --> [].

% NOTE: Only the upper-/outer-most actions are executed in
% parallel so to speak. If those are composite actions then
% the composite actions tend to be executed "as normal"
% (unless of course if such an action happens to be a
% parallel_all, parallel_race or similar)
tick_children(
    [FirstTopLayerAcn|TopLayerAcnsRest],
    ID,
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
        obj_id(ID),
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
            ID,
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


