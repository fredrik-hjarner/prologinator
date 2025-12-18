execute_action_impl(
    action(parallel_race(Children)),
    actions_old([_ParentAction|RestActions]),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    % Execute children sequentially (stop immediately
    % if one finishes).
    tick_children_race(Children, ID, Result),
    {( Result = despawned ->
        Status = despawned,
        ActionsOut = []
    ; Result = race_won ->
        % A child finished! The race is won.
        % We discard losers' actions and restore parent
        % actions.
        Status = completed,
        ActionsOut = RestActions
    ; Result = ongoing(RemainingChildren) ->
        % Everyone yielded.
        % We yield and update the parallel_race state.
        Status = yielded,
        ActionsOut = [parallel_race(
            RemainingChildren)|RestActions]
    )}.

% ==========================================================
% Helpers
% ==========================================================

% tick_children_race(+Children, +ID, -Result, +CtxIn,
% -CtxOut)
% Result is one of:
%   - despawned
%   - race_won       <-- At least one finished
%   - ongoing(List)  <-- All children yielded
tick_children_race([], _ID, ongoing([])) --> [].

tick_children_race(
    [ChildAction|RestChildren],
    ID,
    Result
) -->
    % 1. Normalize child to list
    {( ChildAction = [_|_] ->
        ChildList = ChildAction
    ;
        ChildList = [ChildAction]
    )},
    % 2. Tick the child
    tick_object(
        actions_old(ChildList),
        obj_id(ID),
        result(ChildStatus, actions_new(RemainingActions))
    ),
    ( {ChildStatus = despawned} ->
        {Result = despawned}
    ; {ChildStatus = completed} ->
        % WINNER: Stop immediately (Skip fairness).
        % Subsequent children are NOT executed.
        {Result = race_won}
    ; % ChildStatus = yielded
        % Child yielded, check the next racer
        tick_children_race(
            RestChildren,
            ID,
            RestResult
        ),
        {( RestResult = despawned ->
            Result = despawned
        ; RestResult = race_won ->
            % Someone downstream won. Propagate the win.
            Result = race_won
        ; RestResult = ongoing(RestRemaining) ->
            % No one won. Flatten the list
            % (Fix nested list bug).
            append(
                RemainingActions,
                RestRemaining,
                AllRemaining
            ),
            Result = ongoing(AllRemaining)
        )}
    ).


