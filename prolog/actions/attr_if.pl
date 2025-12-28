builtin_action(attr_if(_, _)). % if-then
builtin_action(attr_if(_, _, _)). % if-then-else-then

% if-then
execute_action_impl(
    actions_old([
        attr_if(Condition, ThenActions)|Rest
    ]),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    (check_condition(ID, Condition)
    ->
        % Condition succeeded: execute ThenActions
        tick_object(
            actions_old(ThenActions),
            obj_id(ID),
            result(Status, actions_new(ActionAfterTick))
        ),
        {append(ActionAfterTick, Rest, ActionsOut)}
    ;
        % Else: do nothing.
        {Status = completed},
        {ActionsOut = Rest}
    ).

% =========================================================
% attr_if Action
% =========================================================
% Conditionally executes one of two action lists based on
% a condition.
%
% Syntax: attr_if(Condition, ThenActions, ElseActions)
%
% Semantics:
%   - Checks Condition immediately (non-blocking)
%   - If true: executes ThenActions
%   - If false: executes ElseActions
%   - Always completes in the same frame (instant)
%
% Condition supports same forms as wait_until:
%   - Comparisons: hp < 0, parent_id/hp >= 10
%   - Membership: sword in inventory
%   - Logical: and([...]), or([...]), not(...)

% if-then-else-then
execute_action_impl(
    actions_old([
        attr_if(Condition, ThenActions, ElseActions)|Rest
    ]),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    execute_attr_if(
        ID,
        Condition,
        ThenActions,
        ElseActions,
        Rest,
        result(Status, actions_new(ActionsOut))
    ).

% ==========================================================
% Implementation: execute_attr_if
% ==========================================================
% Immediate conditional: checks condition once and
% branches. No yielding.

execute_attr_if(
    ObjID,
    Condition,
    ThenActions,
    ElseActions,
    Rest,
    result(Status, actions_new(ActionsOut))
) -->
    (check_condition(ObjID, Condition)
    ->
        % Condition succeeded: execute ThenActions
        tick_object(
            actions_old(ThenActions),
            obj_id(ObjID),
            result(Status, actions_new(ActionAfterTick))
        ),
        {append(ActionAfterTick, Rest, ActionsOut)}
    ;
        % Condition failed: execute ElseActions
        tick_object(
            actions_old(ElseActions),
            obj_id(ObjID),
            result(Status, actions_new(ActionAfterTick))
        ),
        {append(ActionAfterTick, Rest, ActionsOut)}
    ).
