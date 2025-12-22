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

execute_action_impl(
    action(attr_if(Condition, ThenActions, ElseActions)),
    actions_old([_|Rest]),
    obj_id(ID),
    result(completed, actions_new(ActionsOut))
) -->
    execute_attr_if(
        ID,
        Condition,
        ThenActions,
        ElseActions,
        Rest,
        ActionsOut
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
    ActionsOut
) -->
    ctx_get(Ctx),
    (   {check_condition(Ctx, ObjID, Condition)}
    ->
        % Condition succeeded: execute ThenActions
        {append(ThenActions, Rest, ActionsOut)}
    ;
        % Condition failed: execute ElseActions
        {append(ElseActions, Rest, ActionsOut)}
    ).
