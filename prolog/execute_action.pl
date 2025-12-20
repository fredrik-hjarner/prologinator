% Action Execution Module
% Handles execution of all game actions
% Core interface - implementations are in separate modules

% Multifile declaration for execute_action_impl/5
% MUST be declared before loading implementation modules
% Implementation clauses are provided by the modules below
% NOTE: discontiguous declaration is now in
% prolog/discontiguous.pl

% Load all action implementation modules (after multifile
%   declaration)

% ==========================================================
% Custom Actions: Runtime Expansion
% ==========================================================

% Dynamic predicate to store user-defined action definitions
% NOTE: dynamic declaration is now in prolog/dynamic.pl

% ==========================================================
% Top-level execute_action (with resolution)
% ==========================================================
execute_action(
    action(Action),
    actions_old(ActionsIn),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    % 1. Resolve value specs in action
    resolve_action(ID, Action, ResolvedAction),
    % 2. Delegate to existing logic (renamed)
    % We need to put the ResolvedAction as new head on
    % actions_old
    {ActionsIn = [_|Rest]},
    execute_action_resolved(
        action(ResolvedAction),
        actions_old([ResolvedAction|Rest]),
        obj_id(ID),
        result(Status, actions_new(ActionsOut))
    ).

% ==========================================================
% execute_action_resolved
% ==========================================================
% Wrapper: validates action then delegates to implementation
% Now threads Context directly to accumulate side effects.
% Also handles user-defined actions via runtime expansion.
execute_action_resolved(
    action(Action),
    actions_old(ActionsIn),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    % TODO: action_validation nowadays it almost useless
    %       since custom actions allow anything.
    %       so maybe I should validate builtins separately?
    % {action_validation(Action)},
    % validate(Action, action_schema),
    ( {builtin_action(Action)} ->
        % It's a built-in action - execute normally
        execute_action_impl(
            actions_old(ActionsIn),
            obj_id(ID),
            result(Status, actions_new(ActionsOut))
        )
    ; {user_action(Action, Body)} -> % total prolog voodoo!
        % It's a user-defined action!
        % Action unifies with Template, binding variables
        % in Body automatically
        % Body now has the correct bindings, use it directly
        % Note: Body may contain attr() specs, so recurse
        % through top-level execute_action for resolution
        % Replace the user action with its Body in the queue
        {ActionsIn = [Action|Rest]},
        {ExpandedActions = [Body|Rest]},
        execute_action(
            action(Body),
            actions_old(ExpandedActions),
            obj_id(ID),
            result(Status, actions_new(ActionsOut))
        )
    ;
        % Unknown action
        {throw(unknown_action(Action))}
    ).

% ==========================================================
% execute_action_impl/5
% ==========================================================
% Internal implementation (no validation)
% Implementation clauses are provided by individual action
%   files in ./actions/ directory (one file per action)
% ==========================================================

