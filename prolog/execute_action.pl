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
    actions_old([Action|Rest]),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    % TODO: Add toggle to enable/disable debug logging here
    % {Action =.. [Functor|_]},
    % {format("execute_action: ~w~n", [Functor])},
    % 1. Resolve value specs in action
    resolve_action(ID, Action, ResolvedAction),
    % 2. Delegate to existing logic (renamed)
    % We need to put the ResolvedAction as new head on
    % actions_old
    execute_action_resolved(
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
    actions_old([Action|Rest]),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    % TODO: action_validation nowadays it almost useless
    %       since custom actions allow anything.
    %       so maybe I should validate builtins separately?
#ifdef ENABLE_VALIDATION
    {action_validation(Action)},
#endif
    % validate(Action, action_schema),
    ( {builtin_action(Action)} ->
        % It's a built-in action - execute normally
#ifdef ENABLE_LOG_ACTIONS
        {functor(Action, Functor, _)},
        {format("executing `~w`~n", [Functor])},
#endif
        % execute_action_impl(
        %     actions_old([Action|Rest]),
        %     obj_id(ID),
        %     result(Status, actions_new(ActionsOut))
        % )
        catch_dcg(
            execute_action_impl(
                actions_old([Action|Rest]),
                obj_id(ID),
                result(Status, actions_new(ActionsOut))
            ),
            Error,
            ( write('Error during execute_action_impl: '),
              write(Error), nl,
              throw(Error)
            )
        )
    ; {user_action(Action, Body)} -> % total prolog voodoo!
        % It's a user-defined action!
        % Action unifies with Template, binding variables
        % in Body automatically
        % Body now has the correct bindings, use it directly
        % Note: Body may contain attr() specs, so recurse
        % through top-level execute_action for resolution
        % Replace the user action with its Body in the queue
        execute_action(
            actions_old([Body|Rest]),
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

