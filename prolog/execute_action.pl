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
% execute_action
% ==========================================================
% Wrapper: validates action then delegates to implementation
% Now threads Context directly to accumulate side effects.
% Also handles user-defined actions via runtime expansion.
execute_action(
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
    % {findall(
    %     Head,
    %     builtin_action(Head),
    %     Heads
    % )},
    % {pretty_print(Heads)},
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

