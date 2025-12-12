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
% execute_action/5
% ==========================================================

% Forward execution (normal game)
% :- pred execute_action(+Action, +ObjIn, -ObjOut,
%     -Commands, -RevHints) 
%     :: (action(Action), game_object(ObjIn)) 
%     => (action(Action), game_object(ObjOut),
%     list(command, Commands), list(rev_hint, RevHints))
%     + is_det.

% Reverse execution (undo/replay)
% :- pred execute_action(-Action, -ObjIn, +ObjOut,
%     +Commands, +RevHints) + is_det.

% Action inference - which must the action have been?
% :- pred execute_action(-Action, +ObjIn, +ObjOut,
%     +Commands, +RevHints).

% Validation (consistency check)
% :- pred execute_action(+Action, +ObjIn, +ObjOut,
%     +Commands, +RevHints).

% ==========================================================
% Top-level execute_action (with resolution)
% ==========================================================
execute_action(
    ctx_old(CtxOld),
    ctx_new(CtxNew),
    action(Action),
    obj_old(ObjIn),
    result(Status, ObjOut)
) :-
    % 1. Resolve value specs in action
    obj_id(ObjIn, MyID),
    resolve_action(CtxOld, MyID, Action, ResolvedAction),
    
    % 2. Delegate to existing logic (renamed)
    execute_action_resolved(
        ctx_old(CtxOld),
        ctx_new(CtxNew),
        action(ResolvedAction),
        obj_old(ObjIn),
        result(Status, ObjOut)
    ).

% ==========================================================
% RENAMED: execute_action → execute_action_resolved
% ==========================================================
% Wrapper: validates action then delegates to implementation
% Now threads Context directly to accumulate side effects.
% Also handles user-defined actions via runtime expansion.
execute_action_resolved(
    ctx_old(CtxOld),
    ctx_new(CtxNew),
    action(Action),
    obj_old(ObjIn),
    result(Status, ObjOut)
) :-
    % TODO: action_validation nowadays it almost useless
    %       since custom actions allow anything.
    %       so maybe I should validate builtins separately?
    action_validation(Action),
    % validate(Action, action_schema),
    ( builtin_action(Action) ->
        % It's a built-in action - execute normally
        execute_action_impl(
            ctx_old(CtxOld),
            ctx_new(CtxNew),
            action(Action),
            obj_old(ObjIn),
            result(Status, ObjOut)
        )
    ; user_action(Action, Body) -> % absolute prolog voodoo!
        % It's a user-defined action!
        % Action unifies with Template, binding variables
        % in Body automatically
        % Body now has the correct bindings, use it directly
        % Note: Body may contain attr() specs, so recurse
        % through top-level execute_action for resolution
        execute_action(
            ctx_old(CtxOld),
            ctx_new(CtxNew),
            action(Body),
            obj_old(ObjIn),
            result(Status, ObjOut)
        )
    ;
        % Unknown action
        throw(unknown_action(Action))
    ).

% ==========================================================
% execute_action_impl/5
% ==========================================================
% Internal implementation (no validation)
% Implementation clauses are provided by individual action
%   files in ./actions/ directory (one file per action)
% ==========================================================

