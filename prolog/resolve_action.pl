% Value Resolution Module
% Resolves attr() references in actions before execution
%
% This module handles resolution of attr() references in
% actions. When a user writes: set_attr(dest, :x)
% The attr(x) gets resolved to the actual value (e.g., 42)
% before the action executes.

% ==========================================================
% Value Resolution
% ==========================================================
% Resolve all value specs in an action
%
% Example:
%   Input:  set_attr(Path, ValueExpr)
%   Output: set_attr(Path, 42)  (if x = 42)
%
%   Input:  move_to(:target_x, :target_y, 5)
%   Output: move_to(100, 200, 5)
%           (if target_x=100, target_y=200)

% Helper: resolve list of arguments, threading context
% resolve_args(_MyID, [], []) --> [].
% resolve_args(MyID, [Arg|Rest], [ResArg|ResRest]) -->
%     resolve_arg(MyID, Arg, ResArg),
%     resolve_args(MyID, Rest, ResRest).

% Resolve individual arguments
%
% If the argument is prefixed with ., it is treated as a
% path reference that must resolve to a value (read
% context).
resolve_arg(MyID, :Path, V) -->
    !,
    (   {ground(Path)} % Only resolve if path is ground
    % Use strict path resolution to get the value
    ->  resolve_path_strict(MyID, Path, V)
    ;   {V = :Path}
    ).


% NEW: Handle default/2 expressions
resolve_arg(MyID, default(ValueExpr, Fallback), V) -->
    !,
    resolve_default(MyID, ValueExpr, Fallback, V).

% Lists need recursive resolution
% TODO: Do I even need this? I don't think I ever need this
%       in current design.
% resolve_arg(MyID, List, ResolvedList) -->
%     {List = [_|_]},
%     !,
%     resolve_args(MyID, List, ResolvedList).
% resolve_arg(_MyID, [], []) --> [].

% Catch-all?
% Pass through primitives and other terms
resolve_arg(_MyID, Other, Other) --> !, [].

% Helper for default/2 resolution in actions
resolve_default(MyID, :Path, Fallback, V) -->
    {ground(Path)},
    !,
    ( resolve_path_strict(MyID, Path, ResolvedValue) ->
        {V = ResolvedValue}
    ;
        % If strict path resolution fails (due to missing
        % attribute),
        % use the fallback. Context remains unchanged.
        {V = Fallback}
    ).

% Catch-all?
% If ValueExpr is not an :Path, resolve it normally.
% Note: This handles cases like default(10, 0) -> 10
resolve_default(MyID, ValueExpr, _Fallback, V) -->
    !,
    resolve_arg(MyID, ValueExpr, V).

% ==========================================================
% Path Resolution (DCG version: handles x:y:z chains)
% NOTE: resolve_path_strict//3 and resolve_path_to_attr//3
% definitions are now consolidated in
% prolog/conditions/path_resolution.pl to avoid
% discontiguous warnings.
% ==========================================================
