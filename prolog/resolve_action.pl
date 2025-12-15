% Value Resolution Module
% Resolves attr() references in actions before execution

% ==========================================================
% Value Resolution
% ==========================================================
% Resolve all value specs in an action
resolve_action(MyID, ActionIn, ActionOut) -->
    {ActionIn =.. [Functor|Args]},
    resolve_args(MyID, Args, ResolvedArgs),
    {ActionOut =.. [Functor|ResolvedArgs]}.

% Helper: resolve list of arguments, threading context
resolve_args(_MyID, [], []) --> [].
resolve_args(MyID, [Arg|Rest], [ResArg|ResRest]) -->
    resolve_arg(MyID, Arg, ResArg),
    resolve_args(MyID, Rest, ResRest).

% Resolve individual arguments
resolve_arg(MyID, attr(Path), V) -->
    {ground(Path)},  % Only resolve if path is ground
    !,
    resolve_path(MyID, Path, V).

% Lists need recursive resolution
resolve_arg(MyID, List, ResolvedList) -->
    {List = [_|_]},
    !,
    resolve_args(MyID, List, ResolvedList).
resolve_arg(_MyID, [], []) --> [].

% Pass through primitives and other terms
resolve_arg(_MyID, Other, Other) --> [].

% ==========================================================
% Path Resolution (handles x/y/z chains)
% ==========================================================
% Simple attribute (no path)
resolve_path(MyID, AttrName, Value) -->
    {atom(AttrName)},  % Not a / term
    !,
    ctx_attr_val(MyID/AttrName, Value).

% Path with / separator (handles nested / like a/b/c)
resolve_path(MyID, FirstAttr/RestPath, Value) -->
    !,
    % FirstAttr might itself be a path (like a/b),
    % so resolve it
    resolve_path(MyID, FirstAttr, NextID),
    % Continue from that object (RestPath may be nested)
    resolve_path(NextID, RestPath, Value).

