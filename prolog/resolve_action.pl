% Value Resolution Module
% Resolves attr() references in actions before execution
%
% This module handles resolution of attr(Path) references in
% actions. When a user writes: set_attr(dest, attr(x))
% The attr(x) gets resolved to the actual value (e.g., 42)
% before the action executes.

% ==========================================================
% Value Resolution
% ==========================================================
% Resolve all value specs in an action
%
% Example:
%   Input:  set_attr(dest, attr(x))
%   Output: set_attr(dest, 42)  (if x = 42)
%
%   Input:  move_to(attr(target_x), attr(target_y), 5)
%   Output: move_to(100, 200, 5)
%           (if target_x=100, target_y=200)
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
%
% Example:
%   Input:  attr(x) -> Output: 42 (value of x)
%   Input:  attr(parent_id/x) -> Output: 100
%           (value of x on parent)
%   Input:  "hello" -> Output: "hello" (passed through)
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
% Resolves a path to a VALUE by navigating through object
% IDs.
%
% Examples (assuming object 1 has parent_id=2,
%   object 2 has x=100):
%   resolve_path(1, x, Value) 
%     -> Value = <value of x on object 1>
%
%   resolve_path(1, parent_id/x, Value)
%     -> Step 1: resolve_path(1, parent_id, NextID)
%        -> NextID = 2
%     -> Step 2: resolve_path(2, x, Value) -> Value = 100
%
%   resolve_path(1, parent_id/target_y/z, Value)
%     -> Navigates: 1 -> parent_id(2) -> target_y -> z
%     -> Returns the final value

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

% ==========================================================
% Path Resolution to ObjectID/Key (for attribute operations)
% ==========================================================
% Resolves a path to an ObjectID/Key PAIR instead of a
% value.
% Used by actions like set_attr, copy_attr, incr, decr that
% need to operate on attributes via paths.
%
% Examples (assuming object 1 has parent_id=2):
%   resolve_path_to_attr(1, x, Pair)
%     -> Pair = 1/x  (object 1, attribute x)
%
%   resolve_path_to_attr(1, parent_id/x, Pair)
%     -> Step 1: resolve_path(1, parent_id, NextID)
%        -> NextID = 2
%     -> Step 2: resolve_path_to_attr(2, x, Pair)
%        -> Pair = 2/x
%     -> Returns: 2/x  (object 2, attribute x)
%
%   resolve_path_to_attr(1, parent_id/target_y, Pair)
%     -> Navigates to parent object, returns: 2/target_y
%
% Usage in set_attr:
%   set_attr(parent_id/x, 100) 
%     -> resolve_path_to_attr gets 2/x
%     -> ctx_set_attr_val(2/x, 100) sets x=100 on object 2

% Simple attribute (no path) - returns current object ID
% and key
resolve_path_to_attr(MyID, AttrName, MyID/AttrName) -->
    {atom(AttrName)},  % Not a / term
    !.

% Path with / separator - navigate to final object, return
%   final ObjectID/Key
resolve_path_to_attr(MyID,
                     FirstAttr/RestPath,
                     FinalID/Key) -->
    !,
    % Navigate through FirstAttr to get NextID
    resolve_path(MyID, FirstAttr, NextID),
    % Continue resolving RestPath
    resolve_path_to_attr(NextID, RestPath, FinalID/Key).

