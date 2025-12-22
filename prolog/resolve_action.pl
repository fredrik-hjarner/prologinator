% Value Resolution Module
% Resolves attr() references in actions before execution
%
% This module handles resolution of attr(Path) references in
% actions. When a user writes: set_attr(dest, @x)
% The attr(x) gets resolved to the actual value (e.g., 42)
% before the action executes.

% ==========================================================
% Value Resolution
% ==========================================================
% Resolve all value specs in an action
%
% Example:
%   Input:  set_attr(dest, @x)
%   Output: set_attr(dest, 42)  (if x = 42)
%
%   Input:  move_to(@target_x, @target_y, 5)
%   Output: move_to(100, 200, 5)
%           (if target_x=100, target_y=200)

% Signature-aware resolution to protect Path positions
% (L-values)

% set_attr(Path, ValueExpr) - Path is NOT resolved, Value
% IS resolved
resolve_action(
    MyID, set_attr(Path, ValueExpr), set_attr(Path, Value)
) -->
    !,
    resolve_arg(MyID, ValueExpr, Value).

% copy_attr(SrcPath, DestPath) - Neither is resolved
resolve_action(
    _MyID, copy_attr(Src, Dest), copy_attr(Src, Dest)
) -->
    !,
    [].

% incr(Path, AmountExpr) - Path is NOT resolved, Amount IS
% resolved
resolve_action(
    MyID, incr(Path, AmtExpr), incr(Path, Amt)
) -->
    !,
    resolve_arg(MyID, AmtExpr, Amt).

% decr(Path, AmountExpr) - Path is NOT resolved, Amount IS
% resolved
resolve_action(
    MyID, decr(Path, AmtExpr), decr(Path, Amt)
) -->
    !,
    resolve_arg(MyID, AmtExpr, Amt).

% Default: resolve all arguments (for move_to, wait, etc.)
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
%   Input:  @x -> Output: 42 (value of x)
%   Input:  @parent_id@x -> Output: 100
%           (value of x on parent)
%   Input:  "hello" -> Output: "hello" (passed through)
% TODO: The way I lint single files does not work here
%       since the @ operator is not defined so it fails.
resolve_arg(MyID, @Path, V) -->
    {ground(Path)},  % Only resolve if path is ground
    !,
    resolve_path_strict(MyID, Path, V).

% NEW: Handle default/2 expressions
resolve_arg(MyID, default(ValueExpr, Fallback), V) -->
    !,
    resolve_default(MyID, ValueExpr, Fallback, V).

% Lists need recursive resolution
resolve_arg(MyID, List, ResolvedList) -->
    {List = [_|_]},
    !,
    resolve_args(MyID, List, ResolvedList).
resolve_arg(_MyID, [], []) --> [].

% Pass through primitives and other terms
resolve_arg(_MyID, Other, Other) --> [].

% Helper for default/2 resolution in actions
resolve_default(MyID, @Path, Fallback, V) -->
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
% If ValueExpr is not an @Path, resolve it normally.
% Note: This handles cases like default(10, 0) -> 10
resolve_default(MyID, ValueExpr, _Fallback, V) -->
    resolve_arg(MyID, ValueExpr, V).

% ==========================================================
% Path Resolution (DCG version: handles x@y@z chains)
% ==========================================================
% Resolves a path to a VALUE by navigating through object
% IDs.
%
% Examples (assuming object 1 has parent_id=2,
%   object 2 has x=100):
%   resolve_path_strict(1, x, Value) 
%     -> Value = <value of x on object 1>
%
%   resolve_path_strict(1, parent_id@x, Value)
%     -> Step 1: resolve_path_strict(1, parent_id, NextID)
%        -> NextID = 2
%     -> Step 2: resolve_path_strict(2,x,Value) -> Value=100
%
%   resolve_path_strict(1, parent_id@target_y@z, Value)
%     -> Navigates: 1 -> parent_id(2) -> target_y -> z
%     -> Returns the final value

% Simple attribute (no path)
resolve_path_strict(MyID, AttrName, Value) -->
    {atom(AttrName)},  % Not a @ term
    !,
    ctx_attr_val(MyID/AttrName, Value).

% Path with @ separator (handles nested @ like a@b@c)
resolve_path_strict(MyID, FirstAttr@RestPath, Value) -->
    !,
    % FirstAttr might itself be a path (like a@b),
    % so resolve it
    resolve_path_strict(MyID, FirstAttr, NextID),
    % Continue from that object (RestPath may be nested)
    resolve_path_strict(NextID, RestPath, Value).

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
%   resolve_path_to_attr(1, parent_id@x, Pair)
%     -> Step 1: resolve_path_strict(1, parent_id, NextID)
%        -> NextID = 2
%     -> Step 2: resolve_path_to_attr(2, x, Pair)
%        -> Pair = 2/x
%     -> Returns: 2/x  (object 2, attribute x)
%
%   resolve_path_to_attr(1, parent_id@target_y, Pair)
%     -> Navigates to parent object, returns: 2/target_y
%
% Usage in set_attr:
%   set_attr(parent_id@x, 100) 
%     -> resolve_path_to_attr gets 2/x
%     -> ctx_set_attr_val(2/x, 100) sets x=100 on object 2

% Simple attribute (no path) - returns current object ID
% and key
resolve_path_to_attr(MyID, AttrName, MyID/AttrName) -->
    {atom(AttrName)},  % Not a @ term
    !.

% Path with @ separator - navigate to final object, return
%   final ObjectID/Key
resolve_path_to_attr(MyID,
                     FirstAttr@RestPath,
                     FinalID/Key) -->
    !,
    % Navigate through FirstAttr to get NextID
    resolve_path_strict(MyID, FirstAttr, NextID),
    % Continue resolving RestPath
    resolve_path_to_attr(NextID, RestPath, FinalID/Key).
