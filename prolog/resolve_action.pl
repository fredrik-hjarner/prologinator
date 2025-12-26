% Value Resolution Module
% Resolves attr() references in actions before execution
%
% This module handles resolution of attr() references in
% actions. When a user writes: set_attr(dest, .x)
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
%   Input:  move_to(.target_x, .target_y, 5)
%   Output: move_to(100, 200, 5)
%           (if target_x=100, target_y=200)

% set_attr(Path, ValueExpr) - Path is NOT resolved (it's a
% location), Value IS resolved
resolve_action(
    MyID, set_attr(Path, ValueExpr), set_attr(Path, Value)
) -->
    !,
    resolve_arg(MyID, ValueExpr, Value).

% copy_attr(SrcPath, DestPath) - Neither is resolved (both
% are locations)
resolve_action(
    _MyID, copy_attr(Src, Dest), copy_attr(Src, Dest)
) -->
    !,
    [].

% incr(Path, AmountExpr) - Path is NOT resolved (location),
% Amount IS resolved
resolve_action(
    MyID, incr(Path, AmtExpr), incr(Path, Amt)
) -->
    !,
    resolve_arg(MyID, AmtExpr, Amt).

% decr(Path, AmountExpr) - Path is NOT resolved (location),
% Amount IS resolved
resolve_action(
    MyID, decr(Path, AmtExpr), decr(Path, Amt)
) -->
    !,
    resolve_arg(MyID, AmtExpr, Amt).

% TODO: These prolly don't need to be dcgs because I'm not
%       doing anything with context... but dunno maybe it
%       can be good "just in case"?
% Don't resolve load/1
resolve_action(
    _MyID, load(Path), load(Path)
) -->
    !,
    [].

% Don't resolve wait_until/1
resolve_action(
    _MyID, wait_until(Condition), wait_until(Condition)
) -->
    !,
    [].

resolve_action(
    _MyID,
    attr_if(Condition, ThenActions, ElseActions),
    attr_if(Condition, ThenActions, ElseActions)
) -->
    !,
    [].

resolve_action(
    _MyID,
    spawn(Actions),
    spawn(Actions)
) -->
    !,
    [].

resolve_action(
    _MyID,
    despawn,
    despawn
) -->
    !,
    [].

resolve_action(
    _MyID,
    fork(Actions),
    fork(Actions)
) -->
    !,
    [].

resolve_action(
    MyID,
    wait(Frames),
    wait(ResolvedFrames)
) -->
    !,
    resolve_arg(MyID, Frames, ResolvedFrames).

resolve_action(
    _MyID,
    list(Actions),
    list(Actions)
) -->
    !,
    [].

resolve_action(
    _MyID,
    noop,
    noop
) -->
    !,
    [].

resolve_action(
    MyID,
    move_delta(Frames, DX, DY),
    move_delta(ResolvedFrames, ResolvedDX, ResolvedDY)
) -->
    !,
    resolve_arg(MyID, Frames, ResolvedFrames),
    resolve_arg(MyID, DX, ResolvedDX),
    resolve_arg(MyID, DY, ResolvedDY).

resolve_action(
    MyID,
    move_to(X, Y, Frames),
    move_to(ResolvedX, ResolvedY, ResolvedFrames)
) -->
    !,
    resolve_arg(MyID, X, ResolvedX),
    resolve_arg(MyID, Y, ResolvedY),
    resolve_arg(MyID, Frames, ResolvedFrames).

resolve_action(
    _MyID,
    parallel_all(Actions),
    parallel_all(Actions)
) -->
    !,
    [].

resolve_action(
    _MyID,
    parallel_race(Actions),
    parallel_race(Actions)
) -->
    !,
    [].

resolve_action(
    MyID,
    repeat(Times, Actions),
    repeat(ResolvedTimes, Actions)
) -->
    !,
    resolve_arg(MyID, Times, ResolvedTimes).

% This an an internal repeat (continuation) so all stuff
% should already have been resolved
resolve_action(
    _MyID,
    repeat(Times, A1, A2),
    repeat(Times, A1, A2)
) -->
    !,
    [].

resolve_action(
    _MyID,
    trigger_state_change(Change),
    trigger_state_change(Change)
) -->
    !,
    [].

resolve_action(
    MyID,
    wait_key_down(Key),
    wait_key_down(ResolvedKey)
) -->
    !,
    resolve_arg(MyID, Key, ResolvedKey).

resolve_action(
    MyID,
    wait_key_held(Key),
    wait_key_held(ResolvedKey)
) -->
    !,
    resolve_arg(MyID, Key, ResolvedKey).

resolve_action(
    MyID,
    wait_key_up(Key),
    wait_key_up(ResolvedKey)
) -->
    !,
    resolve_arg(MyID, Key, ResolvedKey).

resolve_action(
    _MyID,
    loop(Actions),
    loop(Actions)
) -->
    !,
    [].

% internal loop (continuation). should not be resolved
% can never be called by user and stuff should already have
% been resolved.
resolve_action(
    _MyID,
    loop(Running, Original),
    loop(Running, Original)
) -->
    !,
    [].

% TODO: It's retarded that `resolve_action` only takes ONE
% actions as it's 2nd argument and not a list of actions.
resolve_action(
    _MyID,
    define_action(Name, Action),
    define_action(Name, Action)
) -->
    !,
    [].

resolve_action(
    _MyID,
    log(Message),
    log(Message)
) -->
    !,
    [].

% base case for builtins. should raise exception because all
% builtins should be resolved.
resolve_action(
    _MyID,
    Action,
    Action
) -->
    {builtin_action(Action)},
    !,
    {format(
        "resolve_action not implemented for: ~w~n",
        [Action]
    )},
    {throw(not_implemented(resolve_action/3))}.

% base case for non-builtin actions, i.e. custom
% user-defined actions. should never be resolved, I think...
% because it's the built-in actions themselves that does the
% resolution.
resolve_action(
    MyID,
    Action,
    Action
) -->
    !,
    [].

% Helper: resolve list of arguments, threading context
resolve_args(_MyID, [], []) --> [].
resolve_args(MyID, [Arg|Rest], [ResArg|ResRest]) -->
    resolve_arg(MyID, Arg, ResArg),
    resolve_args(MyID, Rest, ResRest).

% Resolve individual arguments
%
% If the argument is prefixed with ., it is treated as a
% path reference that must resolve to a value (read
% context).
resolve_arg(MyID, .Path, V) -->
    !,
    (   {ground(Path)} % Only resolve if path is ground
    % Use strict path resolution to get the value
    ->  resolve_path_strict(MyID, Path, V)
    ;   {V = .Path}
    ).


% NEW: Handle default/2 expressions
resolve_arg(MyID, default(ValueExpr, Fallback), V) -->
    !,
    resolve_default(MyID, ValueExpr, Fallback, V).

% Lists need recursive resolution
% TODO: Do I even need this? I don't think I ever need this
%       in current design.
resolve_arg(MyID, List, ResolvedList) -->
    {List = [_|_]},
    !,
    resolve_args(MyID, List, ResolvedList).
resolve_arg(_MyID, [], []) --> [].

% Catch-all?
% Pass through primitives and other terms
resolve_arg(_MyID, Other, Other) --> !, [].

% Helper for default/2 resolution in actions
resolve_default(MyID, .Path, Fallback, V) -->
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
% If ValueExpr is not an .Path, resolve it normally.
% Note: This handles cases like default(10, 0) -> 10
resolve_default(MyID, ValueExpr, _Fallback, V) -->
    !,
    resolve_arg(MyID, ValueExpr, V).

% ==========================================================
% Path Resolution (DCG version: handles x.y.z chains)
% NOTE: resolve_path_strict//3 and resolve_path_to_attr//3
% definitions are now consolidated in
% prolog/conditions/path_resolution.pl to avoid
% discontiguous warnings.
% ==========================================================
