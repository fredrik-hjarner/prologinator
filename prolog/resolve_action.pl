% Value Resolution Module
% Resolves attr() references in actions before execution

% ==========================================================
% Value Resolution
% ==========================================================
% Resolve all value specs in an action
resolve_action(MyID, ActionIn, ActionOut, CtxIn, CtxOut) :-
    ActionIn =.. [Functor|Args],
    resolve_args(MyID, Args, ResolvedArgs, CtxIn, CtxOut),
    ActionOut =.. [Functor|ResolvedArgs].

% Helper: resolve list of arguments, threading context
resolve_args(_MyID, [], [], CtxIn, CtxOut) :-
    CtxOut = CtxIn.
resolve_args(MyID, [Arg|Rest], [ResArg|ResRest],
             CtxIn, CtxOut) :-
    resolve_arg(MyID, Arg, ResArg, CtxIn, CtxMid),
    resolve_args(MyID, Rest, ResRest, CtxMid, CtxOut).

% Resolve individual arguments
resolve_arg(MyID, attr(Path), V, CtxIn, CtxOut) :-
    ground(Path),  % Only resolve if path is ground
    !,
    resolve_path(MyID, Path, V, CtxIn, CtxOut).

% Lists need recursive resolution
resolve_arg(MyID, List, ResolvedList, CtxIn, CtxOut) :-
    List = [_|_],
    !,
    resolve_args(MyID, List, ResolvedList, CtxIn, CtxOut).
resolve_arg(_MyID, [], [], CtxIn, CtxOut) :-
    CtxOut = CtxIn.

% Pass through primitives and other terms
resolve_arg(_MyID, Other, Other, CtxIn, CtxOut) :-
    CtxOut = CtxIn.

% ==========================================================
% Path Resolution (handles x/y/z chains)
% ==========================================================
% Simple attribute (no path)
resolve_path(MyID, AttrName, Value, CtxIn, CtxOut) :-
    atom(AttrName),  % Not a / term
    !,
    ctx_attr_val(MyID/AttrName, Value, CtxIn, CtxIn),
    CtxOut = CtxIn.

% Path with / separator (handles nested / like a/b/c)
resolve_path(
    MyID, FirstAttr/RestPath, Value, CtxIn, CtxOut
) :-
    !,
    % FirstAttr might itself be a path (like a/b),
    % so resolve it
    resolve_path(MyID, FirstAttr, NextID, CtxIn, CtxMid),
    % Continue from that object (RestPath may be nested)
    resolve_path(NextID, RestPath, Value, CtxMid, CtxOut).

