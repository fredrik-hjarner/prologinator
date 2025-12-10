% Value Resolution Module
% Resolves attr() references in actions before execution

:- module(resolve_action, [resolve_action/4]).

:- use_module(library(lists), [maplist/3]).
:- use_module('./types/adv_accessors', [ctx_attr_val/3]).

% ==========================================================
% Value Resolution
% ==========================================================
% Resolve all value specs in an action
resolve_action(Ctx, MyID, ActionIn, ActionOut) :-
    ActionIn =.. [Functor|Args],
    maplist(resolve_arg(Ctx, MyID), Args,
            ResolvedArgs),
    ActionOut =.. [Functor|ResolvedArgs].

% Resolve individual arguments
resolve_arg(Ctx, MyID, attr(Path), V) :-
    ground(Path),  % Only resolve if path is ground
    !,
    resolve_path(Ctx, MyID, Path, V).

% Lists need recursive resolution
resolve_arg(Ctx, MyID, List, ResolvedList) :-
    List = [_|_], !,
    maplist(resolve_arg(Ctx, MyID), List,
            ResolvedList).
resolve_arg(_Ctx, _MyID, [], []).

% Pass through primitives and other terms
resolve_arg(_Ctx, _MyID, Other, Other).

% ==========================================================
% Path Resolution (handles x/y/z chains)
% ==========================================================
% Simple attribute (no path)
resolve_path(Ctx, MyID, AttrName, Value) :-
    atom(AttrName),  % Not a / term
    !,
    ctx_attr_val(Ctx, MyID/AttrName, Value).

% Path with / separator (handles nested / like a/b/c)
resolve_path(Ctx, MyID, FirstAttr/RestPath, Value) :-
    !,
    % FirstAttr might itself be a path (like a/b),
    % so resolve it
    resolve_path(Ctx, MyID, FirstAttr, NextID),
    % Continue from that object (RestPath may be nested)
    resolve_path(Ctx, NextID, RestPath, Value).

