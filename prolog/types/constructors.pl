% Constructors Module
% Provides constructors for common empty/default structures.
% Useful for shortening test code substantially.

:- module(constructors, [
    empty_ctx/1,
    ctx_with_attrs/2,
    empty_attr_store/1
]).

:- use_module(library(assoc), [empty_assoc/1]).
:- use_module('./accessors').

% ==========================================================
% Context Constructors
% ==========================================================

% Creates an empty context with default values:
% - frame(0)
% - objects([])
% - attrs(empty assoc)
% - status(playing)
% - next_id(1)
% - commands([])
% - rev_hints([])
empty_ctx(ctx(state(
    frame(0),
    objects([]),
    attrs(EmptyAttrs),
    status(playing),
    next_id(1),
    commands([]),
    rev_hints([])
))) :-
    empty_assoc(EmptyAttrs).

% Creates a context with a provided attribute store.
% All other fields use defaults (same as empty_ctx/1).
ctx_with_attrs(Attrs, ctx(state(
    frame(0),
    objects([]),
    attrs(Attrs),
    status(playing),
    next_id(1),
    commands([]),
    rev_hints([])
))).

% ==========================================================
% Attribute Store Constructors
% ==========================================================

% Alias for empty_assoc/1 for consistency.
empty_attr_store(EmptyAttrs) :-
    empty_assoc(EmptyAttrs).
