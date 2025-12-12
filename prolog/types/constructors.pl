% Constructors Module
% Provides constructors for common empty/default structures.
% Useful for shortening test code substantially.

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
% - input(events([]), held([]))
empty_ctx(ctx(state(
    frame(0),
    objects([]),
    attrs(EmptyAttrs),
    status(playing),
    next_id(1),
    commands([])
), input(events([]), held([])))) :-
    empty_assoc(EmptyAttrs).

% Creates a context with a provided attribute store.
% All other fields use defaults (same as empty_ctx/1).
ctx_with_attrs(Attrs, ctx(state(
    frame(0),
    objects([]),
    attrs(Attrs),
    status(playing),
    next_id(1),
    commands([])
), input(events([]), held([])))).

% Creates a context with provided frame and attribute store.
% All other fields use defaults.
ctx_with_frame_attrs(Frame, Attrs, ctx(state(
    frame(Frame),
    objects([]),
    attrs(Attrs),
    status(playing),
    next_id(1),
    commands([])
), input(events([]), held([])))).

% ==========================================================
% Attribute Store Constructors
% ==========================================================

% Alias for empty_assoc/1 for consistency.
empty_attr_store(EmptyAttrs) :-
    empty_assoc(EmptyAttrs).
