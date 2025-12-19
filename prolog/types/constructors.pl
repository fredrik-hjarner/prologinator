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
% - actionstore(empty assoc)
% - input(events([]), held([]))
empty_ctx(ctx(state(
    frame(0),
    objects([]),
    attrs(EmptyAttrs),
    status(playing),
    next_id(1),
    commands(spawn_cmds([]), fork_cmds([])),
    actionstore(EmptyActionStore)
), input(events([]), held([])))) :-
    empty_assoc(EmptyAttrs),
    empty_assoc(EmptyActionStore).

% Creates a context with a provided attribute store.
% All other fields use defaults (same as empty_ctx/1).
ctx_with_attrs(Attrs, ctx(state(
    frame(0),
    objects([]),
    attrs(Attrs),
    status(playing),
    next_id(1),
    commands(spawn_cmds([]), fork_cmds([])),
    actionstore(EmptyActionStore)
), input(events([]), held([])))) :-
    empty_assoc(EmptyActionStore).

% Creates a context with provided frame and attribute store.
% All other fields use defaults.
ctx_with_frame_attrs(Frame, Attrs, ctx(state(
    frame(Frame),
    objects([]),
    attrs(Attrs),
    status(playing),
    next_id(1),
    commands(spawn_cmds([]), fork_cmds([])),
    actionstore(EmptyActionStore)
), input(events([]), held([])))) :-
    empty_assoc(EmptyActionStore).

% ==========================================================
% Context Constructors with Input
% ==========================================================

% Creates a context with custom input (events and/or held
% keys). All other fields use defaults.
ctx_with_inputevents_inputheld(Events, Held, ctx(state(
    frame(0),
    objects([]),
    attrs(EmptyAttrs),
    status(playing),
    next_id(1),
    commands(spawn_cmds([]), fork_cmds([])),
    actionstore(EmptyActionStore)
), input(events(Events), held(Held)))) :-
    empty_assoc(EmptyAttrs),
    empty_assoc(EmptyActionStore).

% Creates a context with objects. All other fields use
% defaults.
ctx_with_objs(Objects, ctx(state(
    frame(0),
    objects(Objects),
    attrs(EmptyAttrs),
    status(playing),
    next_id(1),
    commands(spawn_cmds([]), fork_cmds([])),
    actionstore(EmptyActionStore)
), input(events([]), held([])))) :-
    empty_assoc(EmptyAttrs),
    empty_assoc(EmptyActionStore).

% Creates a context with objects and input (events and/or
% held keys). All other fields use defaults.
ctx_with_objs_input(Objects, Events, Held, ctx(state(
    frame(0),
    objects(Objects),
    attrs(EmptyAttrs),
    status(playing),
    next_id(1),
    commands(spawn_cmds([]), fork_cmds([])),
    actionstore(EmptyActionStore)
), input(events(Events), held(Held)))) :-
    empty_assoc(EmptyAttrs),
    empty_assoc(EmptyActionStore).

% Creates a context with frame, objects, and input (events
% and/or held keys). All other fields use defaults.
ctx_with_frame_objs_input(Frame, Objects, Events, Held,
    ctx(state(
        frame(Frame),
        objects(Objects),
        attrs(EmptyAttrs),
        status(playing),
        next_id(1),
        commands(spawn_cmds([]), fork_cmds([])),
        actionstore(EmptyActionStore)
    ), input(events(Events), held(Held)))) :-
    empty_assoc(EmptyAttrs),
    empty_assoc(EmptyActionStore).

% ==========================================================
% Attribute Store Constructors
% ==========================================================

% Alias for empty_assoc/1 for consistency.
empty_attr_store(EmptyAttrs) :-
    empty_assoc(EmptyAttrs).
