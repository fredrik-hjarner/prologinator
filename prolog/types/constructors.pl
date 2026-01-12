% Constructors Module
% Provides constructors for common empty/default structures.
% Useful for shortening test code substantially.

% ==========================================================
% Context Constructors
% ==========================================================

% Empty context with all default values:
% - frame(0)
% - objects([])
% - attrs(empty assoc)
% - status(playing)
% - next_id(1)
% - commands(spawn_cmds([]), fork_cmds([]))
% - actionstore(empty assoc)
% - rng_index(0)
% - input(events([]), held([]))
empty_ctx(ctx(state(
    frame(0),
    objects([]),
    attrs(EmptyAttrs),
    status(playing),
    next_id(1),
    commands(spawn_cmds([]), fork_cmds([])),
    actionstore(EmptyActionStore),
    rng_index(0)
), input(events([]), held([])))) :-
    empty_assoc(EmptyAttrs),
    empty_assoc(EmptyActionStore).

% Creates a context with a provided attribute store.
% All other fields use defaults.
ctx_with_attrs(Attrs, Ctx) :-
    empty_ctx(Def),
    ctx_set_attrs(Attrs, Def, Ctx).

% Creates a context with provided frame and attribute store.
% All other fields use defaults.
ctx_with_frame_attrs(Frame, Attrs, Ctx) :-
    empty_ctx(Def),
    ctx_set_frame(Frame, Def, Ctx1),
    ctx_set_attrs(Attrs, Ctx1, Ctx).

% Creates a context with custom input (events and/or held
% keys). All other fields use defaults.
ctx_with_inputevents_inputheld(Events, Held, Ctx) :-
    empty_ctx(Def),
    ctx_set_input(input(events(Events), held(Held)), Def,
                  Ctx).

% Creates a context with objects. All other fields use
% defaults.
ctx_with_objs(Objects, Ctx) :-
    empty_ctx(Def),
    ctx_set_objs(Objects, Def, Ctx).

% Creates a context with objects and input (events and/or
% held keys). All other fields use defaults.
ctx_with_objs_input(Objects, Events, Held, Ctx) :-
    empty_ctx(Def),
    ctx_set_objs(Objects, Def, Ctx1),
    ctx_set_input(input(events(Events), held(Held)), Ctx1,
                  Ctx).

% Creates a context with frame, objects, and input (events
% and/or held keys). All other fields use defaults.
ctx_with_frame_objs_input(Frame, Objects, Events, Held,
                          Ctx) :-
    empty_ctx(Def),
    ctx_set_frame(Frame, Def, Ctx1),
    ctx_set_objs(Objects, Ctx1, Ctx2),
    ctx_set_input(input(events(Events), held(Held)), Ctx2,
                  Ctx).

% ==========================================================
% Attribute Store Constructors
% ==========================================================

% Alias for empty_assoc/1 for consistency.
empty_attr_store(EmptyAttrs) :-
    empty_assoc(EmptyAttrs).
