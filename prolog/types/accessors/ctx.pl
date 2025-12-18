% Context Accessors Module
% Provides getter and setter predicates for context data
% structures
%
% This module centralizes all field access patterns to
% ensure consistent access to nested structures and make
% refactoring easier.

% Old convention (deprecated):
% For getters the functors start with the "thing" we get
% from, then comes _ followed by all "stuff" we want to get.
% The fields come in the order in which they occur on the
% "thing"! So try to keep that!
% For setters it's the same, except we add _ followed by the
% "thing" in the end.
% Rationale for covention:
% The functor immediately tells you what to put where.

% New convention (what we're refactoring towards):
% The setters should have CtxIn and CtxOut as last arguments
% so that they can be better used in DCGs.
% The new naming convention is to start the predicate name
% with the entity then followed by all the fields to be set
% on it such as ctx_set_frame, ctx_set_objs_attrs.
% Single fields setters don't use dcg, but bulk setters do
% use dcg.

% ==========================================================
%
%   Context Accessors
%
% ==========================================================

% ==========================================================
% Context Getters
% ==========================================================

% ----------------------------------------------------------
% Single Field Getters
% ----------------------------------------------------------

% getter ctx_frame/3 for dcg use
ctx_frame(F, Ctx, Ctx) :-
    Ctx = ctx(state(frame(F), _, _, _, _, _, _), _).

% getter ctx_objs/3 for dcg use
ctx_objs(Objects, Ctx, Ctx) :-
    Ctx = ctx(state(_, objects(Objects), _, _, _, _, _), _).

% getter ctx_attrs/3 for dcg use
ctx_attrs(Attrs, Ctx, Ctx) :-
    Ctx = ctx(state(_, _, attrs(Attrs), _, _, _, _), _).

% getter ctx_state/3 for dcg use
ctx_state(State, Ctx, Ctx) :-
    Ctx = ctx(State, _).

% getter ctx_cmds/3 for dcg use
ctx_cmds(Commands, Ctx, Ctx) :-
    Ctx = ctx(state(_, _, _, _, _,
                    commands(Commands), _), _).

% getter ctx_status/3 for dcg use
ctx_status(Status, Ctx, Ctx) :-
    Ctx = ctx(state(_, _, _, status(Status), _, _, _), _).

% getter ctx_nextid/3 for dcg use
ctx_nextid(NID, Ctx, Ctx) :-
    Ctx = ctx(state(_, _, _, _, next_id(NID), _, _), _).

% getter ctx_actionstore/3 for dcg use
ctx_actionstore(ActionStore, Ctx, Ctx) :-
    Ctx = ctx(state(_, _, _, _, _, _,
                    actionstore(ActionStore)), _).

% getter ctx_input/3 for dcg use
ctx_input(Input, Ctx, Ctx) :-
    Ctx = ctx(_, Input).

% TODO: rename to inputevents?
% getter ctx_events/3 for dcg use
ctx_events(E, Ctx, Ctx) :-
    Ctx = ctx(_, input(events(E), _)).

% Get held keys from context
% getter ctx_held/3 for dcg use
ctx_held(H, Ctx, Ctx) :-
    Ctx = ctx(_, input(_, held(H))).

% ----------------------------------------------------------
% Bulk Getters
% ----------------------------------------------------------

% bulk getter ctx_objs_cmds//2 for dcg use
ctx_objs_cmds(Objs, Cmds) -->
    ctx_objs(Objs),
    ctx_cmds(Cmds).

% bulk getter ctx_objs_attrs//2 for dcg use
ctx_objs_attrs(Objs, Attrs) -->
    ctx_objs(Objs),
    ctx_attrs(Attrs).

% bulk getter ctx_status_cmds//2 for dcg use
ctx_status_cmds(Status, Cmds) -->
    ctx_status(Status),
    ctx_cmds(Cmds).

% bulk getter ctx_objs_nextid_cmds//3 for dcg use
ctx_objs_nextid_cmds(Objs, NextID, Cmds) -->
    ctx_objs(Objs),
    ctx_nextid(NextID),
    ctx_cmds(Cmds).

% ==========================================================
% Context Setters
% ==========================================================

% ----------------------------------------------------------
% Single Field Setters
% ----------------------------------------------------------

% Set frame in context
ctx_set_frame(
    NewFrame,
    ctx(state(_, Objects, Attrs, Status, NextID,
              Commands, ActionStore), Input),
    ctx(state(frame(NewFrame), Objects, Attrs, Status,
              NextID, Commands, ActionStore), Input)
).

% Set objects in context
ctx_set_objs(
    NewObjects,
    ctx(state(F, _, Attrs, Status, NextID, Commands,
              ActionStore),
        Input),
    ctx(state(F, objects(NewObjects), Attrs, Status,
              NextID, Commands, ActionStore), Input)
).

% Set attributes in context
ctx_set_attrs(
    NewAttrs,
    ctx(state(F, Objects, _, Status, NextID, Commands,
              ActionStore),
        Input),
    ctx(state(F, Objects, attrs(NewAttrs), Status,
              NextID, Commands, ActionStore), Input)
).

% Set state in context
ctx_set_state(
    NewState,
    ctx(_, Input),
    ctx(NewState, Input)
).

% Set commands in context
ctx_set_cmds(
    NewCommands,
    ctx(state(F, Objects, Attrs, Status, NextID, _,
              ActionStore),
        Input),
    ctx(state(F, Objects, Attrs, Status, NextID,
              commands(NewCommands), ActionStore), Input)
).

% Set status in context
ctx_set_status(
    NewStatus,
    ctx(state(F, Objects, Attrs, _, NextID, Commands,
              ActionStore),
        Input),
    ctx(state(F, Objects, Attrs, status(NewStatus),
              NextID, Commands, ActionStore), Input)
).

% Set next_id in context
ctx_set_nextid(
    NewNID,
    ctx(state(F, Objs, Attrs, Status, _, Cmds,
              ActionStore), Input),
    ctx(state(F, Objs, Attrs, Status, next_id(NewNID),
              Cmds, ActionStore), Input)
).

% Set actionstore in context
ctx_set_actionstore(
    NewActionStore,
    ctx(state(F, Objects, Attrs, Status, NextID,
              Commands, _),
        Input),
    ctx(state(F, Objects, Attrs, Status, NextID,
              Commands, actionstore(NewActionStore)), Input)
).

% Set input in context
ctx_set_input(
    NewInput,
    ctx(State, _),
    ctx(State, NewInput)
).

% ----------------------------------------------------------
% Bulk Setters (CtxOld, CtxNew as hidden last arguments)
% ----------------------------------------------------------

ctx_set_objs_cmds(Objs, Cmds) -->
    ctx_set_objs(Objs),
    ctx_set_cmds(Cmds).

ctx_set_objs_attrs(Objs, Attrs) -->
    ctx_set_objs(Objs),
    ctx_set_attrs(Attrs).

ctx_set_status_cmds(Status, Cmds) -->
    ctx_set_status(Status),
    ctx_set_cmds(Cmds).

ctx_set_nextid_cmds(NextID, Cmds) -->
    ctx_set_nextid(NextID),
    ctx_set_cmds(Cmds).

ctx_set_objs_nextid_cmds(Objs, NextID, Cmds) -->
    ctx_set_objs(Objs),
    ctx_set_nextid(NextID),
    ctx_set_cmds(Cmds).