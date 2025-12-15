% Accessors Module
% Provides getter and setter predicates for game data
% structures
%
% This module centralizes all field access patterns to
% ensure consistent access to nested structures and make
% refactoring easier.

% Conventions:
% For getters the functors start with the "thing" we get
% from, then comes _ followed by all "stuff" we want to get.
% The fields come in the order in which they occur on the
% "thing"! So try to keep that!
% For setters it's the same, except we add _ followed by the
% "thing" in the end.
% Rationale for covention:
% The functor immediately tells you what to put where.

% ==========================================================
% Context Accessors
% ==========================================================

% Get frame from context
ctx_frame(
    ctx(state(frame(F), _, _, _, _, _), _), F
).

% Set frame in context
ctx_frame_ctx(
    ctx(state(_, Objects, Attrs, Status, NextID,
              Commands), Input),
    NewFrame,
    ctx(state(frame(NewFrame), Objects, Attrs, Status,
              NextID, Commands), Input)
).

% Get objects from context
ctx_objs(
    ctx(state(_, objects(Objects), _, _, _, _), _),
    Objects
).

% Set objects in context
ctx_objs_ctx(
    ctx(state(F, _, Attrs, Status, NextID, Commands),
        Input),
    NewObjects,
    ctx(state(F, objects(NewObjects), Attrs, Status,
              NextID, Commands), Input)
).

% Get attributes from context
ctx_attrs(
    ctx(state(_, _, attrs(Attrs), _, _, _), _), Attrs
).

% Set attributes in context
ctx_attrs_ctx(
    ctx(state(F, Objects, _, Status, NextID, Commands),
        Input),
    NewAttrs,
    ctx(state(F, Objects, attrs(NewAttrs), Status,
              NextID, Commands), Input)
).

% Get state from context
ctx_state(
    ctx(State, _), State
).

% Set state in context
ctx_state_ctx(
    ctx(_, Input), NewState, ctx(NewState, Input)
).

% Get commands from context
ctx_cmds(
    ctx(state(_, _, _, _, _, commands(Commands)), _),
    Commands
).

% Set commands in context
ctx_cmds_ctx(
    ctx(state(F, Objects, Attrs, Status, NextID, _),
        Input),
    NewCommands,
    ctx(state(F, Objects, Attrs, Status, NextID,
              commands(NewCommands)), Input)
).

% Get objects and commands from context
% (bulk getter)
ctx_objs_cmds(
    ctx(state(_, objects(Objects), _, _, _,
              commands(Commands)), _),
    Objects,
    Commands
).

% Get objects and attrs from context
% (bulk getter)
ctx_objs_attrs(
    ctx(state(_, objects(Objects), attrs(Attrs), _, _, _),
        _),
    Objects,
    Attrs
).

% Set objects and commands in context
% (bulk setter)
ctx_objs_cmds_ctx(
    ctx(state(F, _, Attrs, Status, NextID, _), Input),
    NewObjects,
    NewCommands,
    ctx(state(F, objects(NewObjects), Attrs, Status,
              NextID, commands(NewCommands)), Input)
).

% Set objects and attrs in context
% (bulk setter)
ctx_objs_attrs_ctx(
    ctx(state(F, _, _, Status, NextID, Commands), Input),
    NewObjects,
    NewAttrs,
    ctx(state(F, objects(NewObjects), attrs(NewAttrs),
              Status, NextID, Commands), Input)
).

% Get status from context
ctx_status(
    ctx(state(_, _, _, status(Status), _, _), _),
    Status
).

% Set status in context
ctx_status_ctx(
    ctx(state(F, Objects, Attrs, _, NextID, Commands),
        Input),
    NewStatus,
    ctx(state(F, Objects, Attrs, status(NewStatus),
              NextID, Commands), Input)
).

% Get status and commands from context
% (bulk getter - order matches state structure)
ctx_status_cmds(
    ctx(state(_, _, _, status(Status), _,
              commands(Commands)), _),
    Status,
    Commands
).

% Set status and commands in context
% (bulk setter - order matches state structure)
ctx_status_cmds_ctx(
    ctx(state(F, Objects, Attrs, _, NextID, _), Input),
    NewStatus,
    NewCommands,
    ctx(state(F, Objects, Attrs, status(NewStatus),
              NextID, commands(NewCommands)), Input)
).

% Get next_id from context
ctx_nextid(
    ctx(state(_, _, _, _, next_id(NID), _), _), NID
).

% Set next_id in context
ctx_nextid_ctx(
    ctx(state(F, Objs, Attrs, Status, _, Cmds), Input),
    NewNID,
    ctx(state(F, Objs, Attrs, Status, next_id(NewNID),
              Cmds), Input)
).

% Set next_id and commands in context
% (bulk setter - order matches state structure)
ctx_nextid_cmds_ctx(
    ctx(state(F, Objs, Attrs, Status, _, _), Input),
    NewNextID,
    NewCommands,
    ctx(state(F, Objs, Attrs, Status,
              next_id(NewNextID),
              commands(NewCommands)), Input)
).

% Get objects, next_id, and commands from context
% (bulk getter - order matches state structure)
ctx_objs_nextid_cmds(
    ctx(state(_, objects(Objects), _, _, next_id(NextID),
              commands(Commands)), _),
    Objects,
    NextID,
    Commands
).

% Set objects, next_id, and commands in context
% (bulk setter - order matches state structure)
ctx_objs_nextid_cmds_ctx(
    ctx(state(F, _, Attrs, Status, _, _), Input),
    NewObjects,
    NewNextID,
    NewCommands,
    ctx(state(F, objects(NewObjects), Attrs, Status,
              next_id(NewNextID),
              commands(NewCommands)), Input)
).

% Get input from context
ctx_input(ctx(_, Input), Input).

% Set input in context
ctx_input_ctx(
    ctx(State, _),
    NewInput,
    ctx(State, NewInput)
).

% Get events from context
% TODO: rename to inputevents?
ctx_events(ctx(_, input(events(E), _)), E).

% Get held keys from context
ctx_held(ctx(_, input(_, held(H))), H).

% ==========================================================
% State Accessors
% ==========================================================

% NOTE: State accessors are discouraged! Preferably use
%       context accessors instead!

% ==========================================================
% Object Accessors
% ==========================================================

% Extract ID from object
obj_id(object(id(ID), _, _, _), ID).

% Extract actions from object
obj_acns(object(_, _, actions(Actions), _), Actions).

% Extract type from object
obj_type(object(_, type(Type), _, _), Type).

% Extract collisions from object
obj_collisions(
    object(_, _, _, collisions(Colls)), Colls
).

% Extract ID and type from object
obj_id_type(object(id(ID), type(Type), _, _), ID, Type).

% Set actions in object
obj_acns_obj(
    object(id(ID), type(Type), _, collisions(Colls)),
    NewActions,
    object(id(ID), type(Type), actions(NewActions),
           collisions(Colls))
).

