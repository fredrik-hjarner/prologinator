% Accessors Module
% Provides getter and setter predicates for game data
% structures
%
% This module centralizes all field access patterns to
% ensure consistent access to nested structures and make
% refactoring easier.

:- use_module(library(assoc), [
    empty_assoc/1,
    put_assoc/4,
    del_assoc/4
]).

% Conventions:
% For getters the functors start with the "thing" we get
% from, then comes _ followed by all "stuff" we want to get.
% The fields come in the order in which they occur on the
% "thing"! So try to keep that!
% For setters it's the same, except we add _ followed by the
% "thing" in the end.
% Rationale for covention:
% The functor immediately tells you what to put where.

:- module(accessors, [
    % Context accessors
    ctx_frame/2,
    ctx_frame_ctx/3,
    ctx_objs/2,
    ctx_objs_ctx/3,
    ctx_attrs/2,
    ctx_attrs_ctx/3,
    ctx_state/2,
    ctx_state_ctx/3,
    ctx_cmds/2,
    ctx_cmds_ctx/3,
    ctx_objs_cmds/3,
    ctx_objs_cmds_ctx/4,
    ctx_objs_attrs/3,
    ctx_objs_attrs_ctx/4,
    ctx_status/2,
    ctx_status_ctx/3,
    ctx_status_cmds/3,
    ctx_status_cmds_ctx/4,
    ctx_nextid/2,
    ctx_nextid_ctx/3,
    ctx_objs_nextid_cmds/4,
    ctx_objs_nextid_cmds_ctx/5,
    % State accessors
    state_status_state/3,
    state_attrs/2,
    state_attrs_state/3,
    % Object accessors
    obj_id/2,
    obj_acns/2,
    obj_type/2,
    obj_collisions/2,
    obj_id_type/3,
    obj_acns_obj/3
]).

% ==========================================================
% Context Accessors
% ==========================================================

% Get frame from context
ctx_frame(
    ctx(state(frame(F), _, _, _, _, _)), F
).

% Set frame in context
ctx_frame_ctx(
    ctx(state(_, Objects, Attrs, Status, NextID,
              Commands)),
    NewFrame,
    ctx(state(frame(NewFrame), Objects, Attrs, Status,
              NextID, Commands))
).

% Get objects from context
ctx_objs(
    ctx(state(_, objects(Objects), _, _, _, _)),
    Objects
).

% Set objects in context
ctx_objs_ctx(
    ctx(state(F, _, Attrs, Status, NextID, Commands)),
    NewObjects,
    ctx(state(F, objects(NewObjects), Attrs, Status,
              NextID, Commands))
).

% Get attributes from context
ctx_attrs(
    ctx(state(_, _, attrs(Attrs), _, _, _)), Attrs
).

% Set attributes in context
ctx_attrs_ctx(
    ctx(state(F, Objects, _, Status, NextID, Commands)),
    NewAttrs,
    ctx(state(F, Objects, attrs(NewAttrs), Status,
              NextID, Commands))
).

% Get state from context
ctx_state(
    ctx(State), State
).

% Set state in context
ctx_state_ctx(
    ctx(_), NewState, ctx(NewState)
).

% Get commands from context
ctx_cmds(
    ctx(state(_, _, _, _, _, commands(Commands))),
    Commands
).

% Set commands in context
ctx_cmds_ctx(
    ctx(state(F, Objects, Attrs, Status, NextID, _)),
    NewCommands,
    ctx(state(F, Objects, Attrs, Status, NextID,
              commands(NewCommands)))
).

% Get objects and commands from context
% (bulk getter)
ctx_objs_cmds(
    ctx(state(_, objects(Objects), _, _, _,
              commands(Commands))),
    Objects,
    Commands
).

% Get objects and attrs from context
% (bulk getter)
ctx_objs_attrs(
    ctx(state(_, objects(Objects), attrs(Attrs), _, _, _)),
    Objects,
    Attrs
).

% Set objects and commands in context
% (bulk setter)
ctx_objs_cmds_ctx(
    ctx(state(F, _, Attrs, Status, NextID, _)),
    NewObjects,
    NewCommands,
    ctx(state(F, objects(NewObjects), Attrs, Status,
              NextID, commands(NewCommands)))
).

% Set objects and attrs in context
% (bulk setter)
ctx_objs_attrs_ctx(
    ctx(state(F, _, _, Status, NextID, Commands)),
    NewObjects,
    NewAttrs,
    ctx(state(F, objects(NewObjects), attrs(NewAttrs),
              Status, NextID, Commands))
).

% Get status from context
ctx_status(
    ctx(state(_, _, _, status(Status), _, _)),
    Status
).

% Set status in context
ctx_status_ctx(
    ctx(state(F, Objects, Attrs, _, NextID, Commands)),
    NewStatus,
    ctx(state(F, Objects, Attrs, status(NewStatus),
              NextID, Commands))
).

% Get status and commands from context
% (bulk getter - order matches state structure)
ctx_status_cmds(
    ctx(state(_, _, _, status(Status), _,
              commands(Commands))),
    Status,
    Commands
).

% Set status and commands in context
% (bulk setter - order matches state structure)
ctx_status_cmds_ctx(
    ctx(state(F, Objects, Attrs, _, NextID, _)),
    NewStatus,
    NewCommands,
    ctx(state(F, Objects, Attrs, status(NewStatus),
              NextID, commands(NewCommands)))
).

% Get next_id from context
ctx_nextid(
    ctx(state(_, _, _, _, next_id(NID), _)), NID
).

% Set next_id in context
ctx_nextid_ctx(
    ctx(state(F, Objs, Attrs, Status, _, Cmds)),
    NewNID,
    ctx(state(F, Objs, Attrs, Status, next_id(NewNID),
              Cmds))
).

% Get objects, next_id, and commands from context
% (bulk getter - order matches state structure)
ctx_objs_nextid_cmds(
    ctx(state(_, objects(Objects), _, _, next_id(NextID),
              commands(Commands))),
    Objects,
    NextID,
    Commands
).

% Set objects, next_id, and commands in context
% (bulk setter - order matches state structure)
ctx_objs_nextid_cmds_ctx(
    ctx(state(F, _, Attrs, Status, _, _)),
    NewObjects,
    NewNextID,
    NewCommands,
    ctx(state(F, objects(NewObjects), Attrs, Status,
              next_id(NewNextID),
              commands(NewCommands)))
).

% ==========================================================
% State Accessors
% ==========================================================

% Get attributes from state
% Note: If possible use ctx_attrs directly if you can
%       because ya know maybe you dont need to grab state
%       first ya know.
state_attrs(
    state(_, _, attrs(Attrs), _, _, _), Attrs
).

% Set attributes in state
% Note: If possible use ctx_attrs_ctx directly if you can
%       because ya know maybe you dont need to grab state
%       first ya know.
state_attrs_state(
    state(F, Objects, _, Status, NextID, Commands),
    NewAttrs,
    state(F, Objects, attrs(NewAttrs), Status,
          NextID, Commands)
).

% Set status in state
state_status_state(
    state(F, Objects, Attrs, _, NextID, Commands),
    NewStatus,
    state(F, Objects, Attrs, status(NewStatus),
          NextID, Commands)
).

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


