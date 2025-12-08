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

:- module(accessors, [
    % Context accessors
    ctx_frame/2,
    ctx_frame_ctx/3,
    ctx_objs/2,
    ctx_objs_ctx/3,
    ctx_cmds/2,
    ctx_cmds_ctx/3,
    ctx_revhints/2,
    ctx_revhints_ctx/3,
    ctx_objs_cmds_revhints/4,
    ctx_objs_cmds_revhints_ctx/5,
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
    % Object accessors
    obj_id/2,
    obj_attrs/2,
    obj_acns/2,
    obj_type/2,
    obj_collisions/2,
    obj_id_attrs/3,
    obj_id_type/3,
    obj_type_attrs/3,
    obj_acns_obj/3,
    obj_attrs_obj/3,
    obj_attrs_acns_obj/4
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
    ctx(state(_, Objects, Status, NextID, Commands,
              RevHints)),
    NewFrame,
    ctx(state(frame(NewFrame), Objects, Status, NextID,
              Commands, RevHints))
).

% Get objects from context
ctx_objs(
    ctx(state(_, objects(Objects), _, _, _, _)), Objects
).

% Set objects in context
ctx_objs_ctx(
    ctx(state(F, _, Status, NextID, Commands, RevHints)),
    NewObjects,
    ctx(state(F, objects(NewObjects), Status,
              NextID, Commands, RevHints))
).

% Get commands from context
ctx_cmds(
    ctx(state(_, _, _, _, commands(Commands), _)),
    Commands
).

% Set commands in context
ctx_cmds_ctx(
    ctx(state(F, Objects, Status, NextID, _, RevHints)),
    NewCommands,
    ctx(state(F, Objects, Status, NextID,
              commands(NewCommands), RevHints))
).

% Get rev_hints from context
ctx_revhints(
    ctx(state(_, _, _, _, _, rev_hints(RevHints))),
    RevHints
).

% Set rev_hints in context
ctx_revhints_ctx(
    ctx(state(F, Objects, Status, NextID, Commands, _)),
    NewRevHints,
    ctx(state(F, Objects, Status, NextID,
              Commands, rev_hints(NewRevHints)))
).

% Get objects, commands, and rev_hints from context
% (bulk getter)
ctx_objs_cmds_revhints(
    ctx(state(_, objects(Objects), _, _, commands(Commands),
              rev_hints(RevHints))),
    Objects,
    Commands,
    RevHints
).

% Set objects, commands, and rev_hints in context
% (bulk setter)
ctx_objs_cmds_revhints_ctx(
    ctx(state(F, _, Status, NextID, _, _)),
    NewObjects,
    NewCommands,
    NewRevHints,
    ctx(state(F, objects(NewObjects), Status, NextID,
              commands(NewCommands),
              rev_hints(NewRevHints)))
).

% Get status from context
ctx_status(
    ctx(state(_, _, status(Status), _, _, _)),
    Status
).

% Set status in context
ctx_status_ctx(
    ctx(state(F, Objects, _, NextID, Commands, RevHints)),
    NewStatus,
    ctx(state(F, Objects, status(NewStatus), NextID,
              Commands, RevHints))
).

% Get status and commands from context
% (bulk getter - order matches state structure)
ctx_status_cmds(
    ctx(state(_, _, status(Status), _,
              commands(Commands), _)),
    Status,
    Commands
).

% Set status and commands in context
% (bulk setter - order matches state structure)
ctx_status_cmds_ctx(
    ctx(state(F, Objects, _, NextID, _, RevHints)),
    NewStatus,
    NewCommands,
    ctx(state(F, Objects, status(NewStatus), NextID,
              commands(NewCommands), RevHints))
).

% Get next_id from context
ctx_nextid(
    ctx(state(_, _, _, next_id(NID), _, _)), NID
).

% Set next_id in context
ctx_nextid_ctx(
    ctx(state(F, Objs, Status, _, Cmds, Revs)),
    NewNID,
    ctx(state(F, Objs, Status, next_id(NewNID), Cmds, Revs))
).

% Get objects, next_id, and commands from context
% (bulk getter - order matches state structure)
ctx_objs_nextid_cmds(
    ctx(state(_, objects(Objects), _, next_id(NextID),
              commands(Commands), _)),
    Objects,
    NextID,
    Commands
).

% Set objects, next_id, and commands in context
% (bulk setter - order matches state structure)
ctx_objs_nextid_cmds_ctx(
    ctx(state(F, _, Status, _, _, Revs)),
    NewObjects,
    NewNextID,
    NewCommands,
    ctx(state(F, objects(NewObjects), Status,
              next_id(NewNextID),
              commands(NewCommands), Revs))
).

% ==========================================================
% State Accessors
% ==========================================================

% Set status in state
state_status_state(
    state(F, Objects, _, NextID, Commands, RevHints),
    NewStatus,
    state(F, Objects, status(NewStatus), NextID,
          Commands, RevHints)
).

% ==========================================================
% Object Accessors
% ==========================================================

% Extract ID from object
obj_id(object(id(ID), _, _, _, _), ID).

% Extract attrs from object
obj_attrs(object(_, _, attrs(Attrs), _, _), Attrs).

% Extract actions from object
obj_acns(object(_, _, _, actions(Actions), _), Actions).

% Extract type from object
obj_type(object(_, type(Type), _, _, _), Type).

% Extract collisions from object
obj_collisions(
    object(_, _, _, _, collisions(Colls)), Colls
).

% Extract ID and attrs from object
obj_id_attrs(
    object(id(ID), _, attrs(Attrs), _, _), ID, Attrs
).

% Extract ID and type from object
obj_id_type(object(id(ID), type(Type), _, _, _), ID, Type).

% Extract type and attrs from object
obj_type_attrs(
    object(_, type(Type), attrs(Attrs), _, _), Type, Attrs
).

% Set actions in object
obj_acns_obj(
    object(id(ID), type(Type), attrs(Attrs), _,
           collisions(Colls)),
    NewActions,
    object(id(ID), type(Type), attrs(Attrs),
           actions(NewActions), collisions(Colls))
).

% Set attrs in object
obj_attrs_obj(
    object(id(ID), type(Type), _, actions(Actions),
           collisions(Colls)),
    NewAttrs,
    object(id(ID), type(Type), attrs(NewAttrs),
           actions(Actions), collisions(Colls))
).

% Set attrs and actions in object
obj_attrs_acns_obj(
    object(id(ID), type(Type), _, _, collisions(Colls)),
    NewAttrs,
    NewActions,
    object(id(ID), type(Type), attrs(NewAttrs),
           actions(NewActions), collisions(Colls))
).

