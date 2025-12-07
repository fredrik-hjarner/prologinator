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
    % State accessors
    state_status_state/3,
    % Object accessors
    obj_id/2,
    obj_attrs/2,
    obj_acns/2,
    obj_type/2,
    obj_id_attrs/3,
    obj_id_type/3,
    obj_type_attrs/3,
    obj_acns_obj/3,
    obj_attrs_acns_obj/4
]).

% ==========================================================
% Context Accessors
% ==========================================================

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

% Set attrs and actions in object
obj_attrs_acns_obj(
    object(id(ID), type(Type), _, _, collisions(Colls)),
    NewAttrs,
    NewActions,
    object(id(ID), type(Type), attrs(NewAttrs),
           actions(NewActions), collisions(Colls))
).

