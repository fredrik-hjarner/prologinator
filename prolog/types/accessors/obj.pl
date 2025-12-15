% Object Accessors Module
% Provides getter and setter predicates for object data
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

