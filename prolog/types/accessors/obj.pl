% This module centralizes all field access patterns to
% ensure consistent access to nested structures and make
% refactoring easier.

% Convention:
% The setters should have ObjIn and ObjOut as last arguments
% so that they can be better used in DCGs.
% The new naming convention is to start the predicate name
% with the entity then followed by all the fields to be
% get/set on it such as obj_id, obj_set_id.
% The fields come in the order in which they occur on the
% "thing"! So try to keep that!
% Rationale for covention:
% The functor immediately tells you what each argument is.

% ==========================================================
% Object Accessors
% ==========================================================

% Extract ID from object
obj_id(object(id(ID)), ID).

