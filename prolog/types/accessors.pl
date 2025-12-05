% Accessors Module
% Provides getter and setter predicates for game data
% structures
%
% This module centralizes all field access patterns to
% ensure consistent access to nested structures and make
% refactoring easier.

:- module(accessors, [
    % Context accessors
    context_objects/2,
    context_set_objects/3,
    context_commands/2,
    context_set_commands/3,
    context_rev_hints/2,
    context_set_rev_hints/3,
    % Object accessors
    object_id/2,
    object_attrs/2
]).

% ==========================================================
% Context Accessors
% ==========================================================

% Get objects from context
context_objects(
    ctx(state(_, objects(Objects), _, _, _, _, _)), Objects
).

% Set objects in context
context_set_objects(
    ctx(state(F, _, Status, Score, NextID, Commands,
              RevHints)),
    NewObjects,
    ctx(state(F, objects(NewObjects), Status, Score,
              NextID, Commands, RevHints))
).

% Get commands from context
context_commands(
    ctx(state(_, _, _, _, _, commands(Commands), _)),
    Commands
).

% Set commands in context
context_set_commands(
    ctx(state(F, Objects, Status, Score, NextID, _,
              RevHints)),
    NewCommands,
    ctx(state(F, Objects, Status, Score, NextID,
              commands(NewCommands), RevHints))
).

% Get rev_hints from context
context_rev_hints(
    ctx(state(_, _, _, _, _, _, rev_hints(RevHints))),
    RevHints
).

% Set rev_hints in context
context_set_rev_hints(
    ctx(state(F, Objects, Status, Score, NextID,
              Commands, _)),
    NewRevHints,
    ctx(state(F, Objects, Status, Score, NextID,
              Commands, rev_hints(NewRevHints)))
).

% ==========================================================
% Object Accessors
% ==========================================================

% Extract ID from object
object_id(object(id(ID), _, _, _, _), ID).

% Extract attrs from object
object_attrs(object(_, _, attrs(Attrs), _, _), Attrs).

