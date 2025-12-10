% Type Declarations Module
% Defines all regular types for the game engine
% Based on type_constraint_predicates.md v2 + all addendums

:- module(constraints, [
    % constraints
    state_constraint/1,
    object_constraint/1,
    game_status_constraint/1,
    command_constraint/1,
    action_constraint/1,
    pos_constraint/1,
    attr_constraint/1,
    collision_constraint/1
]).

:- use_module(library(clpz)).
:- use_module(library(lists), [
    maplist/2,
    maplist/3,
    length/2
]).
:- use_module(library(between), [between/3]).
:- use_module(library(time), [time/1]).
:- use_module(library(format), [format/2]).
:- use_module('./accessors', [obj_id/2]).

% ==========================================================
% Helper Predicates (from Addendums 1 & 2)
% ==========================================================

% bounded_list_of/3 - using between/3 to avoid infinite
% generation (Addendum 2)
% NOTE: This uses ground/1 for dispatch (choosing
% algorithm),
% not for logic (validation). Both branches enforce the same
% constraint: length(List) =< MaxLen. The predicate remains
% fully bidirectional:
% - ?- bounded_list_of(Goal, [a,b], 10).  % validates
% - ?- bounded_list_of(Goal, List, 3).    % generates
bounded_list_of(Goal, List, MaxLen) :-
( ground(List)
-> % Fast path: List is already ground (validation mode)
   % Just check length once and validate elements
   length(List, Len),
   Len =< MaxLen,
   maplist(Goal, List)
;  % Constraint path: List is unbound (generation mode)
   % Generate lengths 0, 1, 2, ..., MaxLen
   between(0, MaxLen, Len),
   length(List, Len),
   maplist(Goal, List)
).


% bounded_list_of_depth/4 - for recursive structures
% (Addendum 1)
bounded_list_of_depth(Goal, List, MaxLen, DepthLeft) :-
    between(0, MaxLen, Len),
    length(List, Len),
    maplist_with_depth(Goal, List, DepthLeft).

% maplist_with_depth/3 - depth-aware maplist (Addendum 1)
maplist_with_depth(_, [], _).
maplist_with_depth(Goal, [H|T], DepthLeft) :-
    call(Goal, H, DepthLeft),
    maplist_with_depth(Goal, T, DepthLeft).


% last/2 - get last element of a list (for Addendum 4)
last([X], X).
last([_|T], X) :-
    last(T, X).

% ==========================================================
% Core game_state_constraint/1 Constraint
% (Addendum 4 - final version)
% ==========================================================

state_constraint(
  state(
      frame(Frame),
      objects(Objects),
      attrs(_Attrs),
      status(Status),
      next_id(NextID),
      commands(Commands)
  )
) :-
    Frame #>= 0,
    % Frame #=< 1000,
    bounded_list_of(object_constraint, Objects, 200),
    
    % Extract IDs
    maplist(obj_id, Objects, IDs),
    % Enforce ascending order
    % (implicitly ensures uniqueness) - Addendum 4
    ( ground(IDs)
    ->
        is_ascending(IDs)        % Simple check, no CLP(FD)
    ;
        chain(#<, IDs)            % CLP(FD) for generation
    ),
    
    % Tail of list is the maximum ID (if list non-empty)
    (IDs = [] -> MaxID = -1 ; last(IDs, MaxID)),
    NextID #> MaxID,
    
    game_status_constraint(Status),
    bounded_list_of(command_constraint, Commands, 100).

% ==========================================================
% Status Constraint
% ==========================================================

game_status_constraint(playing).
game_status_constraint(won).
game_status_constraint(lost).

% ==========================================================
% object_constraint/1 Constraint
% (Addendum 1 - skip collision constraints)
% ==========================================================

object_constraint(
  object(
      id(ID),
      type(Type),
      actions(Actions),
      collisions(_Colls)
  )
) :-
    ID #>= 0,
    ID #=< 1000,
    object_type_constraint(Type),
    bounded_list_of(action_constraint, Actions, 100).
    % Skip collision constraints - feature not
    % implemented yet
    % Colls is left unconstrained (permissive)

% ==========================================================
% object_type_constraint/1 Constraint
% ==========================================================

object_type_constraint(static).
object_type_constraint(enemy).
object_type_constraint(proj).
object_type_constraint(player).
object_type_constraint(tower).  % Used in game.pl

% ==========================================================
% attribute_constraint/1 Constraint
% ==========================================================
% Attributes are completely free-form - no validation needed
% Users can store anything: x(100), hp(50), color(red), etc.

attribute_constraint(_).  % Accept any attribute

% Alias for compatibility (attr_constraint/1 exported)
attr_constraint(A) :- attribute_constraint(A).

% ==========================================================
% action_constraint/1 and action_constraint/2 Constraints
% (Addendum 1)
% ==========================================================

% Public wrapper: defaults to depth 10
action_constraint(A) :- action_constraint(A, 10).

% Internal: tracks depth
action_constraint(wait(N), _) :- 
    N #>= 0.

action_constraint(move_to(X, Y, Frames), _) :-
    X in -10..200,
    Y in -10..200,
    Frames #> 0.

action_constraint(despawn, _).

action_constraint(spawn(Type, _X, _Y, Acts), DepthLeft) :-
    DepthLeft #> 0,
    DepthLeft1 #= DepthLeft - 1,
    object_type_constraint(Type),
    % X and Y: no constraints, just ground terms
    bounded_list_of_depth(
        action_constraint, Acts, 100, DepthLeft1
    ).

action_constraint(loop(Acts), DepthLeft) :-
    DepthLeft #> 0,
    DepthLeft1 #= DepthLeft - 1,
    bounded_list_of_depth(
        action_constraint, Acts, 30, DepthLeft1
    ).

action_constraint(trigger_state_change(Change), _) :-
    state_change_constraint(Change).

action_constraint(parallel_all(Children), DepthLeft) :-
    DepthLeft #> 0,
    DepthLeft1 #= DepthLeft - 1,
    bounded_list_of_depth(
        action_constraint, Children, 100, DepthLeft1
    ).

action_constraint(parallel_race(Children), DepthLeft) :-
    DepthLeft #> 0,
    DepthLeft1 #= DepthLeft - 1,
    bounded_list_of_depth(
        action_constraint, Children, 100, DepthLeft1
    ).

action_constraint(
    parallel_all_running(Children), DepthLeft
) :-
    DepthLeft #> 0,
    DepthLeft1 #= DepthLeft - 1,
    bounded_list_of_depth(
        action_constraint, Children, 100, DepthLeft1
    ).

action_constraint(
    parallel_race_running(Children), DepthLeft
) :-
    DepthLeft #> 0,
    DepthLeft1 #= DepthLeft - 1,
    bounded_list_of_depth(
        action_constraint, Children, 100, DepthLeft1
    ).

% ==========================================================
% pos_constraint/1 Constraint
% ==========================================================
% DEPRECATED: spawn now uses separate X, Y arguments
% Kept for backward compatibility if needed elsewhere
pos_constraint(_).  % Accept any position structure

% ==========================================================
% command_constraint/1 Constraint (Addendum 1)
% ==========================================================

command_constraint(spawn_request(Type, _X, _Y, Acts)) :-
    object_type_constraint(Type),
    % X and Y: no constraints
    bounded_list_of(action_constraint, Acts, 100).

command_constraint(state_change(Change)) :-
    state_change_constraint(Change).

% ==========================================================
% state_change_constraint/1 Constraint
% ==========================================================

state_change_constraint(game_over(won)).
state_change_constraint(game_over(lost)).


% ==========================================================
% collision_constraint/1 Constraint
% ==========================================================
% NOTE: Collision constraints are skipped - feature not
% implemented yet
% This is a permissive placeholder that accepts any term

collision_constraint(_).





%
% Random helpers
%

% Check if ground list is strictly ascending
is_ascending([]).
is_ascending([_]).
is_ascending([A, B|Rest]) :-
    A < B,                    % Simple comparison, not #
    is_ascending([B|Rest]).