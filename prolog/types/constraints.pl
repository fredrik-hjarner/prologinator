% Type Declarations Module
% Defines all regular types for the game engine
% Based on type_constraint_predicates.md v2 + all addendums

:- module(constraints, [
    % constraints
    game_state_constraint/1,
    game_object_constraint/1,
    game_status_constraint/1,
    command_constraint/1,
    rev_hint_constraint/1,
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

% get_object_id/2 - extract ID from game_object
% (Addendum 3/4)
get_object_id(game_object(id(ID), _, _, _, _), ID).

% last/2 - get last element of a list (for Addendum 4)
last([X], X).
last([_|T], X) :-
    last(T, X).

% ==========================================================
% Core game_state_constraint/1 Constraint
% (Addendum 4 - final version)
% ==========================================================

game_state_constraint(
  game_state(
      frame(Frame),
      objects(Objects),
      status(Status),
      score(Score),
      next_id(NextID),
      commands(Commands),
      rev_hints(RevHints)
  )
) :-
    Frame #>= 0,
    % Frame #=< 1000,
    bounded_list_of(game_object_constraint, Objects, 200),
    
    % Extract IDs
    maplist(get_object_id, Objects, IDs),
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
    Score #>= 0,
    bounded_list_of(command_constraint, Commands, 100),
    % TODO: next line causes extreme performance problems.
    bounded_list_of(rev_hint_constraint, RevHints, 200).

% ==========================================================
% Status Constraint
% ==========================================================

game_status_constraint(playing).
game_status_constraint(won).
game_status_constraint(lost).

% ==========================================================
% game_object_constraint/1 Constraint
% (Addendum 1 - skip collision constraints)
% ==========================================================

game_object_constraint(
  game_object(
      id(ID),
      type(Type),
      attrs(Attrs),
      actions(Actions),
      collisions(_Colls)
  )
) :-
    ID #>= 0,
    ID #=< 1000,
    object_type_constraint(Type),
    bounded_list_of(attribute_constraint, Attrs, 50),
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

attribute_constraint(pos(X, Y)) :-
    % Coordinates: bounded integers
    X in -10..200,
    Y in -10..200.

% Alias for compatibility (attr_constraint/1 exported)
attr_constraint(A) :- attribute_constraint(A).

% ==========================================================
% action_constraint/1 and action_constraint/2 Constraints
% (Addendum 1)
% ==========================================================

% Public wrapper: defaults to depth 10
action_constraint(A) :- action_constraint(A, 10).

% Internal: tracks depth
action_constraint(wait_frames(N), _) :- 
    N #>= 0.

action_constraint(move_to(X, Y, Frames), _) :-
    X in -10..200,
    Y in -10..200,
    Frames #> 0.

action_constraint(despawn, _).

action_constraint(spawn(Type, Pos, Acts), DepthLeft) :-
    DepthLeft #> 0,
    DepthLeft1 #= DepthLeft - 1,
    object_type_constraint(Type),
    pos_constraint(Pos),
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

action_constraint(parallel(Children), DepthLeft) :-
    DepthLeft #> 0,
    DepthLeft1 #= DepthLeft - 1,
    bounded_list_of_depth(
        action_constraint, Children, 100, DepthLeft1
    ).

action_constraint(parallel_running(Children), DepthLeft) :-
    DepthLeft #> 0,
    DepthLeft1 #= DepthLeft - 1,
    bounded_list_of_depth(
        action_constraint, Children, 100, DepthLeft1
    ).

% ==========================================================
% pos_constraint/1 Constraint
% ==========================================================

pos_constraint(pos(X, Y)) :-
    X in 0..200,
    Y in 0..200.

% ==========================================================
% command_constraint/1 Constraint (Addendum 1)
% ==========================================================

command_constraint(spawn_request(Type, Pos, Acts)) :-
    object_type_constraint(Type),
    pos_constraint(Pos),
    bounded_list_of(action_constraint, Acts, 100).

command_constraint(state_change(Change)) :-
    state_change_constraint(Change).

% ==========================================================
% state_change_constraint/1 Constraint
% ==========================================================

state_change_constraint(score(Delta)) :-
    % Score delta: bounded (allow negative for penalties)
    Delta in 0..1000.

state_change_constraint(game_over(won)).
state_change_constraint(game_over(lost)).

% ==========================================================
% rev_hint_constraint/1 Constraint
% ==========================================================

rev_hint_constraint(despawned(ID, Attrs)) :-
    % Object ID: non-negative integer
    ID #>= 0,
    % Saved attributes at despawn time
    bounded_list_of(attribute_constraint, Attrs, 50).

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