% Type Declarations Module
% Defines all regular types for the game engine
% Based on type_constraint_predicates.md v2 + all addendums

:- module(types, [
    game_state_type/1,
    game_object_type/1,
    game_status_type/1,
    command_type/1,
    rev_hint_type/1,
    action_type/1,
    pos_type/1,
    attr_type/1,
    collision_type/1
]).

:- use_module(library(clpz)).
:- use_module(library(lists), [maplist/2, maplist/3, length/2]).
:- use_module(library(between), [between/3]).

% ============================================================================
% Helper Predicates (from Addendums 1 & 2)
% ============================================================================

% bounded_list_of/3 - using between/3 to avoid infinite generation (Addendum 2)
% NOTE: This uses ground/1 for dispatch (choosing algorithm),
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


% bounded_list_of_depth/4 - for recursive structures (Addendum 1)
bounded_list_of_depth(Goal, List, MaxLen, DepthLeft) :-
    between(0, MaxLen, Len),
    length(List, Len),
    maplist_with_depth(Goal, List, DepthLeft).

% maplist_with_depth/3 - depth-aware maplist (Addendum 1)
maplist_with_depth(_, [], _).
maplist_with_depth(Goal, [H|T], DepthLeft) :-
    call(Goal, H, DepthLeft),
    maplist_with_depth(Goal, T, DepthLeft).

% get_object_id/2 - extract ID from game_object (Addendum 3/4)
get_object_id(game_object(ID, _, _, _, _), ID).

% last/2 - get last element of a list (for Addendum 4)
last([X], X).
last([_|T], X) :-
    last(T, X).

% ============================================================================
% Core game_state_type/1 Constraint (Addendum 4 - final version)
% ============================================================================

game_state_type(
  game_state(Frame, Objects, Status, Score, 
             NextID, Commands, RevHints)
) :-
    Frame #>= 0,
    bounded_list_of(game_object_type, Objects, 1000),
    
    % Extract IDs
    maplist(get_object_id, Objects, IDs),
    % Enforce ascending order (implicitly ensures uniqueness) - Addendum 4
    ( ground(IDs)
    -> is_ascending(IDs)        % Simple check, no CLP(FD)
    ;  chain(#<, IDs)            % CLP(FD) for generation
    ),
    
    % Tail of list is the maximum ID (if list non-empty)
    (IDs = [] -> MaxID = -1 ; last(IDs, MaxID)),
    NextID #> MaxID,
    
    game_status_type(Status),
    Score #>= 0,
    bounded_list_of(command_type, Commands, 100),
    % TODO: next line causes extreme performance problems.
    bounded_list_of(rev_hint_type, RevHints, 1000).

% ============================================================================
% Status Constraint
% ============================================================================

game_status_type(playing).
game_status_type(won).
game_status_type(lost).

% ============================================================================
% game_object_type/1 Constraint (Addendum 1 - skip collision constraints)
% ============================================================================

game_object_type(
  game_object(ID, Type, attrs(Attrs), Actions, _Colls)
) :-
    ID #>= 0,
    object_type_type(Type),
    bounded_list_of(attribute_type, Attrs, 50),
    bounded_list_of(action_type, Actions, 100).
    % Skip collision constraints - feature not implemented yet
    % Colls is left unconstrained (permissive)

% ============================================================================
% object_type_type/1 Constraint
% ============================================================================

object_type_type(static).
object_type_type(enemy).
object_type_type(proj).
object_type_type(player).
object_type_type(tower).  % Used in game.pl

% ============================================================================
% attribute_type/1 Constraint
% ============================================================================

attribute_type(pos(X, Y)) :-
    % Coordinates: bounded integers
    X in -10000..10000,
    Y in -10000..10000.

% Alias for compatibility (attr_type/1 exported)
attr_type(A) :- attribute_type(A).

% ============================================================================
% action_type/1 and action_type/2 Constraints (Addendum 1)
% ============================================================================

% Public wrapper: defaults to depth 10
action_type(A) :- action_type(A, 10).

% Internal: tracks depth
action_type(wait_frames(N), _) :- 
    N #>= 0.

action_type(move_to(X, Y, Frames), _) :-
    X in -10000..10000,
    Y in -10000..10000,
    Frames #> 0.

action_type(despawn, _).

action_type(spawn(Type, Pos, Acts), DepthLeft) :-
    DepthLeft #> 0,
    DepthLeft1 #= DepthLeft - 1,
    object_type_type(Type),
    pos_type(Pos),
    bounded_list_of_depth(action_type, Acts, 100, DepthLeft1).

action_type(loop(Acts), DepthLeft) :-
    DepthLeft #> 0,
    DepthLeft1 #= DepthLeft - 1,
    bounded_list_of_depth(action_type, Acts, 100, DepthLeft1).

action_type(trigger_state_change(Change), _) :-
    state_change_type(Change).

action_type(parallel(Children), DepthLeft) :-
    DepthLeft #> 0,
    DepthLeft1 #= DepthLeft - 1,
    bounded_list_of_depth(
        action_type, Children, 100, DepthLeft1
    ).

action_type(parallel_running(Children), DepthLeft) :-
    DepthLeft #> 0,
    DepthLeft1 #= DepthLeft - 1,
    bounded_list_of_depth(
        action_type, Children, 100, DepthLeft1
    ).

% ============================================================================
% pos_type/1 Constraint
% ============================================================================

pos_type(pos(X, Y)) :-
    X in -10000..10000,
    Y in -10000..10000.

% ============================================================================
% command_type/1 Constraint (Addendum 1)
% ============================================================================

command_type(spawn_request(Type, Pos, Acts)) :-
    object_type_type(Type),
    pos_type(Pos),
    bounded_list_of(action_type, Acts, 100).

command_type(state_change(Change)) :-
    state_change_type(Change).

% ============================================================================
% state_change_type/1 Constraint
% ============================================================================

state_change_type(score(Delta)) :-
    % Score delta: bounded (allow negative for penalties)
    Delta in -1000..1000.

state_change_type(game_over(won)).
state_change_type(game_over(lost)).

% ============================================================================
% rev_hint_type/1 Constraint
% ============================================================================

rev_hint_type(despawned(ID, Attrs)) :-
    % Object ID: non-negative integer
    ID #>= 0,
    % Saved attributes at despawn time
    bounded_list_of(attribute_type, Attrs, 50).

% ============================================================================
% collision_type/1 Constraint
% ============================================================================
% NOTE: Collision constraints are skipped - feature not implemented yet
% This is a permissive placeholder that accepts any term

collision_type(_).





%
% Random helpers
%

% Check if ground list is strictly ascending
is_ascending([]).
is_ascending([_]).
is_ascending([A, B|Rest]) :-
    A < B,                    % Simple comparison, not #
    is_ascending([B|Rest]).