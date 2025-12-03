<spec>
% ================================================================
% Bidirectional Game State Constraints (v2)
% Pure unification + CLP(FD), no type tests
% ================================================================

% ================================================================
% MOTIVATION: The Tyranny of Validation vs Freedom of Constraints
% ================================================================

% Validation is a dead-end. When you test instantiation with atom/1,
% ground/1, or var/1, you invoke the ancient curse of Prolog's
% imperative past: immediate judgment. These predicates are
% executioners—they examine the current state of a variable and pass
% sentence on the spot, with no appeal, no backtracking, no second
% chances. They leave no choice points. Once they fail, the entire
% computation collapses, and Prolog cannot explore alternative paths.
% Validation says: "This must be ground NOW. This must be an atom NOW."
% It enslaves your code to a rigid calling convention, condemning it
% to a single direction of execution. You can check a structure—yes—but
% you cannot generate one. You cannot query with unknowns. You are
% locked into the forward direction, forever marching in lockstep with
% the original programmer's assumptions.

% Constraints, by contrast, are prophetic. They speak not of what IS,
% but of what SHALL BE. When you declare Frame #>= 0 or Status =
% (playing ; won ; lost), you are not testing—you are recording a
% promise. The constraint persists through unification. It flows
% backward through time and forward through space. Unbound variables
% can satisfy constraints; they can be questioned, probed, even
% generated wholesale. A constraint describes a relationship that holds
% regardless of which direction Prolog traverses. When you query
% ?- game_state(GS), GS = game_state(F, _, _, Score, _),
%    Score #> 50, F #< 100,
% you are not validating a pre-built structure—you are exploring the
% solution space, letting Prolog discover what states exist that meet
% your criteria. The constraint says: "Whatever value this variable
% takes, it must respect this law." That is why constraints work
% bidirectionally while validation crumbles.

% We abandon validation because it is fundamentally incompatible with
% bidirectionality. Validation is a function of a specific execution
% order; it requires its input to be ground first. Constraints are
% relations—they exist independently of order. This is the hidden
% reason Prolog's greatest power remains untapped by most programmers:
% they cling to validation, that ancient tool of imperative languages,
% when they could be wielding constraints, the native magic of the
% logical paradigm. In this file, we have chosen the harder path: pure
% unification patterns and CLP(FD) constraints. No type tests. No dead
% ends. Only bidirectional exploration.

:- use_module(library(clpfd)).

% ================================================================
% DESIGN NOTES
% ================================================================

% ATOMS with finite domain (Status):
%   Status must be one of: playing, won, lost
%   We can describe this: game_status(Status)
%   ✓ Safe for unbound variables
%   ✓ Describes the constraint without testing

% ATOMS with infinite domain (ID):
%   An ID could be: tower_1, enemy_9999, player_abc, etc.
%   Impossible to constrain atom/1 without testing.
%   SOLUTION: Use integers instead!
%   ID becomes an integer: 0, 1, 2, 3, ...
%   Now we can use CLP(FD): ID #>= 0
%   ✓ Works with unbound ID
%   ✓ Can generate/query IDs

% LISTS (Objects, Actions, etc):
%   List is: [] or [Head|Tail]
%   Pattern: ([] ; [H|T])
%   NOT a test of "is_list", just structural unification
%   ✓ Safe for unbound variables
%   ✓ Describes shape without testing

% NUMERIC fields (Frame, Score, NextID):
%   Use CLP(FD) constraints: X #>= 0
%   ✓ Works unbound
%   ✓ Can query/generate
%   ✓ Composable with other constraints

% ================================================================
% Core game_state/1 Constraint
% ================================================================

game_state(
  game_state(
    Frame, Objects, Status, 
    Score, NextID, 
    Commands, RevHints
  )
) :-
    % Frame: non-negative integer
    Frame #>= 0,
    % Objects: list of game_objects
    (Objects = [] ; 
     Objects = [_|_]),
    maplist(game_object, Objects),
    % Status: one of {playing, won, lost}
    game_status(Status),
    % Score: non-negative integer
    Score #>= 0,
    % NextID: non-negative integer (next ID to assign)
    NextID #>= 0,
    % Commands: list of command structures
    (Commands = [] ; 
     Commands = [_|_]),
    maplist(command, Commands),
    % RevHints: list of revision hints
    (RevHints = [] ; 
     RevHints = [_|_]),
    maplist(rev_hint, RevHints).

% ================================================================
% Status Constraint
% ================================================================
% Finite set of valid statuses
% Unification-based: game_status(X) constrains X to these values

% NOTE: game_status will prolly be removed in later versions.
game_status(playing).
game_status(won).
game_status(lost).

% ================================================================
% game_object/1 Constraint
% ================================================================

game_object(
  game_object(ID, Type, attrs(Attrs), Actions, Colls)
) :-
    % ID: non-negative integer (no atom testing!)
    % Comment: If you need UUIDs, generate them before
    %          calling game_object/1, then they're ground
    ID #>= 0,
    % Type: one of {static, enemy, proj, player}
    object_type(Type),
    % Attrs: list of attribute structures
    (Attrs = [] ; 
     Attrs = [_|_]),
    maplist(attribute, Attrs),
    % Actions: list of action structures
    (Actions = [] ; 
     Actions = [_|_]),
    maplist(action, Actions),
    % Colls: list of collision structures
    (Colls = [] ; 
     Colls = [_|_]),
    maplist(collision, Colls).

% ================================================================
% object_type/1 Constraint
% ================================================================
% Finite set: {static, enemy, proj, player}

%NOTE: These should probably not be hard-coded since the user of the Prologinator system might want new types...
% ...though the could probable be hardcoded but by some preprocessing collecting all occuring types in a user's game data file.
object_type(static).
object_type(enemy).
object_type(proj).
object_type(player).

% ================================================================
% attribute/1 Constraint
% ================================================================

attribute(pos(X, Y)) :-
    % Coordinates: bounded integers
    % Comment: -10000..10000 is arbitrary; adjust as needed
    X in -10000..10000,
    Y in -10000..10000.
% Comment: Could add more attributes (health, color, etc)
% attribute(health(H)) :- H #> 0.

% ================================================================
% action/1 Constraint
% ================================================================

action(wait_frames(N)) :-
    % Frames to wait: positive
    N #> 0.

action(move_to(X, Y, Frames)) :-
    % Target coordinates: bounded
    X in -10000..10000,
    Y in -10000..10000,
    % Frames to move: positive
    Frames #> 0.

action(despawn).

action(spawn(Type, Pos, Acts)) :-
    % Spawned object type
    object_type(Type),
    % Initial position
    pos(Pos),
    % Initial action queue
    (Acts = [] ; 
     Acts = [_|_]),
    maplist(action, Acts).

action(loop(Acts)) :-
    % Actions to loop
    (Acts = [] ; 
     Acts = [_|_]),
    maplist(action, Acts).

action(trigger_state_change(Change)) :-
    % State change to trigger
    state_change(Change).

action(parallel(Children)) :-
    % Child actions to run in parallel
    (Children = [] ; 
     Children = [_|_]),
    maplist(action, Children).

action(parallel_running(Children)) :-
    % Child actions currently running
    (Children = [] ; 
     Children = [_|_]),
    maplist(action, Children).

% ================================================================
% state_change/1 Constraint
% ================================================================

state_change(score(Delta)) :-
    % Score delta: bounded (allow negative for penalties)
    Delta in -1000..1000.

% NOTE: won/lost will probably not be hard-coded in later versions.
state_change(game_over(won)).
state_change(game_over(lost)).

% ================================================================
% pos/1 Constraint
% ================================================================

pos(pos(X, Y)) :-
    X in -10000..10000,
    Y in -10000..10000.

% ================================================================
% command/1 Constraint
% ================================================================

command(spawn_request(Type, Pos, Acts)) :-
    object_type(Type),
    pos(Pos),
    (Acts = [] ; 
     Acts = [_|_]),
    maplist(action, Acts).

command(state_change(Change)) :-
    state_change(Change).

% ================================================================
% collision/1 Constraint
%
% NOTE: I think collision was sloppily designed, will require one more iteration...
% ================================================================
% Comment: collision(C) could describe:
%   - collision(ID1, ID2) for two colliding objects
%   - collision(Type1, Type2) for object types
%   For now, permissive: any term is a collision
%   If you want to constrain it, add specifics here

collision(C) :-
    % Comment: Currently accepts any term
    % Could add: collision(ID1, ID2) :- ID1 #>= 0, ID2 #>= 0.
    var(C) ; compound(C) ; atom(C) ; number(C).

% ================================================================
% rev_hint/1 Constraint
% ================================================================

rev_hint(despawned(ID, Attrs)) :-
    % Object ID: non-negative integer
    ID #>= 0,
    % Saved attributes at despawn time
    (Attrs = [] ; 
     Attrs = [_|_]),
    maplist(attribute, Attrs).
% Comment: Could add more hint types as needed
% rev_hint(moved(ID, OldPos, NewPos)) :- ...

<addendum_1>
```prolog
% ================================================================
% Bidirectional Game State Constraints (v2) - ADDENDUM 1
% Fixing Unbounded Generation
% ================================================================

% READ THIS AFTER v2. This addendum patches two fatal flaws
% discovered when attempting to generate game states from
% unbound variables.

% ================================================================
% PROBLEM 1: maplist/2 Generates Infinite Lists
% ================================================================

% maplist(Goal, List) with unbound List generates lists of
% length 0, 1, 2, 3, ... forever. No termination.
%
% SOLUTION: Bound all lists to maximum lengths.

% ----------------------------------------------------------------
% Helper: bounded_list_of/3
% ----------------------------------------------------------------

bounded_list_of(Goal, List, MaxLen) :-
    length(List, Len),
    Len #=< MaxLen,
    maplist(Goal, List).

% USE THIS: bounded_list_of(game_object, Objects, 1000)
% NOT THIS: maplist(game_object, Objects)

% ================================================================
% PROBLEM 2: Recursive Structures Have Infinite Depth
% ================================================================

% action(spawn(..., Acts)) where Acts contains more spawns
% creates unbounded nesting: spawn inside spawn inside spawn...
%
% SOLUTION: Thread a depth counter through action/2.
% Only action is truly recursive; other predicates don't need
% depth parameters.

% ----------------------------------------------------------------
% Helper: bounded_list_of_depth/4
% ----------------------------------------------------------------

bounded_list_of_depth(Goal, List, MaxLen, DepthLeft) :-
    length(List, Len),
    Len #=< MaxLen,
    maplist_with_depth(Goal, List, DepthLeft).

maplist_with_depth(_, [], _).
maplist_with_depth(Goal, [H|T], DepthLeft) :-
    call(Goal, H, DepthLeft),
    maplist_with_depth(Goal, T, DepthLeft).

% ================================================================
% REVISED PREDICATES (changes only)
% ================================================================

% ----------------------------------------------------------------
% game_state/1 - NO depth parameter (not recursive)
% ----------------------------------------------------------------

game_state(
  game_state(Frame, Objects, Status, Score, 
             NextID, Commands, RevHints)
) :-
    Frame #>= 0,
    bounded_list_of(game_object, Objects, 1000),
    game_status(Status),
    Score #>= 0,
    NextID #>= 0,
    bounded_list_of(command, Commands, 100),
    bounded_list_of(rev_hint, RevHints, 1000).

% ----------------------------------------------------------------
% game_object/1 - NO depth parameter (not recursive)
% ----------------------------------------------------------------

game_object(
  game_object(ID, Type, attrs(Attrs), Actions, Colls)
) :-
    ID #>= 0,
    object_type(Type),
    bounded_list_of(attribute, Attrs, 50),
    bounded_list_of(action, Actions, 100),
    bounded_list_of(collision, Colls, 50).

% ----------------------------------------------------------------
% action/1 becomes action/2 - ONLY action is recursive
% ----------------------------------------------------------------

% Public wrapper: defaults to depth 10
action(A) :- action(A, 10).

% Internal: tracks depth
action(wait_frames(N), _) :- 
    N #> 0.

action(move_to(X, Y, Frames), _) :-
    X in -10000..10000,
    Y in -10000..10000,
    Frames #> 0.

action(despawn, _).

action(spawn(Type, Pos, Acts), DepthLeft) :-
    DepthLeft #> 0,
    DepthLeft1 #= DepthLeft - 1,
    object_type(Type),
    pos(Pos),
    bounded_list_of_depth(action, Acts, 100, DepthLeft1).

action(loop(Acts), DepthLeft) :-
    DepthLeft #> 0,
    DepthLeft1 #= DepthLeft - 1,
    bounded_list_of_depth(action, Acts, 100, DepthLeft1).

action(trigger_state_change(Change), _) :-
    state_change(Change).

action(parallel(Children), DepthLeft) :-
    DepthLeft #> 0,
    DepthLeft1 #= DepthLeft - 1,
    bounded_list_of_depth(
        action, Children, 100, DepthLeft1
    ).

action(parallel_running(Children), DepthLeft) :-
    DepthLeft #> 0,
    DepthLeft1 #= DepthLeft - 1,
    bounded_list_of_depth(
        action, Children, 100, DepthLeft1
    ).

% ----------------------------------------------------------------
% command/1 - NO depth parameter (not recursive)
% ----------------------------------------------------------------

command(spawn_request(Type, Pos, Acts)) :-
    object_type(Type),
    pos(Pos),
    bounded_list_of(action, Acts, 100).

command(state_change(Change)) :-
    state_change(Change).

% ================================================================
% USAGE NOTES
% ================================================================

% Normal usage: just call /1 versions
% ?- game_state(GS).
% ?- action(A).
%
% Each top-level action starts with depth=10.
% Nested actions (spawn inside spawn) share the depth budget.
%
% For validation of ground terms: depth doesn't matter,
% constraint posting is cheap.
%
% For generation: adjust MaxLen and default depth (10 in
% action/1 wrapper) based on empirical testing.

% ================================================================
% SUGGESTED LIMITS (adjust to taste)
% ================================================================

% Objects in game state: 1000
% Actions per object: 100
% Commands pending: 100
% Attributes per object: 50
% Collisions tracked: 50
% Action nesting depth: 10 (hardcoded in action/1)
%
% These are conservative. Profile with real data to tune.

% ================================================================
% REMOVE FROM v2
% ================================================================

% Delete these lines from the original v2 document:
% - (Objects = [] ; Objects = [_|_])
% - All similar disjunctions for lists
% Replace all maplist/2 calls with bounded_list_of/3
```
</addendum_1>

<addendum_2>
```prolog
% ================================================================
% Bidirectional Game State Constraints (v2) - ADDENDUM 2
% Fixing Infinite Generation in bounded_list_of
% ================================================================

% READ THIS AFTER v2 + ADDENDUM 1.
% This fixes a critical bug that causes infinite generation.

% ================================================================
% PROBLEM: length/2 + maplist/2 = Infinite Loop
% ================================================================

% The Addendum 1 version hangs on generation:
%
% bounded_list_of(Goal, List, MaxLen) :-
%     length(List, Len),
%     Len #=< MaxLen,        % Constraint doesn't stop maplist
%     maplist(Goal, List).   % Generates lists forever
%
% When List is unbound, maplist/2 keeps trying longer lists
% (0, 1, 2, 3, ...) even though Len #=< MaxLen. The constraint
% fails for lengths > MaxLen, but maplist never stops trying.

% ================================================================
% SOLUTION: between/3 Generates Finite Domain
% ================================================================

% between/3 explicitly tries only 0..MaxLen, then stops.

bounded_list_of(Goal, List, MaxLen) :-
    between(0, MaxLen, Len),     % Try 0,1,2,...MaxLen ONLY
    length(List, Len),            % Create list skeleton
    maplist(Goal, List).          % Apply constraints

bounded_list_of_depth(Goal, List, MaxLen, DepthLeft) :-
    between(0, MaxLen, Len),
    length(List, Len),
    maplist_with_depth(Goal, List, DepthLeft).

% maplist_with_depth/3 unchanged from Addendum 1

% ================================================================
% REPLACE in Addendum 1
% ================================================================

% Find these two predicates in Addendum 1 and replace them
% with the versions above. All other code remains the same.
```
</addendum_2>

<addendum_3>
```prolog
% ================================================================
% Bidirectional Game State Constraints (v2) - ADDENDUM 3
% Unique ID Enforcement
% ================================================================

% READ THIS AFTER v2 + ADDENDUM 1 + ADDENDUM 2.
% This adds a critical semantic constraint: unique object IDs.

% ================================================================
% PROBLEM: Duplicate IDs Are Type-Valid But Semantically Invalid
% ================================================================

% The current game_object/1 checks ID #>= 0 in isolation.
% This allows multiple objects to share the same ID:
%
% ?- game_state(GS),
%    GS = game_state(0, [Obj1, Obj2], ...),
%    Obj1 = game_object(1, ...),
%    Obj2 = game_object(1, ...).  % ← INVALID!
%
% maplist/2 cannot see across list elements to enforce
% uniqueness. This requires a global constraint.

% ================================================================
% SOLUTION: all_distinct/1 + NextID Logic
% ================================================================

% Add to game_state/1 (after bounded_list_of for Objects):

game_state(
  game_state(Frame, Objects, Status, Score, 
             NextID, Commands, RevHints)
) :-
    Frame #>= 0,
    bounded_list_of(game_object, Objects, 1000),
    
    % Enforce unique IDs across all objects
    maplist(get_object_id, Objects, IDs),
    all_distinct(IDs),
    
    % NextID must exceed all existing IDs
    (IDs = [] -> MaxID = -1 ; max_list(IDs, MaxID)),
    NextID #> MaxID,
    
    game_status(Status),
    Score #>= 0,
    % Remove standalone: NextID #>= 0  (replaced by above)
    bounded_list_of(command, Commands, 100),
    bounded_list_of(rev_hint, RevHints, 1000).

% Helper to extract ID from game_object
get_object_id(game_object(ID, _, _, _, _), ID).

% ================================================================
% NOTES
% ================================================================

% LABELING (Out of Scope - TBD):
% The constraint system produces abstract templates with
% unbound FD variables. To generate concrete game states,
% you'll eventually need:
%
% concrete_game_state(GS) :-
%     game_state(GS),
%     term_variables(GS, Vars),
%     labeling([ff], Vars).
%
% This is deferred until actual generation use cases emerge.

% COLLISION CONSTRAINTS (Known Issue - Low Priority):
% The author is aware that collision/1 constraints are
% currently subpar (accept any term). This is considered
% easy to fix and will be addressed when collision logic
% is finalized. It does not affect the core architecture.
```
</addendum_3>

<addendum_4>
```prolog
% ================================================================
% Bidirectional Game State Constraints (v2) - ADDENDUM 5
% Optimizing ID Constraints Using Ascending Order
% ================================================================

% READ THIS AFTER v2 + ADDENDUM 1 + ADDENDUM 2 + ADDENDUM 3 
% + ADDENDUM 4.
% This optimizes ID uniqueness from O(N²) to O(N).

% ================================================================
% OBSERVATION: Objects Are Always Sorted by ID in Ascending Order
% ================================================================

% When objects spawn, they are appended to the end of the list
% with monotonically increasing IDs:
%
% Frame 1: Objects = [obj(0)]
% Frame 2: Objects = [obj(0), obj(1)]
% Frame 3: Objects = [obj(0), obj(1), obj(2)]
%
% IDs are in ASCENDING order: lowest at head, highest at tail.
% New objects with higher IDs are always added to the tail.
% Despawning removes objects from anywhere in the list, but new
% spawns always append to the end.

% ================================================================
% OPTIMIZATION: Use chain/2 Instead of all_distinct/1
% ================================================================

% Replace in game_state/1 (from Addendum 3/4):

game_state(
  game_state(Frame, Objects, Status, Score, 
             NextID, Commands, RevHints)
) :-
    Frame #>= 0,
    bounded_list_of(game_object, Objects, 1000),
    
    % Extract IDs
    maplist(get_object_id, Objects, IDs),
    
    % Enforce ascending order (implicitly ensures uniqueness)
    chain(IDs, #<),
    
    % Tail of list is the maximum ID (if list non-empty)
    (IDs = [] -> MaxID = -1 ; last(IDs, MaxID)),
    NextID #> MaxID,
    
    game_status(Status),
    Score #>= 0,
    bounded_list_of(command, Commands, 100),
    bounded_list_of(rev_hint, RevHints, 1000).

% ================================================================
% PERFORMANCE IMPROVEMENT
% ================================================================

% all_distinct(IDs):  O(N²) constraint propagation
% chain(IDs, #<):     O(N) constraint propagation
%
% For 1000 objects, this is ~1000x faster during generation.
% Validation of ground terms remains fast in both cases.
```
</addendum_4>
</spec>