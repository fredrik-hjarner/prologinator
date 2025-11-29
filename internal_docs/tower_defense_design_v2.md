# Tower Defense Game Design v2

## Core Philosophy

**Data-Driven, Action-Centric Design**

Every game behavior lives in GameObject action 
queues. The engine executes actions without 
knowing their meaning. Win conditions, scoring, 
level transitions - all emitted by actions as 
state changes.

**Why?** This is the Schmupinator pattern - 
makes the game JSON-serializable, inspectable, 
and composable. In Prolog, we get homoiconicity 
for free since actions ARE data (terms).

---

## Design Decisions & Motivations

### Decision 1: Parallel as Sequential Threading

**What:** `parallel([A1, A2, A3])` executes each 
action in sequence, threading state through.

**Why NOT deltas?** Each action modifies different 
attributes (shoot changes ammo, rotate changes 
angle, move changes pos). Sequential execution 
with state threading handles this naturally.

**Example:**
```prolog
% Tower needs to shoot AND rotate
actions([
  loop([
    parallel([
      shoot,         % Uses ammo attr
      rotate(5)      % Uses angle attr
    ]),
    wait_frames(10)
  ])
])

% Execution:
% 1. shoot: attrs([ammo(9), angle(0)]) 
%         → attrs([ammo(8), angle(0)])
% 2. rotate: attrs([ammo(8), angle(0)])
%          → attrs([ammo(8), angle(5)])
% Result: Both happened "same frame"
```

**Implementation:**
```prolog
execute_action(
  parallel(Actions),
  game_object(ID, AttrsIn, [_|Rest], Colls),
  game_object(ID, AttrsOut, Rest, Colls),
  AllHints
) :-
  % Thread state through all actions
  foldl(
    execute_one_parallel(ID, Colls),
    Actions,
    (AttrsIn, []),
    (AttrsOut, HintsList)
  ),
  append(HintsList, AllHints).

execute_one_parallel(ID, Colls, Action, 
                     (AttrsIn, HintsAcc),
                     (AttrsOut, [H|HintsAcc])) :-
  execute_action(
    Action,
    game_object(ID, AttrsIn, [Action], Colls),
    game_object(ID, AttrsOut, _, Colls),
    H
  ).
```

**Motivation:** Simple, no special cases, 
leverages Prolog's natural state threading.

---

### Decision 2: State Changes as Emitted Hints

**What:** Actions emit `state_change(Change)` 
in their hints. Tick aggregates these and applies 
to game state.

**Why NOT hardcoded checks?** Any action should 
be able to change any state for any reason. The 
engine shouldn't know what "winning" means.

**Example:**
```prolog
% Enemy reaches goal
execute_action(
  trigger_state_change(game_over(lost)),
  game_object(ID, Attrs, [_|Rest], Colls),
  game_object(ID, Attrs, Rest, Colls),
  [state_change(game_over(lost))]  % Emitted!
).

% Collect coin
execute_action(
  collect_coin,
  ...
  [state_change(score(+10))]  % Different change!
).

% Unlock achievement
execute_action(
  unlock_achievement(speedrun),
  ...
  [state_change(achievement(speedrun))]
).
```

**Aggregation in tick:**
```prolog
tick(StateIn, StateOut) :-
  StateIn = game_state(F, Objs, GameStatus, 
                       Score, ..., LastKF, 
                       OldHints),
  
  % 1. Tick all objects
  tick_all_objects(Objs, TempObjs, Hints1),
  
  % 2. Detect collisions
  detect_collisions(TempObjs, NewObjs, Hints2),
  
  % 3. Combine hints
  append(Hints1, Hints2, AllHints),
  
  % 4. Partition state changes from reverse hints
  partition(
    is_state_change,
    AllHints,
    StateChanges,
    ReverseHints
  ),
  
  % 5. Apply state changes
  apply_state_changes(
    StateChanges,
    GameStatus,
    Score,
    NewGameStatus,
    NewScore
  ),
  
  F1 #= F + 1,
  
  % 6. Keyframe check
  (should_create_keyframe(F1) ->
    NewLastKF = keyframe(frame(F1), NewObjs)
  ;
    NewLastKF = LastKF
  ),
  
  StateOut = game_state(
    F1, NewObjs, NewGameStatus, NewScore, 
    ..., NewLastKF, ReverseHints
  ).

% Helper: identify state change hints
is_state_change(state_change(_)).

% Apply with priority: lost > won > playing
apply_state_changes([], Status, Score, 
                    Status, Score).
apply_state_changes(
  [state_change(game_over(lost))|_],
  _, Score, lost, Score
) :- !.
apply_state_changes(
  [state_change(game_over(won))|Rest],
  Status, Score, FinalStatus, FinalScore
) :-
  (Status = lost ->
    FinalStatus = lost, FinalScore = Score
  ;
    apply_state_changes(
      Rest, won, Score, 
      FinalStatus, FinalScore
    )
  ).
apply_state_changes(
  [state_change(score(Delta))|Rest],
  Status, Score, FinalStatus, FinalScore
) :-
  NewScore #= Score + Delta,
  apply_state_changes(
    Rest, Status, NewScore,
    FinalStatus, FinalScore
  ).
```

**Motivation:** Maximum flexibility. Adding new 
state (health, mana, level) requires NO engine 
changes - just new state_change types.

---

### Decision 3: No RNG (Yet)

**What:** Skip randomness entirely in v2.

**Why?** YAGNI. Current game design has:
- Deterministic tower shooting (loop-based)
- Deterministic enemy movement (scripted paths)
- Deterministic wave spawning (timed)

**When to add:** When we actually implement 
random enemy spawn positions, random tower 
targeting, or random powerups.

**How we'll add later:**
```prolog
game_state(
  ...,
  rng_state(seed(12345), index(42))
)

execute_action(
  move_random_direction,
  game_object(ID, Attrs, Actions, Colls),
  game_object(ID, NewAttrs, NewActions, Colls),
  [random_used(index(42), value(3))]
) :-
  % Get RNG from state (passed in)
  get_rng_value(42, 3),
  % Use 3 to determine direction
  ...
```

**Motivation:** Don't design for problems you 
don't have. Current game is fully deterministic.

---

### Decision 4: Collision Detection Architecture

**What:** Run collision detection AFTER all 
objects tick, store collision IDs on each 
GameObject for ONE frame.

**Why?** Separates concerns:
1. **Object ticking** - executes actions
2. **Collision detection** - detects overlaps
3. **Collision effects** - despawns colliding objs

**Flow:**
```
tick_all_objects(Objs, TempObjs, Hints1)
  ↓
  All objects moved/acted
  ↓
detect_collisions(TempObjs, FinalObjs, Hints2)
  ↓
  Collisions detected, effects applied
  ↓
Combine hints: append(Hints1, Hints2, AllHints)
```

**Implementation:**
```prolog
detect_collisions(Objects, NewObjects, Hints) :-
  % Find all colliding pairs
  findall(
    collision(ID1, ID2),
    (
      member(
        game_object(ID1, attrs(A1), _, _),
        Objects
      ),
      member(
        game_object(ID2, attrs(A2), _, _),
        Objects
      ),
      ID1 @< ID2,  % Avoid duplicates
      member(pos(X1,Y1), A1),
      member(pos(X2,Y2), A2),
      collides_at(X1, Y1, X2, Y2)
    ),
    Collisions
  ),
  
  % Handle collision effects
  handle_collisions(
    Objects,
    Collisions,
    NewObjects,
    Hints
  ).

% Grid-based: same position = collision
collides_at(X, Y, X, Y).

% Handle projectile+enemy collisions
handle_collisions(Objects, Collisions, 
                  NewObjects, Hints) :-
  % Find proj+enemy pairs
  findall(
    (ProjID, EnemyID),
    (
      member(collision(ID1, ID2), Collisions),
      ((is_proj(ID1), is_enemy(ID2),
        ProjID=ID1, EnemyID=ID2) ;
       (is_enemy(ID1), is_proj(ID2),
        ProjID=ID2, EnemyID=ID1))
    ),
    ToRemove
  ),
  
  % Extract all IDs to remove
  findall(ID, member((ID, _), ToRemove), P1),
  findall(ID, member((_, ID), ToRemove), P2),
  append(P1, P2, AllIDs),
  list_to_set(AllIDs, UniqueIDs),
  
  % Remove and collect hints
  remove_with_hints(
    Objects,
    UniqueIDs,
    NewObjects,
    Hints
  ).

% ID prefixes for type checking
is_proj(ID) :- atom_concat(proj, _, ID).
is_enemy(ID) :- atom_concat(enemy, _, ID).

% Remove objects, record as hints
remove_with_hints([], _, [], []).
remove_with_hints(
  [game_object(ID, Attrs, _, _)|Rest],
  ToRemove,
  NewObjs,
  [despawned(ID, Attrs)|RestHints]
) :-
  member(ID, ToRemove),
  !,
  remove_with_hints(Rest, ToRemove, 
                    NewObjs, RestHints).
remove_with_hints([Obj|Rest], ToRemove,
                  [Obj|NewObjs], Hints) :-
  remove_with_hints(Rest, ToRemove, 
                    NewObjs, Hints).
```

**Motivation:** Clean separation. Collision 
detection is ONE concern, handled in ONE place. 
Actions don't need to know about physics.

---

### Decision 5: Loop via Self-Appending

**What:** `loop(Actions)` re-appends itself after 
executing Actions.

**Why?** Self-referential data structure. The 
action contains its own continuation.

**Implementation:**
```prolog
execute_action(
  loop(Actions),
  game_object(ID, Attrs, [_|Rest], Colls),
  game_object(ID, Attrs, NewActions, Colls),
  []  % No hints needed
) :-
  append(Actions, [loop(Actions)], Expanded),
  append(Expanded, Rest, NewActions).
```

**Example:**
```prolog
% Start: [loop([wait(5), shoot]), despawn]
% After 1 tick: [wait(4), shoot, loop([...]), 
%                despawn]
% After 5 ticks: [shoot, loop([...]), despawn]
% After 6 ticks: [wait(5), shoot, loop([...]), 
%                 despawn]
% Infinite loop!
```

**Motivation:** No special "loop counter" state. 
Loop is just data transformation.

---

### Decision 6: Spawn Creates New GameObject

**What:** `spawn(ID, Pos, Actions)` adds new 
object to game state.

**Why?** Spawning is a SIDE EFFECT - it modifies 
the object list. Can't be handled in 
execute_action alone.

**Implementation:**
```prolog
execute_action(
  spawn(NewID, Pos, NewActions),
  game_object(ID, Attrs, [_|Rest], Colls),
  game_object(ID, Attrs, Rest, Colls),
  [spawned(NewID, Pos, NewActions)]  % Hint!
).

% Tick handles spawned hints
tick_all_objects(Objects, FinalObjects, 
                 AllHints) :-
  maplist(tick_object, Objects, 
          NewObjects, HintLists),
  append(HintLists, AllHints),
  
  % Extract spawn requests
  partition(is_spawn, AllHints, 
            SpawnHints, OtherHints),
  
  % Create new objects from spawns
  maplist(create_from_spawn, SpawnHints, 
          SpawnedObjs),
  
  % Add to object list
  append(NewObjects, SpawnedObjs, FinalObjects),
  
  % Return only non-spawn hints
  AllHints = OtherHints.

is_spawn(spawned(_, _, _)).

create_from_spawn(
  spawned(ID, Pos, Actions),
  game_object(ID, attrs([Pos]), Actions, [])
).
```

**Motivation:** Spawn is special - it creates NEW 
execution contexts. Needs coordination at tick 
level.

---

### Decision 7: Sequential as Default Queue

**What:** Action list `[A1, A2, A3]` executes in 
order automatically.

**Why?** No need for explicit `sequential(...)` 
wrapper. It's the default.

**Implementation:**
```prolog
tick_object(
  game_object(ID, Attrs, [], Colls),
  game_object(ID, Attrs, [], Colls),
  []
) :- !.  % No actions left

tick_object(
  game_object(ID, Attrs, [Action|Rest], Colls),
  NewObj,
  Hints
) :-
  execute_action(
    Action,
    game_object(ID, Attrs, [Action|Rest], Colls),
    NewObj,
    Hints
  ).
```

**Motivation:** Simplicity. Most actions are 
sequential. Don't add wrapper for common case.

---

## State Structure

```prolog
game_state(
  frame(N),              % Current frame number
  objects([              % All active GameObjects
    game_object(
      id(tower1),
      attrs([pos(3,2), ammo(10)]),
      actions([loop([...])]),
      collisions([])     % This frame's collisions
    ),
    ...
  ]),
  game_status(playing),  % playing|won|lost
  score(0),              % Current score
  last_keyframe(         % Most recent keyframe
    frame(K),
    keyframe_objects([...])
  ),
  reverse_hints([        % For backward execution
    despawned(enemy1, attrs([pos(5,4)]))
  ])
)
```

**Why this structure?**

- `frame(N)` - Explicit time, needed for keyframes
- `objects([...])` - All active execution contexts
- `game_status` - Derived from actions, not 
  hardcoded
- `score` - Example mutable game state
- `last_keyframe` - For fast seeking
- `reverse_hints` - ONLY for non-invertible ops

**Why collisions on GameObject?** So actions can 
check "did I collide?" via pattern matching:

```prolog
execute_action(
  damage_on_collision,
  game_object(ID, Attrs, [_|Rest], Colls),
  game_object(ID, NewAttrs, Rest, Colls),
  []
) :-
  (Colls = [] ->
    NewAttrs = Attrs  % No collision, no damage
  ;
    % Collision! Take damage
    select(hp(HP), Attrs, RestAttrs),
    HP1 #= HP - 10,
    NewAttrs = [hp(HP1)|RestAttrs]
  ).
```

---

## GameObject Structure

```prolog
game_object(
  id(enemy1),            % Unique identifier
  attrs([               % Key-value attributes
    pos(5,4),
    hp(100)
  ]),
  actions([             % Execution queue
    move_to(7,4,20),
    trigger_state_change(game_over(lost))
  ]),
  collisions([])        % IDs of collided objects
)
```

**Why attrs as list?** Allows dynamic attributes:

```prolog
% Add new attribute
select(pos(X,Y), Attrs, Rest),
NewAttrs = [pos(X,Y), velocity(2,0)|Rest]

% Check if attribute exists
(member(frozen(true), Attrs) -> 
  % Skip movement 
; 
  % Normal movement
)
```

**Why actions as list?** Natural queue structure:

```prolog
% Execute first, keep rest
[Action|Rest]

% Append new actions
append(Actions, [loop(Actions)], NewQueue)
```

---

## Action Types

### Basic Actions

**wait_frames(N)**
- Count down N frames
- Reversible (just count up)

```prolog
execute_action(
  wait_frames(N),
  game_object(ID, Attrs, [_|Rest], Colls),
  game_object(ID, Attrs, NewActions, Colls),
  []
) :-
  (N #> 1 ->
    N1 #= N - 1,
    NewActions = [wait_frames(N1)|Rest]
  ;
    NewActions = Rest  % Done waiting
  ).
```

**move_to(X, Y, Frames)**
- Linear interpolation over Frames
- Reversible (compute previous position)

```prolog
execute_action(
  move_to(TargetX, TargetY, Frames),
  game_object(ID, Attrs, [_|Rest], Colls),
  game_object(ID, NewAttrs, NewActions, Colls),
  []
) :-
  select(pos(CurrentX, CurrentY), Attrs, 
         RestAttrs),
  
  % Compute step
  DX #= (TargetX - CurrentX) // Frames,
  DY #= (TargetY - CurrentY) // Frames,
  
  NewX #= CurrentX + DX,
  NewY #= CurrentY + DY,
  
  NewAttrs = [pos(NewX, NewY)|RestAttrs],
  
  (Frames #> 1 ->
    Frames1 #= Frames - 1,
    NewActions = [
      move_to(TargetX, TargetY, Frames1)|Rest
    ]
  ;
    NewActions = Rest  % Arrived
  ).
```

**despawn**
- Remove self from game
- NOT reversible - needs hint!

```prolog
execute_action(
  despawn,
  game_object(ID, Attrs, _, Colls),
  despawned,  % Special marker
  [despawned(ID, Attrs)]  % Reverse hint!
) :- !.

% Tick filters out despawned objects
tick_all_objects(Objects, FinalObjects, 
                 AllHints) :-
  maplist(tick_object, Objects, 
          TempObjects, HintLists),
  append(HintLists, AllHints),
  
  % Remove despawned markers
  exclude(is_despawned, TempObjects, 
          FinalObjects).

is_despawned(despawned).
```

### Compound Actions

**spawn(ID, Pos, Actions)**
- Create new GameObject
- Emits `spawned(...)` hint
- Tick handles creation

```prolog
execute_action(
  spawn(NewID, Pos, NewActions),
  game_object(ID, Attrs, [_|Rest], Colls),
  game_object(ID, Attrs, Rest, Colls),
  [spawned(NewID, Pos, NewActions)]
).
```

**parallel(Actions)**
- Execute all actions "simultaneously"
- Thread state through sequentially

```prolog
execute_action(
  parallel(Actions),
  game_object(ID, AttrsIn, [_|Rest], Colls),
  game_object(ID, AttrsOut, Rest, Colls),
  AllHints
) :-
  foldl(
    execute_one_parallel(ID, Colls),
    Actions,
    (AttrsIn, []),
    (AttrsOut, HintsList)
  ),
  append(HintsList, AllHints).

execute_one_parallel(ID, Colls, Action,
                     (AttrsIn, HintsAcc),
                     (AttrsOut, [H|HintsAcc])) :-
  execute_action(
    Action,
    game_object(ID, AttrsIn, [Action], Colls),
    game_object(ID, AttrsOut, _, Colls),
    H
  ).
```

**loop(Actions)**
- Repeat Actions infinitely
- Self-appending queue

```prolog
execute_action(
  loop(Actions),
  game_object(ID, Attrs, [_|Rest], Colls),
  game_object(ID, Attrs, NewActions, Colls),
  []
) :-
  append(Actions, [loop(Actions)], Expanded),
  append(Expanded, Rest, NewActions).
```

**trigger_state_change(Change)**
- Emit state change
- Tick aggregates and applies

```prolog
execute_action(
  trigger_state_change(Change),
  game_object(ID, Attrs, [_|Rest], Colls),
  game_object(ID, Attrs, Rest, Colls),
  [state_change(Change)]
).
```

---

## Complete Tick Implementation

```prolog
tick(StateIn, StateOut) :-
  StateIn = game_state(
    Frame,
    Objects,
    GameStatus,
    Score,
    LastKeyframe,
    OldHints
  ),
  
  % 1. Tick all objects
  tick_all_objects(Objects, TempObjects, 
                   Hints1),
  
  % 2. Detect collisions
  detect_collisions(TempObjects, NewObjects, 
                    Hints2),
  
  % 3. Combine all hints
  append(Hints1, Hints2, AllHints),
  
  % 4. Partition state changes
  partition(
    is_state_change,
    AllHints,
    StateChanges,
    ReverseHints
  ),
  
  % 5. Apply state changes
  apply_state_changes(
    StateChanges,
    GameStatus,
    Score,
    NewGameStatus,
    NewScore
  ),
  
  % 6. Increment frame
  Frame1 #= Frame + 1,
  
  % 7. Create keyframe if needed
  (should_create_keyframe(Frame1) ->
    NewKeyframe = keyframe(Frame1, NewObjects)
  ;
    NewKeyframe = LastKeyframe
  ),
  
  % 8. Build new state
  StateOut = game_state(
    Frame1,
    NewObjects,
    NewGameStatus,
    NewScore,
    NewKeyframe,
    ReverseHints
  ).

% Helper: keyframe every 10 frames
should_create_keyframe(F) :-
  F mod 10 #= 0.
```

---

## Example Game Timeline

```prolog
% Frame 0: Initial state
game_state(
  frame(0),
  objects([
    game_object(
      tower1,
      attrs([pos(3,2)]),
      [loop([
        wait_frames(5),
        spawn(
          proj_N,  % N = counter
          pos(3,2),
          [move_to(0,4,10), despawn]
        )
      ])],
      []
    ),
    game_object(
      spawner,
      attrs([]),
      [
        wait_frames(10),
        spawn(
          enemy1,
          pos(0,4),
          [
            move_to(7,4,20),
            trigger_state_change(
              game_over(lost)
            )
          ]
        )
      ],
      []
    )
  ]),
  game_status(playing),
  score(0),
  keyframe(frame(0), []),
  []
).

% Frame 5: Tower shoots
% - Tower executes wait_frames(0) → done
% - Tower executes spawn(proj1, ...)
% - Tick creates proj1

% Frame 10: Enemy spawns
% - Spawner executes spawn(enemy1, ...)
% - Tick creates enemy1

% Frame 15: Collision!
% - proj1 at pos(0,4)
% - enemy1 at pos(3,4)
% ... more ticks ...
% Frame 25: They collide at pos(0,4)
% - detect_collisions finds collision
% - Both despawned
% - Hints: [
%     despawned(proj1, attrs([pos(0,4)])),
%     despawned(enemy1, attrs([pos(0,4)]))
%   ]

% Frame 30: Enemy2 reaches goal
% - enemy2 executes move_to(7,4,0) → done
% - enemy2 executes trigger_state_change(
%     game_over(lost)
%   )
% - Tick applies: GameStatus = lost
% - Game stops
```

---

## Features Demonstrated

### ✅ Fully Implemented

1. **Homoiconicity**
   - Actions are terms
   - Can inspect: `Action = spawn(_, _, _)`
   - Can transform: `append(A, B, NewQueue)`

2. **Bidirectionality**
   - All arithmetic uses `#=/2`, `#</2`, etc.
   - Can compute backwards: 
     `move_to(X,Y,F), NewX #= OldX + DX`
     Can solve for OldX!

3. **Reverse Hints**
   - Only for non-invertible ops (despawn, spawn)
   - Stored in state for backward execution
   - Example: `despawned(enemy1, attrs([...]))`

4. **Keyframes**
   - Every 10 frames snapshot state
   - Enables fast seeking to frame N
   - Structure present, usage demonstrated

5. **Compound Actions**
   - `parallel([...])` - foldl threading
   - `loop([...])` - self-appending
   - `spawn(...)` - creates new context

6. **Collision Detection**
   - Per-frame, post-tick
   - Stores IDs on GameObject
   - Handles effects (despawn)

7. **Pure Predicates**
   - No `var/1`, `is/2`, cuts
   - Uses CLP(FD) throughout
   - Pattern matching for control flow

### ⚠️ Structure Present, Demo Deferred

8. **Constraint Interpolation**
   - Keyframes enable this
   - Can reconstruct frame 35 from KF 30 & 40
   - Example query deferred to implementation

9. **Backward Execution**
   - Reverse hints enable this
   - Can tick backwards using hints
   - Full implementation deferred

### ❌ Deliberately Excluded

- **RNG** - Not needed yet (YAGNI)
- **Level Generation** - Out of scope
- **History Queries** - Out of scope
- **HP System** - Simplified to one-hit

---

## Open Questions for v3

1. **Spawn ID generation** - How to make unique 
   IDs? (proj_1, proj_2, etc.)

2. **Backward execution** - Should we implement 
   full tick_backward/2 or just document?

3. **Interpolation demo** - Include working query 
   or wait for implementation?

4. **File organization** - Still single file or 
   split into:
   - state.pl (state structure)
   - actions.pl (execute_action clauses)
   - tick.pl (main game loop)
   - collisions.pl (collision detection)

5. **Actual win condition** - How does spawner 
   know all waves done? Count or explicit marker?

---

## Summary of Changes from v1

**Simplified:**
- ✅ Parallel execution (foldl threading)
- ✅ State changes (generalized, not just win/lose)
- ✅ Removed RNG scaffolding

**Clarified:**
- ✅ Motivations for each decision
- ✅ Why NOT other approaches
- ✅ Expected behavior with examples

**Kept:**
- ✅ Collision detection post-tick
- ✅ Reverse hints for despawn/spawn
- ✅ Keyframe structure
- ✅ Pure predicates throughout

**Next iteration needs:**
- ID generation strategy
- Full backward execution implementation
- Working interpolation query
- File organization decision
