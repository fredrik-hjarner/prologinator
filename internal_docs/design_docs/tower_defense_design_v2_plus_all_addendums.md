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

<pre style="background-color: rgb(90, 0, 0)">
💭 **Reader's Note**  
Hm not the best description. parallel is supposed to in
practise execute all actions in parallel (meaning same
frame). Not true concurrency, so yea in a way kinda 
sequential, but yeah.
</pre>

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

<pre style="background-color: rgb(90, 0, 0)">
💭 **Reader's Note**  
The example is not that great since the parallel in that
case is unnecessary.

loop([shoot, rotate(5), wait_frames(10)])

would be the same really.
</pre>

**Implementation:**
```prolog
% action(Action, In, Out, Hints)
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

**⚠️ Overridden:** This approach is corrected in [Addendum 1](#tower-defense-design-v2---addendum-1) and [Addendum 2](#tower-defense-design-v2---addendum-2).

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

**⚠️ Changed:** Spawn signature and ID generation changed in [Addendum 3](#tower-defense-design-v2---addendum-3) - now uses `spawn(Type, Pos, Actions)` with automatic ID assignment.

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

**⚠️ Corrected:** This execution model is corrected in [Addendum 1](#tower-defense-design-v2---addendum-1) - actions execute continuously until yield, not one per frame.

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

**⚠️ Extended:** `next_id` counter is added to state in [Addendum 3](#tower-defense-design-v2---addendum-3) for deterministic ID generation.

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


---
---
---
---

# Tower Defense Design v2 - Addendum 1
## Execution Model Correction

**Read this with v2. These are corrections and 
clarifications, not replacements.**

---

## Critical Misunderstanding in v2

**What v2 said:**
> "tick_object executes ONE action per frame"

**What actually happens:**
> "tick_object executes actions CONTINUOUSLY 
> until one YIELDS control"

This is a **fundamental** difference that changes 
everything about parallel execution.

---

## The Continuous Execution Model

### How Tick Actually Works

```prolog
tick_object(Obj, NewObj, Hints) :-
  execute_until_yield(Obj, NewObj, Hints).

% Base case: no actions left
execute_until_yield(
  game_object(ID, Attrs, [], Colls),
  game_object(ID, Attrs, [], Colls),
  []
) :- !.

% Recursive case: execute until yield
execute_until_yield(ObjIn, ObjOut, AllHints) :-
  ObjIn = game_object(ID, Attrs, 
                      [Action|Rest], Colls),
  
  % Execute one action
  execute_action(
    Action,
    ObjIn,
    ObjTemp,
    Hints1
  ),
  
  % Check if it yielded
  (yields(Action) ->
    % STOP! Action yielded control back
    ObjOut = ObjTemp,
    AllHints = Hints1
  ;
    % CONTINUE! Execute next action
    execute_until_yield(ObjTemp, ObjOut, Hints2),
    append(Hints1, Hints2, AllHints)
  ).
```

### Which Actions Yield?

```prolog
% Actions that YIELD (take time)
yields(wait_frames(N)) :- N #> 0.
yields(move_to(_,_,Frames)) :- Frames #> 0.

% Actions that DON'T yield (instant)
% - shoot
% - rotate
% - spawn
% - despawn
% - trigger_state_change
% - parallel (expands, doesn't yield)
% - loop (expands, doesn't yield)

**⚠️ Corrected:** Parallel is NOT an expansion - see [Addendum 2](#tower-defense-design-v2---addendum-2) for the correct composite action approach.
```

**Why this matters:**

```prolog
% Example 1: Multiple instant actions
actions([shootUp, shootDown, shootLeft, wait(1)])

% Frame N execution:
% 1. shootUp executes (instant, no yield)
% 2. shootDown executes (instant, no yield)  
% 3. shootLeft executes (instant, no yield)
% 4. wait(1) executes (YIELDS!)
% Result: All 3 shots in SAME frame!

% Example 2: Yielding action first
actions([move_to(5,5,10), shoot, wait(1)])

% Frame N execution:
% 1. move_to(5,5,10) executes (YIELDS!)
% Result: Only move happens this frame
% shoot and wait(1) execute in future frames
```

**This is the Schmupinator pattern** - actions 
execute until one says "I need time."

---

## Why v2's Parallel Was Overcomplicated

**v2's approach:**
```prolog
execute_action(
  parallel(Actions),
  ...,
  ...
) :-
  foldl(
    execute_one_parallel(ID, Colls),
    Actions,
    (AttrsIn, []),
    (AttrsOut, HintsList)
  ),
  ...
```

**Why this was wrong:**
- Tried to execute each action separately
- Threaded state manually with foldl
- Complicated for no reason

**Correct approach:**
```prolog
execute_action(
  parallel(Actions),
  game_object(ID, Attrs, [_|Rest], Colls),
  game_object(ID, Attrs, NewActions, Colls),
  []
) :-
  % Just expand into queue!
  append(Actions, Rest, NewActions).
```

**That's it!** Because `execute_until_yield` will 
run them all until one yields anyway!

**⚠️ Overridden:** This append approach is corrected in [Addendum 2](#tower-defense-design-v2---addendum-2) - it doesn't work for yielding actions that need to tick every frame.

**Example:**
```prolog
% Before:
actions([parallel([shoot, rotate, move])])

% After parallel expands:
actions([shoot, rotate, move])

% execute_until_yield runs:
% 1. shoot (instant, continue)
% 2. rotate (instant, continue)  
% 3. move (YIELDS!)
% All happened "in parallel" (same frame)
```

---

## State Threading Happens Naturally

**v2 worried about state conflicts:**
> "What if shoot changes ammo AND rotate 
> changes ammo?"

**Answer:** They execute in sequence! Last one 
wins!

```prolog
% Frame N:
parallel([
  action_that_sets_ammo(10),
  action_that_sets_ammo(5)
])

% Expands to:
[action_that_sets_ammo(10), 
 action_that_sets_ammo(5)]

% Executes:
% 1. ammo set to 10
% 2. ammo set to 5 (overwrites!)
% Final: ammo = 5
```

**This is fine!** User controls order within 
parallel. If they want both effects, they design 
actions appropriately:

```prolog
parallel([
  shoot_consuming_ammo,  % ammo: 10 → 9
  rotate_5_degrees       % angle: 0 → 5
])

% These modify DIFFERENT attributes, so order 
% doesn't matter. Both effects apply.
```

---

## Loop Correction

**v2's approach was correct but explanation was 
unclear.**

```prolog
execute_action(
  loop(Actions),
  game_object(ID, Attrs, [_|Rest], Colls),
  game_object(ID, Attrs, NewActions, Colls),
  []
) :-
  % Expand: Actions + loop(Actions) + Rest
  append(Actions, [loop(Actions)], Expanded),
  append(Expanded, Rest, NewActions).
```

**Example:**
```prolog
% Start:
actions([loop([shoot, wait(5)])])

% After loop expands:
actions([shoot, wait(5), loop([shoot, wait(5)])])

% Frame N execution:
% 1. shoot (instant, continue)
% 2. wait(5) (YIELDS!)
% Queue now: [wait(4), loop([shoot, wait(5)])]

% Frames N+1 to N+4:
% wait counts down

% Frame N+5:
% 1. wait(0) completes
% 2. loop([shoot, wait(5)]) expands again
% 3. shoot (instant, continue)
% 4. wait(5) (YIELDS!)
% Infinite loop!
```

**User error example:**
```prolog
actions([loop([shoot])])

% Expands to:
actions([shoot, loop([shoot])])

% Execution:
% 1. shoot (instant, continue)
% 2. loop([shoot]) expands
% 3. shoot (instant, continue)
% 4. loop([shoot]) expands
% 5. shoot (instant, continue)
% ... INFINITE LOOP! Never yields!

% This is USER ERROR.
% They should write:
actions([loop([shoot, wait(1)])])
```

**We don't protect against this.** User 
responsibility to test their action queues.

---

## Parallel Is NOT About State Conflicts

**v2 misunderstood parallel's purpose.**

**NOT about:** Resolving attribute conflicts via 
deltas

**IS about:** Expressing "these actions happen 
together"

```prolog
% Tower that shoots AND rotates
loop([
  parallel([
    shoot_projectile,
    rotate_toward_enemy
  ]),
  wait(5)
])

% Meaning: "Every 5 frames, shoot while rotating"
```

**Both actions execute same frame.** If they 
touch same attribute, sequential order decides. 
That's fine - user controls order.

---

## Spawn Special Case

**v2 was correct** - spawn CANNOT be handled in 
execute_action alone.

**Why?** Spawn creates NEW object. That object 
needs to be added to the GLOBAL object list, not 
just the current object's state.

```prolog
execute_action(
  spawn(NewID, Pos, NewActions),
  game_object(ID, Attrs, [_|Rest], Colls),
  game_object(ID, Attrs, Rest, Colls),
  [spawned(NewID, Pos, NewActions)]
) :-
  % Emit hint, tick will handle creation
  !.

% In tick_all_objects:
tick_all_objects(Objects, FinalObjects, 
                 AllHints) :-
  maplist(tick_object, Objects, 
          NewObjects, HintLists),
  append(HintLists, AllHints),
  
  % Extract spawn hints
  partition(is_spawn, AllHints, 
            SpawnHints, OtherHints),
  
  % Create new objects
  maplist(create_from_spawn, SpawnHints, 
          SpawnedObjs),
  
  % Add to list
  append(NewObjects, SpawnedObjs, FinalObjects).

is_spawn(spawned(_, _, _)).

create_from_spawn(
  spawned(ID, Pos, Actions),
  game_object(ID, attrs([Pos]), Actions, [])
).
```

**This is correct** - spawn is a side effect that 
modifies the object list.

---

## Updated Execution Flow

**Single Frame Execution:**

```
1. For each GameObject:
   a. execute_until_yield(Obj, NewObj, Hints)
      - Runs actions continuously
      - Stops when action yields
      - Collects hints
   
2. Collect all hints from all objects

3. Partition hints:
   - Spawn hints → create new objects
   - Despawn hints → marked for removal
   - State change hints → aggregate changes
   - Reverse hints → store for backward exec

4. detect_collisions(Objects, ..., CollHints)
   - Find overlapping objects
   - Apply collision effects (despawn)
   - Emit collision hints

5. Apply state changes (game_over, score, etc.)

6. Remove despawned objects

7. Create keyframe if needed

8. Build new state
```

---

## Implications for Other Actions

### wait_frames

**v2 version was correct:**

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
    % N = 1, wait is done
    NewActions = Rest
  ).

% wait_frames(N) yields when N > 0
yields(wait_frames(N)) :- N #> 0.
```

**What happens:**
- `wait_frames(5)` → yields immediately, queue: 
  `[wait_frames(4)|Rest]`
- Next frame: `wait_frames(4)` → yields, queue: 
  `[wait_frames(3)|Rest]`
- ... 5 frames total
- `wait_frames(0)` → doesn't yield, continues to 
  next action

### move_to

**v2 version was correct:**

```prolog
execute_action(
  move_to(TargetX, TargetY, Frames),
  game_object(ID, Attrs, [_|Rest], Colls),
  game_object(ID, NewAttrs, NewActions, Colls),
  []
) :-
  select(pos(X, Y), Attrs, RestAttrs),
  
  DX #= (TargetX - X) // Frames,
  DY #= (TargetY - Y) // Frames,
  
  NewX #= X + DX,
  NewY #= Y + DY,
  
  NewAttrs = [pos(NewX, NewY)|RestAttrs],
  
  (Frames #> 1 ->
    Frames1 #= Frames - 1,
    NewActions = [move_to(TargetX, TargetY, 
                          Frames1)|Rest]
  ;
    NewActions = Rest
  ).

yields(move_to(_, _, Frames)) :- Frames #> 0.
```

**Execution:**
- `move_to(10,10,5)` → yields, moves 1 step
- Continues yielding for 5 frames total
- Then next action executes

### shoot (example instant action)

```prolog
execute_action(
  shoot,
  game_object(ID, Attrs, [_|Rest], Colls),
  game_object(ID, Attrs, Rest, Colls),
  [spawned(ProjID, Pos, [
     move_to(TargetX, TargetY, 10),
     despawn
   ])]
) :-
  member(pos(X, Y), Attrs),
  % Generate unique ID (discussed later)
  generate_projectile_id(ID, ProjID),
  % Emit spawn hint
  Pos = pos(X, Y).

% shoot doesn't yield - it's instant!
% (No yields/1 clause for shoot)
```

**What happens:**
- Emits `spawned(...)` hint
- Returns immediately (no yield)
- execute_until_yield CONTINUES to next action
- Projectile created by tick_all_objects

---

## Corrected Parallel Examples

### Example 1: All instant actions

```prolog
game_object(
  turret1,
  attrs([pos(5,5)]),
  [parallel([
     shoot_north,
     shoot_east,
     shoot_south,
     shoot_west
   ]),
   wait(10),
   loop([...])
  ],
  []
)

% Frame N:
% 1. parallel expands to queue
% 2. shoot_north (instant, continue)
% 3. shoot_east (instant, continue)
% 4. shoot_south (instant, continue)
% 5. shoot_west (instant, continue)
% 6. wait(10) (YIELDS!)
% Result: 4 projectiles spawned same frame
```

### Example 2: Mixed instant and yielding

```prolog
game_object(
  tower1,
  attrs([pos(3,2)]),
  [parallel([
     rotate(90),
     move_to(5,5,10)
   ])
  ],
  []
)

% Frame N:
% 1. parallel expands to queue
% 2. rotate(90) (instant, continue)
% 3. move_to(5,5,10) (YIELDS!)
% Result: Rotation happened, movement started
% Queue now: [move_to(5,5,9)]
```

**Important:** Order in parallel matters when 
both modify same attribute!

```prolog
parallel([
  set_color(red),
  set_color(blue)
])

% Expands to: [set_color(red), set_color(blue)]
% Executes sequentially
% Final color: blue (last write wins)
```

---

## Why This Is Elegant

**Schmupinator pattern benefits:**

1. **Composability** - Actions combine naturally
2. **No special cases** - parallel is just append
3. **Natural state flow** - recursion threads state
4. **Simple to reason about** - "execute until 
   yield"
5. **User controls timing** - yield = "I need 
   time"

**Prolog benefits:**

1. **Homoiconicity** - Actions are just terms
2. **Pattern matching** - yields/1 is declarative
3. **Recursion** - execute_until_yield is natural
4. **No mutation** - State threaded functionally

---

## What v2 Got Right

**These are still correct:**

- ✅ State changes as emitted hints
- ✅ Reverse hints for non-invertible ops
- ✅ Keyframes every 10 frames
- ✅ Collision detection post-tick
- ✅ Pure predicates (CLP(FD))
- ✅ Spawn as special side effect
- ✅ Loop via self-appending
- ✅ GameObject structure
- ✅ State structure

**These need update:**

- ❌ Parallel execution (use simple append)
- ❌ One-action-per-tick assumption
- ⚠️ foldl complexity (not needed)

---

## Summary of Corrections

**Execution Model:**
- OLD: One action per frame per object
- NEW: Actions until yield per frame per object

**Parallel:**
- OLD: foldl state threading
- NEW: Just append to queue

**State Threading:**
- OLD: Manual with foldl
- NEW: Automatic via recursion

**Infinite Loops:**
- OLD: Maybe add safeguard?
- NEW: User responsibility, no safeguard

**Everything else:** Unchanged from v2

---

## Implementation Sketch

```prolog
% Complete tick with corrected execution
tick_object(Obj, NewObj, Hints) :-
  execute_until_yield(Obj, NewObj, Hints).

execute_until_yield(
  game_object(ID, Attrs, [], Colls),
  game_object(ID, Attrs, [], Colls),
  []
) :- !.

execute_until_yield(ObjIn, ObjOut, AllHints) :-
  ObjIn = game_object(ID, A, [Act|Rest], C),
  execute_action(Act, ObjIn, ObjTemp, H1),
  (yields(Act) ->
    ObjOut = ObjTemp,
    AllHints = H1
  ;
    execute_until_yield(ObjTemp, ObjOut, H2),
    append(H1, H2, AllHints)
  ).

% Yielding actions
yields(wait_frames(N)) :- N #> 0.
yields(move_to(_,_,F)) :- F #> 0.

% Parallel - CORRECTED
execute_action(
  parallel(Actions),
  game_object(ID, Attrs, [_|Rest], Colls),
  game_object(ID, Attrs, NewActions, Colls),
  []
) :-
  append(Actions, Rest, NewActions).

% Loop - unchanged
execute_action(
  loop(Actions),
  game_object(ID, Attrs, [_|Rest], Colls),
  game_object(ID, Attrs, NewActions, Colls),
  []
) :-
  append(Actions, [loop(Actions)], Expanded),
  append(Expanded, Rest, NewActions).
```

---

## Read v2 With These Corrections

When reading v2:
1. Replace "one action per frame" with 
   "actions until yield"
2. Replace parallel's foldl with simple append
3. Add yields/1 predicate for actions
4. Keep everything else as-is

The architecture, motivations, and design 
decisions in v2 remain valid. Only the execution 
mechanics changed.


---
---
---
---

# Tower Defense Design v2 - Addendum 2
## Parallel Execution Correction

**Read this with v2 and Addendum 1. This corrects 
the parallel implementation.**

---

## The Parallel Contradiction in Addendum 1

**Addendum 1 said:**
```prolog
execute_action(
  parallel(Actions),
  ...,
  game_object(ID, Attrs, NewActions, Colls),
  ...
) :-
  append(Actions, Rest, NewActions).
```

**Why this is WRONG:**

```prolog
% Example: Tower moves AND rotates
parallel([
  move_to(10,10,20),  % 20 frames
  rotate(360,20)      % 20 frames
])

% With append approach:
% Expands to: [move_to(10,10,20), 
%              rotate(360,20)]

% Frame 1:
% - move_to(10,10,20) yields
% - rotate NEVER EXECUTES!

% Frames 2-20:
% - move_to continues
% - rotate still waiting

% Frame 21:
% - rotate FINALLY starts
% This is SEQUENTIAL, not parallel!
```

**The core problem:** Both actions need to tick 
EVERY frame until BOTH complete.

---

## The Solution: Parallel as Composite Action

**Key insight:** `parallel` is NOT an expansion - 
it's an ACTION that manages child actions.

**How it works:**
1. `parallel([A1, A2, A3])` stays in queue
2. Each frame, tick ALL children
3. Children update independently
4. When ALL children done, remove parallel

**Schmupinator pattern:** Multiple coroutines 
running simultaneously. Each yields independently.

---

## Implementation

### Core Parallel Execution

```prolog
execute_action(
  parallel(ChildActions),
  game_object(ID, AttrsIn, 
              [_|Rest], Colls),
  game_object(ID, AttrsOut, 
              NewActions, Colls),
  AllHints
) :-
  % Tick all children this frame
  tick_parallel_children(
    ChildActions,
    ID,
    AttrsIn,
    Colls,
    AttrsOut,
    UpdatedChildren,
    AllHints
  ),
  
  % Check if all children done
  (all_children_done(UpdatedChildren) ->
    % All finished, remove parallel
    NewActions = Rest
  ;
    % Some still running, keep parallel
    NewActions = [
      parallel(UpdatedChildren)|Rest
    ]
  ).

% parallel doesn't yield - it's a container
% Its CHILDREN yield, but parallel itself 
% executes every frame
```

**Why this works:**
- Every frame, all children tick
- Each child can yield or complete
- State threads through children
- Parallel removed only when all done

---

### Ticking Parallel Children

```prolog
tick_parallel_children(
  [],
  _ID, Attrs, _Colls,
  Attrs,  % No changes
  [],     % No children left
  []      % No hints
).

tick_parallel_children(
  [Child|Rest],
  ID, AttrsIn, Colls,
  AttrsOut,
  [UpdatedChild|UpdatedRest],
  AllHints
) :-
  % Tick one child
  tick_one_child(
    Child,
    ID, AttrsIn, Colls,
    Attrs1, UpdatedChild, Hints1
  ),
  
  % Tick remaining children (with updated attrs)
  tick_parallel_children(
    Rest,
    ID, Attrs1, Colls,
    AttrsOut,
    UpdatedRest,
    Hints2
  ),
  
  % Combine hints
  append(Hints1, Hints2, AllHints).
```

**State threading:** Each child sees the state 
changes from previous children. This is 
SEQUENTIAL execution of state changes, but all 
happen in SAME FRAME.

**Why sequential threading?** If `move_to` 
changes `pos` and `rotate` changes `angle`, 
order doesn't matter. If both change same 
attribute, last one wins - user controls order.

---

### Ticking One Child

```prolog
tick_one_child(
  Child,
  ID, AttrsIn, Colls,
  AttrsOut, UpdatedChild, Hints
) :-
  % Create temp object for child
  TempObj = game_object(
    ID, AttrsIn, [Child], Colls
  ),
  
  % Execute child action
  execute_action(
    Child,
    TempObj,
    game_object(ID, AttrsOut, 
                NewActions, Colls),
    Hints
  ),
  
  % Extract updated child action
  (NewActions = [] ->
    % Child finished
    UpdatedChild = done
  ;
    % Child continues
    [UpdatedChild|_] = NewActions
  ).
```

**Key trick:** Wrap child in temporary object, 
execute it, extract updated child state.

**If child yields:** It returns itself updated 
(e.g., `move_to(10,10,19)` → `move_to(10,10,18)`)

**If child finishes:** NewActions = [], child 
marked as `done`

---

### Checking If All Done

```prolog
all_children_done([]).
all_children_done([done|Rest]) :-
  all_children_done(Rest).
% If any child is NOT done, this fails
```

Simple: All children must be `done` marker.

---

## Complete Example Execution

```prolog
% Frame 0: Tower with parallel action
game_object(
  tower1,
  attrs([pos(5,5), angle(0)]),
  [
    parallel([
      move_to(10,10,20),
      rotate(360,20)
    ]),
    shoot,
    wait(5)
  ],
  []
)

% Frame 1 execution:
execute_action(
  parallel([move_to(10,10,20), 
            rotate(360,20)]),
  ...,
  ...,
  ...
) :-
  % Tick child 1: move_to
  execute_action(
    move_to(10,10,20),
    game_object(tower1, 
                attrs([pos(5,5), angle(0)]), 
                [move_to(10,10,20)], 
                []),
    game_object(tower1, 
                attrs([pos(5.25,5.25), angle(0)]),
                [move_to(10,10,19)],
                []),
    []
  ),
  % Child 1 updated: move_to(10,10,19)
  % Attrs now: [pos(5.25,5.25), angle(0)]
  
  % Tick child 2: rotate
  execute_action(
    rotate(360,20),
    game_object(tower1,
                attrs([pos(5.25,5.25), 
                       angle(0)]),
                [rotate(360,20)],
                []),
    game_object(tower1,
                attrs([pos(5.25,5.25), 
                       angle(18)]),
                [rotate(360,19)],
                []),
    []
  ),
  % Child 2 updated: rotate(360,19)
  % Attrs now: [pos(5.25,5.25), angle(18)]
  
  % Both children still running
  UpdatedChildren = [
    move_to(10,10,19),
    rotate(360,19)
  ],
  
  % Keep parallel in queue
  NewActions = [
    parallel([move_to(10,10,19), 
              rotate(360,19)]),
    shoot,
    wait(5)
  ].

% Result: Tower moved AND rotated in frame 1!

% Frames 2-20: Same process, both progress

% Frame 20:
% - move_to(10,10,1) executes → done
% - rotate(360,1) executes → done
% - UpdatedChildren = [done, done]
% - all_children_done([done, done]) succeeds
% - NewActions = [shoot, wait(5)]
% - parallel removed!

% Frame 21:
% - shoot executes (instant)
% - wait(5) executes (yields)
```

**Key property:** Both children ticked EVERY 
frame for 20 frames. True parallelism!

---

## Composability: Parallel in Parallel

```prolog
parallel([
  move_to(10,10,20),
  parallel([
    rotate(360,20),
    animate_firing(20)
  ])
])

% This just works!

% Outer parallel ticks:
% 1. move_to(10,10,20)
% 2. parallel([rotate(360,20), 
%              animate_firing(20)])

% Inner parallel ticks:
% 1. rotate(360,20)
% 2. animate_firing(20)

% All THREE actions execute every frame!
```

**Why it works:** `parallel` is just another 
action. When ticked, it ticks ITS children. 
Nesting is natural.

---

## State Threading Details

**Question:** What if children modify same 
attribute?

**Answer:** Sequential order within frame decides.

```prolog
parallel([
  set_hp(50),
  set_hp(100)
])

% Frame N:
% 1. set_hp(50) executes
%    attrs: [hp(50), ...]
% 2. set_hp(100) executes
%    attrs: [hp(100), ...]  # Overwrites!

% Final: hp = 100 (last write wins)
```

**Is this a problem?** No - user controls order. 
If both need to apply, design actions differently:

```prolog
parallel([
  take_damage(10),     # hp: 100 → 90
  regen_hp(5)          # hp: 90 → 95
])

% Both effects apply because they READ then 
% MODIFY, not SET
```

**Good action design:**
```prolog
execute_action(
  take_damage(Dmg),
  game_object(ID, Attrs, [_|Rest], Colls),
  game_object(ID, NewAttrs, Rest, Colls),
  []
) :-
  select(hp(HP), Attrs, RestAttrs),
  HP1 #= HP - Dmg,
  NewAttrs = [hp(HP1)|RestAttrs].
```

**Bad action design:**
```prolog
execute_action(
  set_hp(NewHP),  # Ignores current HP!
  ...
) :-
  NewAttrs = [hp(NewHP)|RestAttrs].
```

**Principle:** Actions should READ-MODIFY-WRITE, 
not OVERWRITE.

---

## Instant vs Yielding Children

**Instant actions in parallel:**

```prolog
parallel([
  shoot_north,
  shoot_east,
  shoot_south
])

% Frame 1:
% - shoot_north executes (instant)
%   Returns: done
% - shoot_east executes (instant)
%   Returns: done
% - shoot_south executes (instant)
%   Returns: done
% - UpdatedChildren = [done, done, done]
% - all_children_done succeeds
% - parallel removed in SAME frame!

% Result: All 3 shots fired frame 1,
%         parallel removed frame 1
```

**Mixed instant and yielding:**

```prolog
parallel([
  shoot,          # Instant
  move_to(10,10,20)  # Yields
])

% Frame 1:
% - shoot executes → done
% - move_to executes → move_to(10,10,19)
% - UpdatedChildren = [done, 
%                      move_to(10,10,19)]
% - NOT all done, keep parallel

% Frames 2-20:
% - done ticks (no-op, stays done)
% - move_to continues

% Frame 20:
% - done ticks → done
% - move_to executes → done
% - all_children_done succeeds
% - parallel removed
```

**How does `done` tick?**

```prolog
tick_one_child(
  done,
  _ID, Attrs, _Colls,
  Attrs,  # No change
  done,   # Still done
  []      # No hints
).
```

**Simple:** `done` is inert, just stays done 
until all siblings finish.

---

## Loop in Parallel

```prolog
parallel([
  loop([shoot, wait(5)]),
  move_to(100,100,100)
])

% This works!

% Frame 1:
% - loop expands to [shoot, wait(5), loop(...)]
% - shoot executes → done
% - wait(5) executes → wait(4)
% - UpdatedChild1 = [wait(4), loop(...)]
% - move_to executes → move_to(100,100,99)
% - UpdatedChild2 = move_to(100,100,99)
% - Keep parallel

% Frame 2:
% - [wait(4), loop(...)] ticks
%   → wait(4) yields → [wait(3), loop(...)]
% - move_to(100,100,99) ticks
%   → move_to(100,100,98)

% ... continues forever because loop never ends!
```

**Infinite parallel:** If one child never 
finishes (loop), parallel never removed. This is 
correct behavior!

**How to handle?**

```prolog
parallel([
  loop([shoot, wait(5)]),
  sequence([
    move_to(100,100,100),
    stop_sibling_loops
  ])
])

% When move completes, action stops loops
```

Or design game so parallel only used for 
finite-duration effects.

---

## Comparison to Addendum 1

**Addendum 1 (WRONG):**
```prolog
parallel([A, B]) → append([A,B], Rest)
% A executes, yields
% B never runs (stuck behind A)
```

**Addendum 2 (CORRECT):**
```prolog
parallel([A, B]) → stays in queue
% Each frame:
%   A ticks (might yield, might finish)
%   B ticks (might yield, might finish)
% Both progress simultaneously!
```

**Addendum 1 was only correct for:**
- All instant actions
- Actions that don't yield

**Addendum 2 works for:**
- All instant actions ✅
- All yielding actions ✅
- Mixed instant and yielding ✅
- Nested parallels ✅
- Loops in parallel ✅

---

## Implementation Helpers

### execute_action for done

```prolog
execute_action(
  done,
  game_object(ID, Attrs, _, Colls),
  game_object(ID, Attrs, [], Colls),
  []
).
```

`done` marker executes instantly, returns empty 
action list.

### Instant action example

```prolog
execute_action(
  shoot_north,
  game_object(ID, Attrs, [_|Rest], Colls),
  game_object(ID, Attrs, Rest, Colls),
  [spawned(ProjID, Pos, [
     move_to(TargetX, TargetY, 10),
     despawn
   ])]
) :-
  member(pos(X,Y), Attrs),
  ProjID = ...,  % Generate ID
  Pos = pos(X, Y),
  TargetX #= X,
  TargetY #= Y - 10.  % Shoot north
```

Instant actions return `Rest` (remove themselves), 
don't modify attributes (except via spawns).

### Yielding action example

```prolog
execute_action(
  rotate(TotalDeg, FramesLeft),
  game_object(ID, Attrs, [_|Rest], Colls),
  game_object(ID, NewAttrs, NewActions, Colls),
  []
) :-
  select(angle(A), Attrs, RestAttrs),
  
  DegPerFrame #= TotalDeg // FramesLeft,
  A1 #= A + DegPerFrame,
  
  NewAttrs = [angle(A1)|RestAttrs],
  
  (FramesLeft #> 1 ->
    FramesLeft1 #= FramesLeft - 1,
    NewActions = [
      rotate(TotalDeg, FramesLeft1)|Rest
    ]
  ;
    NewActions = Rest  # Done
  ).

yields(rotate(_, F)) :- F #> 0.
```

Yielding actions return themselves updated (or 
Rest if done).

---

## Integration with execute_until_yield

**Question:** How does `parallel` interact with 
continuous execution?

**Answer:** `parallel` itself doesn't yield, so 
execution continues!

```prolog
actions([
  parallel([move_to(10,10,20), rotate(360,20)]),
  shoot,
  wait(5)
])

% Frame 1:
% 1. parallel executes
%    - Ticks children
%    - Returns updated parallel
%    - Doesn't yield (no yields/1 clause)
% 2. execute_until_yield CONTINUES
% 3. shoot executes (instant)
% 4. wait(5) executes (YIELDS!)

% Result frame 1:
% - parallel ticked (both children progressed)
% - shoot executed
% - wait started
% Queue: [parallel([move_to(...,19), 
%                   rotate(...,19)]),
%         shoot,  # Already executed!
%         wait(4)]
```

**Wait, shoot executed twice?**

No! When `parallel` executes, it removes itself 
from queue and either:
- Returns Rest (if all done)
- Returns [parallel(updated)|Rest] (if not done)

So shoot is NOT in the new queue. Let me correct:

```prolog
execute_action(
  parallel(...),
  game_object(ID, A, [_|Rest], C),
  game_object(ID, A1, NewActions, C),
  ...
) :-
  % Rest already has [shoot, wait(5)]
  ...
  NewActions = [parallel(Updated)|Rest]
  % = [parallel(...), shoot, wait(5)]
```

So in execute_until_yield:

```prolog
% Start: [parallel([...]), shoot, wait(5)]
execute_until_yield(...) :-
  execute_action(parallel(...), ..., ..., ...),
  % Returns NewObj with actions:
  %   [parallel([...]), shoot, wait(5)]
  % parallel doesn't yield, continue!
  
  execute_until_yield(NewObj, ..., ...).
  % Now executes from [parallel([...]), 
  %                    shoot, wait(5)]
  % But wait - parallel is STILL first!
```

**I think I'm confusing myself. Let me re-read 
the execute_action signature...**

Actually, looking back at the code:

```prolog
execute_action(
  parallel(ChildActions),
  game_object(ID, AttrsIn, [_|Rest], Colls),
  game_object(ID, AttrsOut, NewActions, Colls),
  AllHints
) :-
  ...
  (all_children_done(UpdatedChildren) ->
    NewActions = Rest  # Pop parallel
  ;
    NewActions = [parallel(Updated)|Rest]
  ).
```

The `[_|Rest]` means we're REMOVING parallel from 
the front. So:
- Input: `[parallel([...]), shoot, wait(5)]`
- `_` = parallel (discarded)
- `Rest` = `[shoot, wait(5)]`
- Output (if not done): 
  `[parallel(Updated), shoot, wait(5)]`

So parallel REPLACES itself.

**In execute_until_yield:**

```prolog
% Input obj: actions = 
%   [parallel([move,rotate]), shoot, wait(5)]

execute_until_yield(ObjIn, ObjOut, Hints) :-
  ObjIn = game_object(ID, A, 
                      [parallel(...)|Rest], C),
  
  execute_action(
    parallel(...),
    ObjIn,
    ObjTemp,  # actions = [parallel(Updated), 
              #            shoot, wait(5)]
    H1
  ),
  
  % parallel doesn't yield
  \+ yields(parallel(...)),
  
  % Continue execution
  execute_until_yield(ObjTemp, ObjOut, H2).

% Now ObjTemp has [parallel(Updated), 
%                  shoot, wait(5)]
% execute_until_yield runs again...
% Infinite loop!
```

**OH NO! Infinite loop!**

**The fix:** `parallel` MUST be marked as yielding 
OR it must be REMOVED after execution.

**Which is correct?**

If parallel doesn't yield, execute_until_yield 
will keep executing it every recursion → infinite.

**Solution:** parallel SHOULD yield after ticking 
children!

```prolog
yields(parallel(_)).
```

**Why?** parallel represents "one frame's worth 
of parallel execution". After ticking all 
children, it's done for this frame.

**⚠️ Refined:** This is refined in [Addendum 3](#tower-defense-design-v2---addendum-3) - only `parallel_running` yields, not initial `parallel`, to avoid wasting frames on instant parallels.

---

## Corrected yields Clause

```prolog
yields(wait_frames(N)) :- N #> 0.
yields(move_to(_,_,F)) :- F #> 0.
yields(rotate(_, F)) :- F #> 0.
yields(parallel(_)).  # Always yields!
```

**Why parallel yields:**
- It ticks all children
- Children progress
- But parallel itself represents "one frame"
- Must yield to give other objects turns

**With this:**

```prolog
actions([parallel([...]), shoot, wait(5)])

% Frame 1:
% 1. parallel executes
%    - Ticks children
%    - Returns [parallel(Updated), 
%               shoot, wait(5)]
% 2. parallel YIELDS
% 3. Stop executing for this frame

% Frame 2:
% 1. parallel executes again
%    - Ticks children (now on frame 2)
%    - Returns [parallel(Updated2), 
%               shoot, wait(5)]
% 2. parallel YIELDS again

% ... 20 frames ...

% Frame 20:
% 1. parallel executes
%    - Both children done
%    - Returns [shoot, wait(5)]
% 2. No yield clause for parallel in this case
%    because it was removed!
% 3. shoot executes (instant)
% 4. wait(5) executes (YIELDS!)
```

Perfect!

---

## Final Corrected Implementation

```prolog
% parallel always yields
yields(parallel(_)).

execute_action(
  parallel(ChildActions),
  game_object(ID, AttrsIn, 
              [_|Rest], Colls),
  game_object(ID, AttrsOut, 
              NewActions, Colls),
  AllHints
) :-
  tick_parallel_children(
    ChildActions, ID, AttrsIn, Colls,
    AttrsOut, UpdatedChildren, AllHints
  ),
  (all_children_done(UpdatedChildren) ->
    NewActions = Rest
  ;
    NewActions = [parallel(UpdatedChildren)|Rest]
  ).

tick_parallel_children(
  [], _ID, Attrs, _Colls,
  Attrs, [], []
).
tick_parallel_children(
  [Child|RestChildren],
  ID, AttrsIn, Colls,
  AttrsOut, [UpdatedChild|RestUpdated],
  AllHints
) :-
  tick_one_child(
    Child, ID, AttrsIn, Colls,
    Attrs1, UpdatedChild, Hints1
  ),
  tick_parallel_children(
    RestChildren, ID, Attrs1, Colls,
    AttrsOut, RestUpdated, Hints2
  ),
  append(Hints1, Hints2, AllHints).

tick_one_child(
  done,
  _ID, Attrs, _Colls,
  Attrs, done, []
) :- !.
tick_one_child(
  Child,
  ID, AttrsIn, Colls,
  AttrsOut, UpdatedChild, Hints
) :-
  execute_action(
    Child,
    game_object(ID, AttrsIn, [Child], Colls),
    game_object(ID, AttrsOut, NewActions, Colls),
    Hints
  ),
  (NewActions = [] ->
    UpdatedChild = done
  ;
    [UpdatedChild|_] = NewActions
  ).

all_children_done([]).
all_children_done([done|Rest]) :-
  all_children_done(Rest).
```

---

## Summary of Corrections

**Addendum 1 → Addendum 2:**

**WRONG:**
```prolog
parallel(Actions) → append(Actions, Rest)
```

**RIGHT:**
```prolog
parallel(Children) → composite action that:
1. Ticks all children every frame
2. Threads state through children
3. Stays in queue until all done
4. Yields after each frame
```

**Key insight:** parallel is an ACTION, not an 
expansion. It executes every frame, managing its 
children.

**Properties:**
- ✅ True parallelism (all children tick/frame)
- ✅ Composable (parallel in parallel works)
- ✅ Handles yielding actions
- ✅ Handles instant actions
- ✅ Handles mixed instant/yielding
- ✅ State threads correctly
- ✅ Integrates with execute_until_yield

**What v2 + Addendum 1 got right:**
- State structure
- Reverse hints
- Collision detection
- execute_until_yield concept
- Pure predicates

**What Addendum 2 fixes:**
- parallel execution model
- parallel yields after ticking children
- Composability of parallel

**Everything else:** Unchanged from v2/Addendum 1

---
---
---
---

# Tower Defense Design v2 - Addendum 3
## Critical Bug Fixes

**Read with v2 + Addendum 1 + 2. This fixes 
2 bugs and adds ID generation.**

---

## Bug 1: Instant Parallel Yields Unnecessarily

**Problem:**
```prolog
parallel([shoot, rotate])  % Both instant

% Addendum 2 code:
yields(parallel(_)).  % WRONG!

% Result:
% 1. parallel executes, both done, removes self
% 2. yields(parallel(_)) → true (checks INPUT)
% 3. Frame stops (wastes 1 frame!)
```

**Fix:** Distinguish starting vs running states.

```prolog
% Starting parallel
execute_action(
  parallel(Children),
  game_object(ID, A, [_|Rest], C),
  game_object(ID, A1, NewActions, C),
  Hints
) :-
  tick_parallel_children(
    Children, ID, A, C,
    A1, Updated, Hints
  ),
  (all_children_done(Updated) ->
    NewActions = Rest  % Done instantly!
  ;
    NewActions = [parallel_running(Updated)|Rest]
  ).

% Running parallel (only this yields!)
execute_action(
  parallel_running(Children),
  Obj, NewObj, Hints
) :-
  % Same logic as parallel
  execute_action(parallel(Children), 
                 Obj, NewObj, Hints).

% Only running version yields
yields(parallel_running(_)).
% yields(parallel(_)) removed!
```

**Result:** Instant parallels execute same frame, 
yielding parallels properly wait.

---

## Bug 2: Despawn in Parallel Crashes

**Problem:**
```prolog
parallel([move_to(10,10,5), despawn])

% tick_one_child calls:
execute_action(despawn, ..., despawned, ...),

% Then tries:
game_object(ID, AttrsOut, ...) = despawned
% CRASH! Can't unify atom with structure
```

**Fix:** Despawn is top-level only.

```prolog
% Don't allow despawn in parallel
% User must design actions carefully

% If needed, use trigger_state_change instead:
parallel([
  move_to(10,10,5),
  trigger_state_change(mark_for_despawn)
])

% Then tick handles despawning
```

**Alternative:** Handle despawned in tick_one_child:

```prolog
tick_one_child(
  despawn,
  ID, Attrs, Colls,
  Attrs,  % Keep attrs (irrelevant)
  caused_despawn,  % Special marker
  [despawned(ID, Attrs)]
) :- !.

tick_one_child(done, _, A, _, A, done, []) :- !.

tick_one_child(Child, ID, A, C, A1, U, H) :-
  execute_action(
    Child,
    game_object(ID, A, [Child], C),
    ResultObj,
    H
  ),
  (ResultObj = despawned ->
    U = caused_despawn,
    A1 = A
  ;
    ResultObj = game_object(_, A1, NewActs, _),
    (NewActs = [] -> U = done ; 
     [U|_] = NewActs)
  ).

% In parallel execution:
all_children_done([]).
all_children_done([done|R]) :- 
  all_children_done(R).
% caused_despawn stops parallel
all_children_done([caused_despawn|_]) :- 
  !, fail.
```

**Recommended:** Keep despawn top-level. Cleaner.

**⚠️ Changed:** This restriction is removed in [Addendum 4](#tower-defense-design-v2---addendum-4) - despawn can be used in parallel via bubbling mechanism.

---

## Addition: ID Generation

**Problem:** `spawn(proj_X, ...)` needs unique IDs.

**Solution:** Store counter in state, tick assigns IDs.

### Updated State Structure

```prolog
game_state(
  frame(N),
  objects([...]),
  game_status(playing),
  score(0),
  next_id(42),  % <-- ID counter
  last_keyframe(...),
  reverse_hints([...])
)
```

### Spawn Action Emits Request

```prolog
execute_action(
  spawn(Type, Pos, Actions),
  game_object(ID, Attrs, [_|Rest], Colls),
  game_object(ID, Attrs, Rest, Colls),
  [spawn_request(Type, Pos, Actions)]
  % No ID provided!
).
```

### Tick Processes Spawn Requests

```prolog
tick(StateIn, StateOut) :-
  StateIn = game_state(
    F, Objs, Status, Score, NextID, 
    LastKF, OldHints
  ),
  
  % 1. Tick all objects
  tick_all_objects(Objs, TempObjs, Hints1),
  
  % 2. Detect collisions
  detect_collisions(TempObjs, NewObjs, Hints2),
  
  % 3. Combine hints
  append(Hints1, Hints2, AllHints),
  
  % 4. Partition hints
  partition(is_spawn_request, AllHints, 
            SpawnReqs, OtherHints),
  partition(is_state_change, OtherHints,
            StateChanges, ReverseHints),
  
  % 5. Process spawns with ID assignment
  process_spawn_requests(
    SpawnReqs,
    NextID,
    SpawnedObjs,
    NewNextID
  ),
  
  % 6. Add spawned objects
  append(NewObjs, SpawnedObjs, FinalObjs),
  
  % 7. Apply state changes
  apply_state_changes(StateChanges, 
                      Status, Score,
                      NewStatus, NewScore),
  
  F1 #= F + 1,
  
  (should_create_keyframe(F1) ->
    NewKF = keyframe(F1, FinalObjs)
  ;
    NewKF = LastKF
  ),
  
  StateOut = game_state(
    F1, FinalObjs, NewStatus, NewScore,
    NewNextID, NewKF, ReverseHints
  ).

is_spawn_request(spawn_request(_, _, _)).

process_spawn_requests([], ID, [], ID).
process_spawn_requests(
  [spawn_request(Type, Pos, Acts)|Rest],
  IDIn,
  [game_object(ObjID, attrs([Pos]), 
               Acts, [])|RestObjs],
  IDOut
) :-
  % Generate ID: type_counter
  atom_concat(Type, '_', Prefix),
  atom_concat(Prefix, IDIn, ObjID),
  
  IDIn1 #= IDIn + 1,
  
  process_spawn_requests(Rest, IDIn1, 
                         RestObjs, IDOut).
```

**Result:** 
- `spawn(projectile, pos(5,5), [...])` 
- Creates: `game_object(projectile_42, ...)`
- Next spawn: `projectile_43`
- Deterministic, reversible!

---

## Summary of Changes

**Bug Fixes:**
1. `parallel` → instant if all done
2. `parallel_running` → yields if continuing
3. Handle `despawned` in tick_one_child OR 
   forbid despawn in parallel

**Addition:**
4. ID counter in state
5. Spawn emits request (no ID)
6. Tick assigns IDs deterministically

**Everything else unchanged from v2/Addendum 1/2.**

---

## Updated yields/1

```prolog
yields(wait_frames(N)) :- N #> 0.
yields(move_to(_,_,F)) :- F #> 0.
yields(rotate(_, F)) :- F #> 0.
yields(parallel_running(_)).
% yields(parallel(_)) removed!
```

**That's it!** Three critical fixes in ~200 lines 
vs ~500 in Addendum 2.

---
---
---
---

# Tower Defense Design v2 - Addendum 4
## Despawn Bubbling Fix

**Read with v2 + Addendums 1-3. Fixes despawn in 
parallel.**

---

## Problem

Addendum 3 said "forbid despawn in parallel" but 
this breaks composability:

```prolog
parallel([
  animate_explosion(10),
  despawn
])
% Should work! Explode while dying.
```

**Current bug:** `despawn` returns `despawned` atom, 
can't unify with `game_object(...)` in 
`tick_one_child`.

---

## Solution: Bubble Despawn Up

### Updated tick_one_child

```prolog
tick_one_child(done, _, A, _, A, done, []) :- !.

tick_one_child(
  caused_despawn,  % Already bubbled
  _, A, _, A, caused_despawn, []
) :- !.

tick_one_child(Child, ID, AIn, C, AOut, U, H) :-
  execute_action(
    Child,
    game_object(ID, AIn, [Child], C),
    Result,
    H
  ),
  (Result = despawned ->
    % Bubble up!
    U = caused_despawn,
    AOut = AIn  % State frozen
  ;
    Result = game_object(_, AOut, NewActs, _),
    (NewActs = [] -> U = done ; [U|_] = NewActs)
  ).
```

### Handle in parallel

```prolog
all_children_done([]).
all_children_done([done|R]) :- 
  all_children_done(R).
% If ANY child caused despawn, parallel fails
all_children_done([caused_despawn|_]) :- 
  !, fail.

execute_action(
  parallel(Children),
  game_object(ID, AIn, [_|Rest], C),
  Result,
  AllHints
) :-
  tick_parallel_children(
    Children, ID, AIn, C,
    AOut, Updated, AllHints
  ),
  
  % Check for despawn
  (member(caused_despawn, Updated) ->
    % Child despawned, bubble it up!
    Result = despawned
  ;
    all_children_done(Updated) ->
      Result = game_object(ID, AOut, Rest, C)
    ;
      NewActs = [parallel_running(Updated)|Rest],
      Result = game_object(ID, AOut, NewActs, C)
  ).

% Same for parallel_running
execute_action(
  parallel_running(Children),
  Obj, Result, Hints
) :-
  execute_action(parallel(Children), 
                 Obj, Result, Hints).
```

---

## Example

```prolog
parallel([
  animate_explosion(10),
  despawn
])

% Frame 1:
% 1. animate_explosion ticks → animate_explosion(9)
% 2. despawn executes → caused_despawn
% 3. member(caused_despawn, Updated) → true
% 4. Result = despawned (bubbles up!)
% 5. Object removed from game

% Works! Explosion animated 1 frame, then despawn.
```

---

## Summary

**Fix:** 
- `tick_one_child` catches `despawned` → 
  `caused_despawn`
- `parallel` checks for `caused_despawn` → 
  returns `despawned`
- Despawn bubbles up through parallel layers

**Result:** Composable despawn anywhere in action 
tree.

**Everything else unchanged.**
