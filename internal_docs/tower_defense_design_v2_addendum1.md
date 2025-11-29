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
