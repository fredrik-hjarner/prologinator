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
