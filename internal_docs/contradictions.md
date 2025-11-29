# Contradictions in Tower Defense Design v2 + Addendums

This document lists contradictions found in the design document, particularly where later addendums override earlier statements.

---

## 1. Parallel Execution Implementation

### v2 (Lines 54-79)
**States:** `parallel` uses `foldl` to thread state through actions sequentially.

```prolog
execute_action(
  parallel(Actions),
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

### Addendum 1 (Lines 1132-1141)
**Overrides v2:** Says to use simple `append` instead of foldl.

```prolog
execute_action(
  parallel(Actions),
  ...
) :-
  % Just expand into queue!
  append(Actions, Rest, NewActions).
```

**Says:** "That's it! Because `execute_until_yield` will run them all until one yields anyway!"

### Addendum 2 (Lines 1696-1733)
**Overrides Addendum 1:** Shows that append approach is WRONG for yielding actions.

**States:** "The core problem: Both actions need to tick EVERY frame until BOTH complete."

**Correct approach:** `parallel` is a composite action that stays in queue and ticks all children every frame.

### Addendum 3 (Lines 2658-2708)
**Refines Addendum 2:** Adds `parallel_running` distinction to fix instant parallel bug.

**Contradiction:** Addendum 1's "simple append" approach is completely wrong for yielding actions, only works for all-instant actions.

---

## 2. Execution Model

### v2 (Line 1013)
**States:** "tick_object executes ONE action per frame"

### Addendum 1 (Lines 1010-1020)
**Overrides v2:** "What v2 said: 'tick_object executes ONE action per frame'"

**Corrected to:** "tick_object executes actions CONTINUOUSLY until one YIELDS control"

**States:** "This is a **fundamental** difference that changes everything about parallel execution."

---

## 3. Parallel Yielding Behavior

### Addendum 2 (Lines 2466-2483)
**States:** `parallel` always yields after ticking children.

```prolog
yields(parallel(_)).  % Always yields!
```

**Reasoning:** "parallel represents 'one frame's worth of parallel execution'. After ticking all children, it's done for this frame."

### Addendum 3 (Lines 2658-2708)
**Overrides Addendum 2:** Distinguishes `parallel` (starting) vs `parallel_running` (running).

**States:** Only `parallel_running` yields, not initial `parallel`.

```prolog
yields(parallel_running(_)).
% yields(parallel(_)) removed!
```

**Reasoning:** Instant parallels should execute same frame without yielding.

**Contradiction:** Addendum 2 said parallel always yields, but Addendum 3 shows this causes unnecessary frame waste for instant parallels.

---

## 4. Despawn in Parallel

### Addendum 3 (Lines 2712-2779)
**States:** "Despawn is top-level only."

**Says:** "Don't allow despawn in parallel. User must design actions carefully."

**Alternative provided:** Use `trigger_state_change(mark_for_despawn)` instead.

**Also provides:** Alternative implementation to handle `despawned` in `tick_one_child`, but recommends keeping despawn top-level.

### Addendum 4 (Lines 2931-3063)
**Overrides Addendum 3:** Allows despawn in parallel via bubbling mechanism.

**States:** "Addendum 3 said 'forbid despawn in parallel' but this breaks composability"

**Example:** `parallel([animate_explosion(10), despawn])` should work.

**Solution:** Bubble `despawned` up through parallel layers as `caused_despawn`.

**Contradiction:** Addendum 3 forbids it, Addendum 4 allows it with proper handling.

---

## 5. Spawn Action Signature

### v2 (Lines 406-412, 669-680)
**States:** `spawn(NewID, Pos, NewActions)` - takes explicit ID.

```prolog
execute_action(
  spawn(NewID, Pos, NewActions),
  ...
  [spawned(NewID, Pos, NewActions)]
).
```

### Addendum 3 (Lines 2803-2812)
**Overrides v2:** Changes to `spawn(Type, Pos, Actions)` - no ID provided.

```prolog
execute_action(
  spawn(Type, Pos, Actions),
  ...
  [spawn_request(Type, Pos, Actions)]
  % No ID provided!
).
```

**Reasoning:** ID generation should be deterministic via counter in state.

**Contradiction:** v2 expects caller to provide ID, Addendum 3 has tick assign IDs automatically.

---

## 6. Spawn Hint Format

### v2 (Lines 406-412, 435-440)
**Uses:** `spawned(NewID, Pos, NewActions)`

```prolog
is_spawn(spawned(_, _, _)).

create_from_spawn(
  spawned(ID, Pos, Actions),
  game_object(ID, attrs([Pos]), Actions, [])
).
```

### Addendum 3 (Lines 2815-2886)
**Overrides v2:** Changes to `spawn_request(Type, Pos, Actions)`

```prolog
is_spawn_request(spawn_request(_, _, _)).

process_spawn_requests(
  [spawn_request(Type, Pos, Acts)|Rest],
  IDIn,
  [game_object(ObjID, attrs([Pos]), Acts, [])|RestObjs],
  IDOut
) :-
  % Generate ID: type_counter
  atom_concat(Type, '_', Prefix),
  atom_concat(Prefix, IDIn, ObjID),
  ...
```

**Contradiction:** Different hint format, different processing logic.

---

## 7. State Structure

### v2 (Lines 485-507)
**Shows state without `next_id`:**

```prolog
game_state(
  frame(N),
  objects([...]),
  game_status(playing),
  score(0),
  last_keyframe(...),
  reverse_hints([...])
)
```

### Addendum 3 (Lines 2789-2801)
**Adds `next_id` to state:**

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

**Contradiction:** State structure changed, but v2 examples don't include this field.

---

## 8. Tick Implementation - Spawn Processing

### v2 (Lines 415-433)
**Shows spawn processing in `tick_all_objects`:**

```prolog
tick_all_objects(Objects, FinalObjects, AllHints) :-
  maplist(tick_object, Objects, NewObjects, HintLists),
  append(HintLists, AllHints),
  
  % Extract spawn requests
  partition(is_spawn, AllHints, SpawnHints, OtherHints),
  
  % Create new objects from spawns
  maplist(create_from_spawn, SpawnHints, SpawnedObjs),
  
  % Add to object list
  append(NewObjects, SpawnedObjs, FinalObjects),
  
  % Return only non-spawn hints
  AllHints = OtherHints.
```

### Addendum 3 (Lines 2817-2866)
**Moves spawn processing to main `tick` function:**

```prolog
tick(StateIn, StateOut) :-
  ...
  % 4. Partition hints
  partition(is_spawn_request, AllHints, SpawnReqs, OtherHints),
  partition(is_state_change, OtherHints, StateChanges, ReverseHints),
  
  % 5. Process spawns with ID assignment
  process_spawn_requests(SpawnReqs, NextID, SpawnedObjs, NewNextID),
  
  % 6. Add spawned objects
  append(NewObjs, SpawnedObjs, FinalObjs),
  ...
```

**Contradiction:** Different location and different processing (with ID assignment).

---

## 9. Parallel Doesn't Yield (Addendum 2)

### Addendum 2 (Lines 1791-1793)
**States:** "parallel doesn't yield - it's a container. Its CHILDREN yield, but parallel itself executes every frame"

### Addendum 2 (Lines 2321-2353)
**Later discussion reveals problem:** If parallel doesn't yield, `execute_until_yield` will infinite loop.

### Addendum 2 (Lines 2453-2473)
**Corrects itself:** "OH NO! Infinite loop! The fix: `parallel` MUST be marked as yielding"

**Final decision:** `yields(parallel(_)).` - Always yields!

**Contradiction:** Addendum 2 contradicts itself within the same document - first says parallel doesn't yield, then says it must yield.

---

## 10. Parallel Expansion vs Composite Action

### Addendum 1 (Lines 1077-1078)
**Lists actions that don't yield:**
```prolog
% - parallel (expands, doesn't yield)
% - loop (expands, doesn't yield)
```

### Addendum 2 (Lines 1740-1753)
**Overrides Addendum 1:** "Key insight: `parallel` is NOT an expansion - it's an ACTION that manages child actions."

**Contradiction:** Addendum 1 treats parallel as expansion, Addendum 2 treats it as composite action.

---

## 11. Loop in Parallel - Execution Model

### Addendum 2 (Lines 2168-2194)
**Shows loop in parallel works:**
```prolog
parallel([
  loop([shoot, wait(5)]),
  move_to(100,100,100)
])
```

**States:** "This works!" and shows execution where loop expands and continues.

**But:** The example shows `UpdatedChild1 = [wait(4), loop(...)]` - a list, not a single action.

**Contradiction:** `tick_one_child` is supposed to return a single `UpdatedChild`, but loop expansion returns a list. This is inconsistent with the `tick_one_child` signature shown earlier.

---

## 12. Spawn ID Generation Strategy

### v2 (Lines 949-952)
**Open Questions for v3:**
1. **Spawn ID generation** - How to make unique IDs? (proj_1, proj_2, etc.)

### Addendum 3 (Lines 2783-2892)
**Answers the question:** Uses counter in state with `atom_concat(Type, '_', Prefix)` and `atom_concat(Prefix, IDIn, ObjID)`.

**But:** The implementation uses `IDIn` (a number) directly in `atom_concat`, which won't work in Prolog - numbers need to be converted to atoms first.

**Contradiction:** Implementation shown is syntactically incorrect for Prolog.

---

## Summary

The document has evolved through multiple iterations, with each addendum correcting or refining previous statements. The main contradictions are:

1. **Parallel execution** - Three different approaches (foldl → append → composite action)
2. **Execution model** - One action per frame → actions until yield
3. **Parallel yielding** - Doesn't yield → always yields → only running version yields
4. **Despawn in parallel** - Forbidden → allowed with bubbling
5. **Spawn signature** - Takes ID → takes Type, ID assigned by tick
6. **State structure** - Missing next_id → includes next_id

Most contradictions are resolved by later addendums, but the document should be read sequentially to understand the final correct approach.

