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
