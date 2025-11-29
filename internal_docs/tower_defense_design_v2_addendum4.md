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
