# Teaching Plan: Understanding the Tower Defense Design

A progressive learning path to understand the FINAL design (after all corrections).

---

## Prerequisites

**Before starting, you should understand:**
- Basic Prolog syntax (facts, rules, predicates)
- Lists in Prolog (`[Head|Tail]` pattern)
- Pattern matching/unification
- Basic CLP(FD) concepts (constraints like `#=`, `#>`)

**If you're new to Prolog:** Start with a basic Prolog tutorial first.

---

## Level 1: Core Concepts (Foundation)

**Goal:** Understand what the system is and why it exists.

### 1.1 The Big Idea
- **Read:** Core Philosophy section (lines 3-17)
- **Key concept:** Actions are data, engine doesn't know what they mean
- **Why it matters:** Makes everything inspectable, reversible, composable

**Checkpoint:** Can you explain why actions being "data" is powerful?

### 1.2 State Structure
- **Read:** Addendum 3, "Updated State Structure" (lines 2789-2801) - this is the FINAL version
- **Key concept:** `game_state` contains frame, objects, status, score, next_id, keyframes, hints
- **Focus on:** What each field represents, especially `next_id` for ID generation

**Checkpoint:** Can you list all fields in `game_state` and what they're for?

### 1.3 GameObject Structure
- **Read:** GameObject Structure section (lines 541-581)
- **Key concept:** Each object has ID, attributes (key-value list), actions (queue), collisions
- **Focus on:** Why attributes are a list, why actions are a queue

**Checkpoint:** Can you explain why `attrs` is a list instead of a fixed structure?

---

## Level 2: Basic Actions (Building Blocks)

**Goal:** Understand how individual actions work.

### 2.1 The execute_action Pattern
- **Read:** Any `execute_action` example (e.g., Addendum 1, lines 1422-1448 for `wait_frames`)
- **Key concept:** `execute_action(Action, ObjectIn, ObjectOut, Hints)`
  - Takes action + input object → produces output object + hints
  - This is Prolog's way of "modifying" immutable data

**Checkpoint:** Why does `execute_action` take TWO `game_object` terms?

### 2.2 Simple Actions
- **Read:** Addendum 1, "wait_frames" and "move_to" (lines 1387-1455)
- **Focus on:** `wait_frames(N)` and `move_to(X, Y, Frames)`
- **Key concept:** Actions can yield (take time) or be instant

**Exercise:** Trace through `wait_frames(3)` for 3 frames. What does the action queue look like each frame?

### 2.3 Yielding vs Instant
- **Read:** Addendum 1, "Which Actions Yield?" (lines 1064-1079)
- **Key concept:** Some actions need time (`wait_frames`, `move_to`), others are instant (`shoot`, `spawn`)
- **Important:** The `yields/1` predicate determines this

**Checkpoint:** Can you list 3 yielding actions and 3 instant actions?

---

## Level 3: Execution Model (How Things Run)

**Goal:** Understand how actions execute over time.

### 3.1 The Continuous Execution Model
- **Read:** Addendum 1, "The Continuous Execution Model" (lines 1024-1105)
- **Key concept:** Actions execute continuously until one YIELDS
- **Important:** This is the CORRECT model - not "one action per frame"

**Example to trace:**
```prolog
actions([shoot, shoot, shoot, wait_frames(5)])
```
- Frame 1: All 3 shoots execute (instant), then wait yields
- Frames 2-5: wait counts down
- Frame 6: wait completes, next action (if any) executes

**Checkpoint:** Why does `execute_until_yield` exist?

### 3.2 The Tick Function
- **Read:** Addendum 3, "Tick Processes Spawn Requests" (lines 2817-2866) - this is the FINAL version
- **Key concept:** Tick orchestrates everything:
  1. Tick all objects (execute actions)
  2. Detect collisions
  3. Process spawn requests (with ID assignment)
  4. Process state changes
  5. Update state

**Checkpoint:** What happens if two objects both emit `state_change(game_over(lost))`?

---

## Level 4: Compound Actions (Composition)

**Goal:** Understand how actions combine.

### 4.1 Loop
- **Read:** Decision 5: Loop via Self-Appending (lines 360-392)
- **Key concept:** `loop(Actions)` re-appends itself after executing Actions
- **Simple mental model:** It's like a recursive function that never stops

**Exercise:** Trace `loop([shoot, wait_frames(5)])` for 10 frames. What happens?

**Important:** Always include a yielding action in loops, or they'll infinite loop!

### 4.2 Spawn
- **Read:** Addendum 3, "Spawn Action Emits Request" (lines 2803-2812) and "Tick Processes Spawn Requests" (lines 2815-2886)
- **Key concept:** Spawn emits `spawn_request(Type, Pos, Actions)`, tick assigns IDs deterministically
- **Important:** This is the FINAL version - spawn takes Type, not ID

**Checkpoint:** Why can't spawn be handled in `execute_action` alone? Why does tick assign IDs?

---

## Level 5: Parallel Execution (The Real Thing)

**Goal:** Understand true parallel execution.

### 5.1 Parallel as Composite Action
- **Read:** Addendum 2, "The Solution: Parallel as Composite Action" (lines 1740-1801)
- **Key concept:** Parallel stays in queue, ticks ALL children every frame
- **Mental model:** Like multiple coroutines running simultaneously

**Checkpoint:** How is parallel different from just expanding to a list?

### 5.2 Parallel Implementation Details
- **Read:** Addendum 2, "Implementation" sections (lines 1756-1907)
- **Focus on:** `tick_parallel_children` and `tick_one_child`
- **Key concept:** State threads through children sequentially, but all happen same frame

**Exercise:** Trace `parallel([move_to(10,10,5), rotate(90,5)])` for 3 frames. What does each child look like each frame?

### 5.3 Parallel Yielding
- **Read:** Addendum 3, "Bug 1: Instant Parallel Yields Unnecessarily" (lines 2658-2708)
- **Key concept:** `parallel` (starting) doesn't yield, `parallel_running` (continuing) does yield
- **Why:** Instant parallels should execute same frame without wasting a frame

**Checkpoint:** Why does `parallel_running` exist?

### 5.4 Despawn in Parallel
- **Read:** Addendum 4, "Solution: Bubble Despawn Up" (lines 2958-3027)
- **Key concept:** Despawn bubbles up through parallel layers as `caused_despawn`
- **Important:** This is the FINAL version - despawn CAN be used in parallel

**Checkpoint:** How does despawn bubble up through nested parallel actions?

---

## Level 6: State Management (Side Effects)

**Goal:** Understand how state changes propagate.

### 6.1 State Changes as Hints
- **Read:** Decision 2: State Changes as Emitted Hints (lines 86-203)
- **Key concept:** Actions emit `state_change(...)`, tick aggregates and applies
- **Why:** Engine doesn't know what "winning" means - actions decide

**Checkpoint:** How would you add a new state (like "mana") without changing the engine?

### 6.2 ID Generation
- **Read:** Addendum 3, "Addition: ID Generation" (lines 2783-2892)
- **Key concept:** Spawn emits `spawn_request`, tick assigns IDs deterministically using counter
- **Important:** This ensures deterministic, reversible execution

**Checkpoint:** Why is deterministic ID generation important?

### 6.3 Reverse Hints
- **Read:** Decision 2 examples and despawn (lines 641-665)
- **Key concept:** Non-invertible operations (despawn, spawn) store hints for backward execution
- **Why:** Can't compute backwards from "object doesn't exist"

**Checkpoint:** Why does `despawn` need a reverse hint but `move_to` doesn't?

---

## Level 7: Collision Detection

**Goal:** Understand how collisions work.

### 7.1 Collision Architecture
- **Read:** Decision 4: Collision Detection Architecture (lines 244-356)
- **Key concept:** Collisions detected AFTER all objects tick, stored on GameObject for one frame
- **Why:** Separates concerns - actions don't know about physics

**Checkpoint:** Why are collisions stored on GameObject instead of in a separate structure?

### 7.2 Collision Effects
- **Read:** Decision 4, "Handle projectile+enemy collisions" (lines 302-352)
- **Key concept:** Collision detection finds pairs, then applies effects (despawn)
- **Simple case:** Same position = collision, both despawn

**Exercise:** What happens if 3 objects collide at the same position?

---

## Level 8: Advanced Topics

**Goal:** Understand edge cases and advanced patterns.

### 8.1 Loop in Parallel
- **Read:** Addendum 2, "Loop in Parallel" (lines 2168-2216)
- **Key concept:** Loops can be parallel children, creating infinite parallel actions
- **Important:** This is correct behavior, not a bug

**Checkpoint:** How do you stop a loop that's in parallel?

### 8.2 Nested Parallel
- **Read:** Addendum 2, "Composability: Parallel in Parallel" (lines 2006-2034)
- **Key concept:** Parallel can contain parallel - all children tick every frame
- **Example:** Outer parallel ticks inner parallel, which ticks its children

**Exercise:** Trace `parallel([move_to(10,10,5), parallel([rotate(90,5), animate(5)])])` for 2 frames.

### 8.3 Keyframes
- **Read:** State Structure, keyframes (lines 499-501, 805-806)
- **Key concept:** Snapshots every 10 frames for fast seeking
- **Use case:** Can reconstruct any frame from nearest keyframe

**Checkpoint:** Why are keyframes useful for a game engine?

---

## Level 9: Putting It All Together

**Goal:** Understand the complete flow.

### 9.1 Complete Example Timeline
- **Read:** Example Game Timeline (lines 811-884)
- **Trace through:** Frame by frame, what happens?
- **Focus on:** How actions execute, when objects spawn, when collisions happen
- **Note:** This uses old spawn signature - mentally update to `spawn(Type, Pos, Actions)`

**Exercise:** Add a second enemy that spawns at frame 20. Trace the timeline.

### 9.2 The Complete Tick
- **Read:** Addendum 3's updated tick (lines 2817-2866)
- **Trace through:** Step by step, what happens in one tick?
- **Focus on:** Order matters - objects tick, then collisions, then spawns, then state changes

**Checkpoint:** What happens if an object spawns another object that spawns another object in the same frame?

---

## Level 10: Design Patterns & Philosophy

**Goal:** Understand the "why" behind the design.

### 10.1 Schmupinator Pattern
- **Read:** Core Philosophy (lines 3-17) and "Why This Is Elegant" (lines 1554-1572)
- **Key concept:** Actions are data, engine is dumb executor
- **Benefits:** Inspectable, reversible, composable, testable

**Checkpoint:** How does this pattern enable bidirectional execution?

### 10.2 Pure Predicates
- **Read:** Features Demonstrated, "Pure Predicates" (lines 923-926)
- **Key concept:** No `var/1`, `is/2`, cuts - uses CLP(FD) throughout
- **Why:** Enables constraint solving, bidirectional execution

**Checkpoint:** Why is avoiding `is/2` important?

### 10.3 Homoiconicity
- **Read:** Features Demonstrated, "Homoiconicity" (lines 892-895)
- **Key concept:** Actions ARE data (Prolog terms), can inspect/transform
- **Example:** Can pattern match on actions, append to queues, etc.

**Exercise:** How would you write a predicate that finds all `spawn` actions in an action queue?

---

## Common Pitfalls & Gotchas

### Pitfall 1: Infinite Loops
- **Problem:** `loop([shoot])` never yields
- **Solution:** Always include a yielding action in loops
- **Read:** Addendum 1, "User error example" (lines 1246-1267)

### Pitfall 2: Thinking Parallel Expands
- **Problem:** Thinking parallel just expands to list
- **Solution:** Parallel is composite action that ticks children every frame
- **Read:** Addendum 2, "The Parallel Contradiction" (lines 1696-1733) - but focus on the CORRECT solution, not the wrong one

### Pitfall 3: State Conflicts in Parallel
- **Problem:** Two parallel children modify same attribute
- **Solution:** Last one wins - user controls order
- **Read:** Addendum 2, "State Threading Details" (lines 2037-2095)

### Pitfall 4: Forgetting parallel_running
- **Problem:** Making all parallel actions yield
- **Solution:** Only `parallel_running` yields, not initial `parallel`
- **Read:** Addendum 3, "Bug 1" (lines 2658-2708)

---

## Practice Exercises

### Exercise 1: Simple Tower
Create a tower that:
- Shoots every 10 frames
- Projectiles move 5 frames then despawn
- Use `spawn(projectile, pos(X,Y), [...])` format
- Trace execution for 20 frames

### Exercise 2: Moving Enemy
Create an enemy that:
- Spawns at (0, 0)
- Moves to (10, 0) over 20 frames
- Despawns when reaching goal

### Exercise 3: Parallel Actions
Create an object that:
- Moves and rotates simultaneously using `parallel`
- Both take 10 frames
- Trace how parallel ticks both every frame

### Exercise 4: Collision
Create:
- A projectile moving right
- An enemy moving left
- They collide at frame 5
- Both despawn

### Exercise 5: State Changes
Create:
- Enemy that emits `state_change(score(+10))` when despawned
- Trace how tick aggregates and applies this

### Exercise 6: Despawn in Parallel
Create:
- Object that animates explosion AND despawns in parallel
- Use `parallel([animate_explosion(10), despawn])`
- Trace how despawn bubbles up

---

## Reading Order Summary

**First pass (get the big picture):**
1. Core Philosophy
2. State Structure (Addendum 3 version with next_id)
3. GameObject Structure
4. Example Game Timeline (mentally update spawn signature)

**Second pass (understand execution):**
5. Basic Actions (Addendum 1)
6. execute_until_yield (Addendum 1)
7. Complete Tick Implementation (Addendum 3 version)
8. Loop and Spawn (Addendum 3 version)

**Third pass (advanced features):**
9. Parallel (Addendum 2 - the CORRECT version)
10. Parallel Yielding (Addendum 3)
11. Despawn Bubbling (Addendum 4)
12. State Changes
13. Collision Detection

**Fourth pass (polish):**
14. Design Patterns section
15. Advanced topics (nested parallel, keyframes)

---

## Questions to Ask Yourself

After each level, ask:

1. **What problem does this solve?**
2. **Why was it designed this way?**
3. **What are the alternatives?**
4. **What are the trade-offs?**
5. **How does this fit with the overall philosophy?**

---

## Getting Help

If you're stuck:

1. **Re-read the relevant section** - these concepts build on each other
2. **Trace through examples** - draw out the state changes
3. **Look at the "Why?" sections** - they explain motivations
4. **Start simple** - understand basic actions before compound ones
5. **Check final implementations** - look for Addendum 2/3/4 for corrected versions

---

## Next Steps After Understanding

Once you understand the design:

1. **Implement it** - Start with basic actions, build up
2. **Extend it** - Add new action types, new state
3. **Optimize it** - Improve collision detection, keyframe strategy
4. **Test it** - Write queries to verify behavior
5. **Document it** - Add examples, clarify edge cases

---

**Remember:** This plan teaches the FINAL, CORRECT design. If you see references to "wrong" approaches in the document, skip them - they're only there for historical context. Focus on the implementations marked as correct in the addendums.

