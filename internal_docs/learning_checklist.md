# Learning Checklist: Tower Defense Design

Track your progress through the teaching plan. Check off items as you complete them.

---

## Prerequisites

- [x] Understand basic Prolog syntax (facts, rules, predicates)
- [x] Understand lists in Prolog (`[Head|Tail]` pattern)
- [x] Understand pattern matching/unification
- [x] Understand basic CLP(FD) concepts (constraints like `#=`, `#>`)

---

## Level 1: Core Concepts (Foundation)

### 1.1 The Big Idea
- [x] Read Core Philosophy section (lines 3-17)
- [x] Understand: Actions are data, engine doesn't know what they mean
- [x] Understand: Makes everything inspectable, reversible, composable
- [x] **Checkpoint:** Can explain why actions being "data" is powerful

### 1.2 State Structure
- [x] Read Addendum 3, "Updated State Structure" (lines 2789-2801)
- [x] Understand: `game_state` contains frame, objects, status, score, next_id, keyframes, hints
- [x] Understand: What each field represents, especially `next_id` for ID generation
- [x] **Checkpoint:** Can list all fields in `game_state` and what they're for

### 1.3 GameObject Structure
- [x] Read GameObject Structure section (lines 541-581)
- [ ] Understand: Each object has ID, attributes (key-value list), actions (queue), collisions
- [ ] Understand: Why attributes are a list, why actions are a queue
- [ ] **Checkpoint:** Can explain why `attrs` is a list instead of a fixed structure

---

## Level 2: Basic Actions (Building Blocks)

### 2.1 The execute_action Pattern
- [x] Read `execute_action` example (e.g., Addendum 1, lines 1422-1448 for `wait_frames`)
- [x] Understand: `execute_action(Action, ObjectIn, ObjectOut, Hints)`
- [x] Understand: Takes action + input object → produces output object + hints
- [x] Understand: This is Prolog's way of "modifying" immutable data
- [x] **Checkpoint:** Can explain why `execute_action` takes TWO `game_object` terms

### 2.2 Simple Actions
- [x] Read Addendum 1, "wait_frames" and "move_to" (lines 1387-1455)
- [x] Understand: `wait_frames(N)` and `move_to(X, Y, Frames)`
- [x] Understand: Actions can yield (take time) or be instant
- [x] **Exercise:** Traced through `wait_frames(3)` for 3 frames

### 2.3 Yielding vs Instant
- [x] Read Addendum 1, "Which Actions Yield?" (lines 1064-1079)
- [x] Understand: Some actions need time (`wait_frames`, `move_to`), others are instant (`shoot`, `spawn`)
- [x] Understand: The `yields/1` predicate determines this
- [x] **Checkpoint:** Can list 3 yielding actions and 3 instant actions

---

## Level 3: Execution Model (How Things Run)

### 3.1 The Continuous Execution Model
- [ ] Read Addendum 1, "The Continuous Execution Model" (lines 1024-1105)
- [ ] Understand: Actions execute continuously until one YIELDS
- [ ] Understand: This is the CORRECT model - not "one action per frame"
- [ ] Traced example: `actions([shoot, shoot, shoot, wait_frames(5)])`
- [ ] **Checkpoint:** Can explain why `execute_until_yield` exists

### 3.2 The Tick Function
- [ ] Read Addendum 3, "Tick Processes Spawn Requests" (lines 2817-2866)
- [ ] Understand: Tick orchestrates everything
- [ ] Understand: 1. Tick all objects, 2. Detect collisions, 3. Process spawn requests, 4. Process state changes, 5. Update state
- [ ] **Checkpoint:** Can explain what happens if two objects both emit `state_change(game_over(lost))`

---

## Level 4: Compound Actions (Composition)

### 4.1 Loop
- [ ] Read Decision 5: Loop via Self-Appending (lines 360-392)
- [ ] Understand: `loop(Actions)` re-appends itself after executing Actions
- [ ] Understand: It's like a recursive function that never stops
- [ ] **Exercise:** Traced `loop([shoot, wait_frames(5)])` for 10 frames
- [ ] Understand: Always include a yielding action in loops, or they'll infinite loop!

### 4.2 Spawn
- [ ] Read Addendum 3, "Spawn Action Emits Request" (lines 2803-2812)
- [ ] Read Addendum 3, "Tick Processes Spawn Requests" (lines 2815-2886)
- [ ] Understand: Spawn emits `spawn_request(Type, Pos, Actions)`, tick assigns IDs deterministically
- [ ] Understand: This is the FINAL version - spawn takes Type, not ID
- [ ] **Checkpoint:** Can explain why spawn can't be handled in `execute_action` alone and why tick assigns IDs

---

## Level 5: Parallel Execution (The Real Thing)

### 5.1 Parallel as Composite Action
- [ ] Read Addendum 2, "The Solution: Parallel as Composite Action" (lines 1740-1801)
- [ ] Understand: Parallel stays in queue, ticks ALL children every frame
- [ ] Understand: Like multiple coroutines running simultaneously
- [ ] **Checkpoint:** Can explain how parallel is different from just expanding to a list

### 5.2 Parallel Implementation Details
- [ ] Read Addendum 2, "Implementation" sections (lines 1756-1907)
- [ ] Understand: `tick_parallel_children` and `tick_one_child`
- [ ] Understand: State threads through children sequentially, but all happen same frame
- [ ] **Exercise:** Traced `parallel([move_to(10,10,5), rotate(90,5)])` for 3 frames

### 5.3 Parallel Yielding
- [ ] Read Addendum 3, "Bug 1: Instant Parallel Yields Unnecessarily" (lines 2658-2708)
- [ ] Understand: `parallel` (starting) doesn't yield, `parallel_running` (continuing) does yield
- [ ] Understand: Instant parallels should execute same frame without wasting a frame
- [ ] **Checkpoint:** Can explain why `parallel_running` exists

### 5.4 Despawn in Parallel
- [ ] Read Addendum 4, "Solution: Bubble Despawn Up" (lines 2958-3027)
- [ ] Understand: Despawn bubbles up through parallel layers as `caused_despawn`
- [ ] Understand: This is the FINAL version - despawn CAN be used in parallel
- [ ] **Checkpoint:** Can explain how despawn bubbles up through nested parallel actions

---

## Level 6: State Management (Side Effects)

### 6.1 State Changes as Hints
- [ ] Read Decision 2: State Changes as Emitted Hints (lines 86-203)
- [ ] Understand: Actions emit `state_change(...)`, tick aggregates and applies
- [ ] Understand: Engine doesn't know what "winning" means - actions decide
- [ ] **Checkpoint:** Can explain how to add a new state (like "mana") without changing the engine

### 6.2 ID Generation
- [ ] Read Addendum 3, "Addition: ID Generation" (lines 2783-2892)
- [ ] Understand: Spawn emits `spawn_request`, tick assigns IDs deterministically using counter
- [ ] Understand: This ensures deterministic, reversible execution
- [ ] **Checkpoint:** Can explain why deterministic ID generation is important

### 6.3 Reverse Hints
- [ ] Read Decision 2 examples and despawn (lines 641-665)
- [ ] Understand: Non-invertible operations (despawn, spawn) store hints for backward execution
- [ ] Understand: Can't compute backwards from "object doesn't exist"
- [ ] **Checkpoint:** Can explain why `despawn` needs a reverse hint but `move_to` doesn't

---

## Level 7: Collision Detection

### 7.1 Collision Architecture
- [ ] Read Decision 4: Collision Detection Architecture (lines 244-356)
- [ ] Understand: Collisions detected AFTER all objects tick, stored on GameObject for one frame
- [ ] Understand: Separates concerns - actions don't know about physics
- [ ] **Checkpoint:** Can explain why collisions are stored on GameObject instead of in a separate structure

### 7.2 Collision Effects
- [ ] Read Decision 4, "Handle projectile+enemy collisions" (lines 302-352)
- [ ] Understand: Collision detection finds pairs, then applies effects (despawn)
- [ ] Understand: Simple case: Same position = collision, both despawn
- [ ] **Exercise:** Thought through what happens if 3 objects collide at the same position

---

## Level 8: Advanced Topics

### 8.1 Loop in Parallel
- [ ] Read Addendum 2, "Loop in Parallel" (lines 2168-2216)
- [ ] Understand: Loops can be parallel children, creating infinite parallel actions
- [ ] Understand: This is correct behavior, not a bug
- [ ] **Checkpoint:** Can explain how to stop a loop that's in parallel

### 8.2 Nested Parallel
- [ ] Read Addendum 2, "Composability: Parallel in Parallel" (lines 2006-2034)
- [ ] Understand: Parallel can contain parallel - all children tick every frame
- [ ] Understand: Outer parallel ticks inner parallel, which ticks its children
- [ ] **Exercise:** Traced `parallel([move_to(10,10,5), parallel([rotate(90,5), animate(5)])])` for 2 frames

### 8.3 Keyframes
- [ ] Read State Structure, keyframes (lines 499-501, 805-806)
- [ ] Understand: Snapshots every 10 frames for fast seeking
- [ ] Understand: Can reconstruct any frame from nearest keyframe
- [ ] **Checkpoint:** Can explain why keyframes are useful for a game engine

---

## Level 9: Putting It All Together

### 9.1 Complete Example Timeline
- [ ] Read Example Game Timeline (lines 811-884)
- [ ] Traced through frame by frame
- [ ] Understood: How actions execute, when objects spawn, when collisions happen
- [ ] Noted: This uses old spawn signature - mentally updated to `spawn(Type, Pos, Actions)`
- [ ] **Exercise:** Added a second enemy that spawns at frame 20 and traced the timeline

### 9.2 The Complete Tick
- [ ] Read Addendum 3's updated tick (lines 2817-2866)
- [ ] Traced through step by step what happens in one tick
- [ ] Understood: Order matters - objects tick, then collisions, then spawns, then state changes
- [ ] **Checkpoint:** Can explain what happens if an object spawns another object that spawns another object in the same frame

---

## Level 10: Design Patterns & Philosophy

### 10.1 Schmupinator Pattern
- [ ] Read Core Philosophy (lines 3-17)
- [ ] Read "Why This Is Elegant" (lines 1554-1572)
- [ ] Understand: Actions are data, engine is dumb executor
- [ ] Understand: Benefits: Inspectable, reversible, composable, testable
- [ ] **Checkpoint:** Can explain how this pattern enables bidirectional execution

### 10.2 Pure Predicates
- [ ] Read Features Demonstrated, "Pure Predicates" (lines 923-926)
- [ ] Understand: No `var/1`, `is/2`, cuts - uses CLP(FD) throughout
- [ ] Understand: Enables constraint solving, bidirectional execution
- [ ] **Checkpoint:** Can explain why avoiding `is/2` is important

### 10.3 Homoiconicity
- [ ] Read Features Demonstrated, "Homoiconicity" (lines 892-895)
- [ ] Understand: Actions ARE data (Prolog terms), can inspect/transform
- [ ] Understand: Can pattern match on actions, append to queues, etc.
- [ ] **Exercise:** Can write a predicate that finds all `spawn` actions in an action queue

---

## Common Pitfalls & Gotchas

- [ ] Read Pitfall 1: Infinite Loops (Addendum 1, lines 1246-1267)
- [ ] Read Pitfall 2: Thinking Parallel Expands (Addendum 2, lines 1696-1733)
- [ ] Read Pitfall 3: State Conflicts in Parallel (Addendum 2, lines 2037-2095)
- [ ] Read Pitfall 4: Forgetting parallel_running (Addendum 3, lines 2658-2708)

---

## Practice Exercises

- [ ] **Exercise 1:** Created simple tower that shoots every 10 frames
- [ ] **Exercise 2:** Created moving enemy that spawns, moves, and despawns
- [ ] **Exercise 3:** Created object with parallel move and rotate
- [ ] **Exercise 4:** Created projectile and enemy that collide and despawn
- [ ] **Exercise 5:** Created enemy that emits state_change(score(+10)) when despawned
- [ ] **Exercise 6:** Created object with parallel animate_explosion and despawn

---

## Reading Order Summary

### First pass (get the big picture)
- [ ] Core Philosophy
- [ ] State Structure (Addendum 3 version with next_id)
- [ ] GameObject Structure
- [ ] Example Game Timeline (mentally updated spawn signature)

### Second pass (understand execution)
- [ ] Basic Actions (Addendum 1)
- [ ] execute_until_yield (Addendum 1)
- [ ] Complete Tick Implementation (Addendum 3 version)
- [ ] Loop and Spawn (Addendum 3 version)

### Third pass (advanced features)
- [ ] Parallel (Addendum 2 - the CORRECT version)
- [ ] Parallel Yielding (Addendum 3)
- [ ] Despawn Bubbling (Addendum 4)
- [ ] State Changes
- [ ] Collision Detection

### Fourth pass (polish)
- [ ] Design Patterns section
- [ ] Advanced topics (nested parallel, keyframes)

---

## Questions to Ask Yourself

After each level, I've asked:
- [ ] What problem does this solve?
- [ ] Why was it designed this way?
- [ ] What are the alternatives?
- [ ] What are the trade-offs?
- [ ] How does this fit with the overall philosophy?

---

## Next Steps After Understanding

- [ ] Ready to implement it - Start with basic actions, build up
- [ ] Ready to extend it - Add new action types, new state
- [ ] Ready to optimize it - Improve collision detection, keyframe strategy
- [ ] Ready to test it - Write queries to verify behavior
- [ ] Ready to document it - Add examples, clarify edge cases

---

**Progress:** ___ / ___ items completed

