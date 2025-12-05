# Rename game_state to state and game_object to object

yup shorter that way.

# For forward execution I should have type validators

Yes, if ground then a thing should be validated with nice
error messages.

# Collisions not 100% implemented

The `Colls` field exists on game objects, but it's currently always empty (`[]`). Here's what I found:

## Current State

1. All objects are created with empty collision lists:
   - In `game.pl`: `game_object(0, tower, attrs([pos(5, 19)]), [...], [])`
   - When spawning: `game_object(ObjID, Type, attrs([Pos]), Acts, [])` (line 330 in engine.pl)

2. The collision detection code creates `collision(ID1, ID2)` terms temporarily but doesn't store them on objects:
```374:387:prolog/engine.pl
detect_collisions(Objects, NewObjects, RevHints) :-
    findall(
        collision(ID1, ID2),
        (
            member(game_object(ID1, _, attrs(A1), _, _), Objects),
            member(game_object(ID2, _, attrs(A2), _, _), Objects),
            ID1 @< ID2,
            member(pos(X1, Y1), A1),
            member(pos(X2, Y2), A2),
            collides_at(X1, Y1, X2, Y2)
        ),
        Collisions
    ),
    handle_collisions(Objects, Collisions, NewObjects, RevHints).
```

3. The design intended collisions to be stored on objects (per the design docs), but that isn't implemented yet.

## What the Colls field would store (if implemented)

According to the type constraint predicates, collisions would be stored as:
- `collision(ID1, ID2)` where `ID1` and `ID2` are integers (object IDs)
- So `Colls` would be a list like: `[collision(5, 10), collision(5, 12)]` meaning object 5 collided with objects 10 and 12

Currently, `Colls` is always `[]` because the implementation removes colliding objects instead of storing collision info on them.