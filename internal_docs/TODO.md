# set_attr and copy_attr consistentcy problems

First of all maybe I just need `set_attr` since maybe
it could be used like set_attr(@hp, @parent_id@hp)?

So yea about the inconsistency... I think it looks a bit
strange that set_attr is used like this currently
`set_attr(x, 1)` because x is an attribute but it does not
use @ syntax... I mean it kinda makes sense but I am not
sure if we can't do something that looks better.

# Allow define_action to take list as input

Actually only a list of actions should be allowed.

# Callback när attr ändras på sig själv eller annan.

Ska man kunna lyssna på när ett attribut 
ändras/updateras? Internt kan det vara ett dirty-fält som
lever en frame eller nåt.

# Move as much as possible into attributes

I.e. have as little as possible as fields on the Game Object

# Maybe an object HAVING actions is incorrect abstraction?

Because that coupling/abstraction breaks apart in some
actions (parallel_all, parallel_race and similar).

# Maybe id and attr_id should be separate?

So that you can for example spawn a new object (i.e.
function generator) that is connected to the same attrs?

# In stdlib I could maybe add some kinda persistent move

like support for dx dy:

`wait_key_down(w), set_attr(dy, -1), set_attr(dx, 0) `

# Make xod:validate/2 into xod:validate/3 with debug msg

Yup. It's very hard to see what's wrong and where.
I prolly need more debug messages etc.

# Rename object to obj

yup shorter.

# Add some setting via env or something to toggle validation

Add some setting via env or something to toggle validation
on an off.

# Wrap state in context

And just like in golang thread context down as FIRST
argument.

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