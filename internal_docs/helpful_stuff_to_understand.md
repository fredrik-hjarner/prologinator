# Types:

```typescript
type Attr =
    | { type: "pos"; x: number; y: number }
    | { type: "hp"; value: number }
    | { type: "ammo"; value: number }
    | { type: "angle"; value: number }
    | { type: "custom"; name: string; payload: unknown };

type StateChange =
    | { type: "game_over"; status: "won" | "lost" }
    | { type: "score"; delta: number }
    | { type: "custom"; name: string; payload: unknown };

type Action =
    | { type: "wait_frames"; frames: number }
    | { type: "move_to"; x: number; y: number; frames: number }
    | { type: "loop"; actions: Action[] }
    | { type: "parallel"; children: Action[] }
    | { type: "spawn"; kind: string; pos: { x: number; y: number }; actions: Action[] }
    | { type: "despawn" }
    | { type: "trigger_state_change"; change: StateChange }
    | { type: "custom"; name: string; payload: unknown };

interface GameObject {
    id: string;
    attrs: Attr[];
    actions: Action[];
    collisions: string[];
}

type Hint =
    | { type: "spawn_request"; kind: string; pos: { x: number; y: number }; actions: Action[] }
    | { type: "despawned"; id: string; attrs: Attr[] }
    | { type: "state_change"; change: StateChange }
    | { type: "reverse_hint"; payload: unknown }
    | { type: "custom"; name: string; payload: unknown };

interface GameState {
    frame: number;
    objects: GameObject[];
    status: "playing" | "won" | "lost";
    score: number;
    nextId: number;
    lastKeyframe: {
        frame: number;
        objects: GameObject[];
    } | null;
    reverseHints: Hint[];
}
```



# Zoning in on GameState type:

```typescript
interface GameState {
    // Current frame number
    frame: number;

    // All active entities
    objects: Array<{
        // e.g. "tower1", "projectile_42"
        id: string;

        // Dynamic list of attribute terms
        attrs: Array<
            | { type: "pos"; x: number; y: number }
            | { type: "hp"; value: number }
            | { type: "ammo"; value: number }
            | { type: "angle"; value: number }
            | { type: "custom"; name: string; payload: unknown }
        >;

        // Action queue (executes until yield)
        actions: Array<
            | { type: "wait_frames"; frames: number }
            | { type: "move_to"; x: number; y: number; frames: number }
            | { type: "loop"; actions: any[] }               // recursive; see note
            | { type: "parallel"; children: any[] }          // recursive; see note
            | {
                  type: "spawn";
                  kind: string;
                  pos: { x: number; y: number };
                  actions: any[];
              }
            | { type: "despawn" }
            | {
                  type: "trigger_state_change";
                  change:
                      | { type: "game_over"; status: "won" | "lost" }
                      | { type: "score"; delta: number }
                      | { type: "custom"; name: string; payload: unknown };
              }
            | { type: "custom"; name: string; payload: unknown }
        >;

        // IDs this object collided with this frame
        collisions: string[];
    }>;

    // playing | won | lost
    status: "playing" | "won" | "lost";

    // Current score
    score: number;

    // Deterministic counter for spawns
    nextId: number;

    // Optional snapshot for fast seeking
    lastKeyframe:
        | null
        | {
              frame: number;
              objects: Array<{
                  id: string;
                  attrs: Array<
                      | { type: "pos"; x: number; y: number }
                      | { type: "hp"; value: number }
                      | { type: "ammo"; value: number }
                      | { type: "angle"; value: number }
                      | { type: "custom"; name: string; payload: unknown }
                  >;
                  actions: Array<
                      | { type: "wait_frames"; frames: number }
                      | { type: "move_to"; x: number; y: number; frames: number }
                      | { type: "loop"; actions: any[] }
                      | { type: "parallel"; children: any[] }
                      | {
                            type: "spawn";
                            kind: string;
                            pos: { x: number; y: number };
                            actions: any[];
                        }
                      | { type: "despawn" }
                      | {
                            type: "trigger_state_change";
                            change:
                                | { type: "game_over"; status: "won" | "lost" }
                                | { type: "score"; delta: number }
                                | { type: "custom"; name: string; payload: unknown };
                        }
                      | { type: "custom"; name: string; payload: unknown }
                  >;
                  collisions: string[];
              }>;
          };

    // Hints needed for backward execution + side effects
    reverseHints: Array<
        | {
              type: "spawn_request";
              kind: string;
              pos: { x: number; y: number };
              actions: any[];
          }
        | {
              type: "despawned";
              id: string;
              attrs: Array<
                  | { type: "pos"; x: number; y: number }
                  | { type: "hp"; value: number }
                  | { type: "ammo"; value: number }
                  | { type: "angle"; value: number }
                  | { type: "custom"; name: string; payload: unknown }
              >;
          }
        | {
              type: "state_change";
              change:
                  | { type: "game_over"; status: "won" | "lost" }
                  | { type: "score"; delta: number }
                  | { type: "custom"; name: string; payload: unknown };
          }
        | { type: "reverse_hint"; payload: unknown }
        | { type: "custom"; name: string; payload: unknown }
    >;
}
```



# Zoning in on GameObject:

```prolog
% game_object(ID, Attrs, Actions, Collisions)
game_object(
    id(enemy1),
    attrs([
       pos(5,4),
       hp(100)
    ]),
    actions([
       move_to(7,4,20),
       trigger_state_change(game_over(lost))
    ]),
    % per frame collisions (so transient?)
    collisions([enemy2, enemy3])
).
```



# Zoning in on execute_action:

```prolog
% execute_action(Action, ObjIn, ObjOut, Hints)
execute_action(
    wait_frames(N),
    game_object(ID, Attrs, [_|Rest], Colls),
    game_object(ID, Attrs, NewActions, Colls),
    []
) :-
    ( N #> 1 ->
        N1 #= N - 1,
        NewActions = [wait_frames(N1)|Rest]
    ;
        % N = 1, wait is done
        NewActions = Rest
    ).
```



# Simple result of execute_action on wait_frames:

```prolog
% Same object, different frames:

Obj0 = game_object(t1, [pos(0,0)], [wait_frames(3), shoot], []).
execute_action(wait_frames(3), Obj0, Obj1, []).

Obj1 = game_object(t1, [pos(0,0)], [wait_frames(2), shoot], []).
execute_action(wait_frames(2), Obj1, Obj2, []).

Obj2 = game_object(t1, [pos(0,0)], [wait_frames(1), shoot], []).
execute_action(wait_frames(1), Obj2, Obj3, []).

Obj3 = game_object(t1, [pos(0,0)], [shoot], []).
```



# Zoning in on execute_action on move_to:

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
    ( Frames #> 1 ->
        Frames1 #= Frames - 1,
        NewActions = [move_to(TargetX, TargetY, Frames1)|Rest]
    ;
        NewActions = Rest
    ).
```



# Example of execute_action on move_to:

```prolog
Obj0 = game_object(t1, [pos(0,0)], [move_to(10,0,5)], []).

% Frame 1: X += 2
execute_action(move_to(10,0,5), Obj0,
               game_object(t1, [pos(2,0)], [move_to(10,0,4)], []), []).

% Frame 2: X += 2
execute_action(move_to(10,0,4), ... -> [pos(4,0)], [move_to(10,0,3)], []).

% Frame 3: X += 2
... -> [pos(6,0)], [move_to(10,0,2)], [].

% Frame 4: X += 2
... -> [pos(8,0)], [move_to(10,0,1)], [].

% Frame 5: X += 2, done
... -> [pos(10,0)], [], [].
```

# Yields:

## Some actions are marked that they yield


```prolog
% Actions that YIELD (take time)
yields(wait_frames(N)) :- N #> 0.
yields(move_to(_,_,Frames)) :- Frames #> 0.
```

## execute_until_yield:

```prolog
tick_object(ObjIn, ObjOut, AllHints) :-
    execute_until_yield(ObjIn, ObjOut, AllHints).

execute_until_yield(
    game_object(ID, Attrs, [], Colls),
    game_object(ID, Attrs, [], Colls),
    []
) :- !.

execute_until_yield(ObjIn, ObjOut, AllHints) :-
    ObjIn = game_object(ID, Attrs, [Action|Rest], Colls),
    execute_action(Action, ObjIn, ObjTemp, Hints1),
    (   yields(Action) ->
        ObjOut  = ObjTemp,
        AllHints = Hints1
    ;   execute_until_yield(ObjTemp, ObjOut, Hints2),
        append(Hints1, Hints2, AllHints)
    ).
```

<pre style="background-color: rgb(90, 0, 0)">
<b>💭 Reviewer's Comment</b>
Hm... I am not sure I like this...
So let's say an action should/wants to yield based on a
condition, i.e. sometimes, then does this pattern support
it?
I am not sure that this yielding mechanism is flexible
enough.
</pre>
