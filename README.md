WIP. Alpha. Still in conceptual/code design phase.

**Prolog 2D Game Engine**.

Like it's predecessor [Schmupinator](https://github.com/fredrik-hjarner/schmupinator) Prologinator has a Domain Specific Language
(DSL) that allows you to makes games. The concept is
simple: all that exist are game objects and every such game
object executes a list of actions, that's it. To bootstrap
the game one empty root game object is automatically spawned
by the engine, the game file is just a list of actions that
that root game object will execute. This for example is a
game file where the only thing that happens is that the root
game object moves to [10, 10] over 100 frames then despawns:

```prolog
[
    move_to(10, 10, 100),
    despawn
].
```

I'm using all kinds of stuff for development:

- bun (for scripts)
  - curl -fsSL https://bun.com/install | bash
- gpp (for preprocessing)
  - sudo apt install gpp
- just (task runner)
  - sudo apt install just
- trealla prolog
- trealla-js (to run prolog in browser)
- scryer prolog
- swi prolog