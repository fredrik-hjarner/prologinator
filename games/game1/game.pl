[
    log("game.pl loaded successfully!"),

    load("games/game1/spawn_player.pl"),
    load("games/game1/spawn_enemy.pl"),
    
    spawn_player,
    spawn_enemy(10, -3)
].
