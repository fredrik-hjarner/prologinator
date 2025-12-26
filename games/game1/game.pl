[
    log("game.pl loaded successfully!"),

    load("games/game1/spawn_player.pl"),
    load("games/game1/spawn_enemy.pl"),
    
    spawn_player,
    wait(20),
    spawn_enemy(16, -3),
    wait(20),
    spawn_enemy(8, -3),
    wait(20),
    spawn_enemy(24, -3),
    log("end of game.pl")
].
