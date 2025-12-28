
define(max_players, 4).
define(default_health, 100).
define(spawn_point, pos(0, 0, 0)).

test_player_init(ID, Player) :-
    MaxPlayers = def(max_players),
    ID =< MaxPlayers,
    DefaultHealth = def(default_health),
    SpawnPoint = def(spawn_point),
    Player = player(ID, DefaultHealth, SpawnPoint).

test_health_check(H) :-
    DefaultHealth = def(default_health),
    H < DefaultHealth.


