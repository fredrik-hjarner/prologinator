% Test file for macros.pl - isolated test bubble
:- use_module('./macros').

% Define some test constants
define(max_players, 4).
define(default_health, 100).
define(spawn_point, pos(0, 0, 0)).

% Test using the macros
% Note: Macros are expanded at compile time, so these will
% work when the file is loaded
test_player_init(ID, Player) :-
    MaxPlayers = def(max_players),
    ID =< MaxPlayers,
    DefaultHealth = def(default_health),
    SpawnPoint = def(spawn_point),
    Player = player(ID, DefaultHealth, SpawnPoint).

test_health_check(H) :-
    DefaultHealth = def(default_health),
    H < DefaultHealth.

% Note: Testing macros in the same file where they're
% defined can be tricky because expansion happens during
% loading. These examples just show syntax.

