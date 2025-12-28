[
    log("spawn_player.pl loaded successfully!"),

    define_action(despawn_shot_behaviour, 
        fork([
            loop([
                wait(1),
                attr_if(:y < 0,
                    [despawn],
                    []
                ),
                attr_if(:x < 0,
                    [despawn],
                    []
                ),
                attr_if(:x > 63,
                    [despawn],
                    []
                )
            ])
        ])
    ),

    define_action(spawn_player_child(X, Y),
        spawn([
            set_attr(:type, player),
            set_attr(:displayChar, 35),  % '#'
            loop([
                copy_attr(:parent_id:x, :x),
                copy_attr(:parent_id:y, :y),
                move_delta(0, X, Y),
                wait(1)
            ])
        ])
    ),

    define_action(spawn_player_shot(DirX, DirY),
        spawn([
            set_attr(:type, proj),
            copy_attr(:parent_id:x, :x),
            copy_attr(:parent_id:y, :y),
            set_attr(:displayChar, 42),  % '*'
            despawn_shot_behaviour,
            fork([
                loop([
                    wait(1),
                    move_delta(0, DirX, DirY)
                ])
            ])
        ])
    ),

    define_action(spawn_player,
        spawn([
            log("log at start of spawn_player"),
            set_attr(:type, player),
            set_attr(:x, 16),
            set_attr(:y, 31),
            log("Player spawned!"),
            set_attr(:displayChar, 64),  % '@'
            log("displayChar was set to 64"),

            %   #
            %  ###
            %  ###

            spawn_player_child(-1, 1),
            spawn_player_child(0, 1),
            spawn_player_child(1, 1),

            spawn_player_child(-1, 2),
            spawn_player_child(0, 2),
            spawn_player_child(1, 2),

            fork([
                loop([
                    wait(3),
                    spawn_player_shot(-1, -2),
                    spawn_player_shot(0, -2),
                    spawn_player_shot(1, -2)
                ])
            ]),
            parallel_all([
                % Right arrow (39) - move right
                loop([
                    wait_key_held(39),
                    move_delta(0, 1, 0),
                    attr_if(:x > 62,
                        [set_attr(:x, 62)],
                        []
                    ),
                    wait(1)
                ]),
                % Left arrow (37) - move left
                loop([
                    wait_key_held(37),
                    move_delta(0, -1, 0),
                    attr_if(:x < 1,
                        [set_attr(:x, 1)],
                        []
                    ),
                    wait(1)
                ]),
                % Up arrow (38) - move up
                loop([
                    wait_key_held(38),
                    move_delta(0, 0, -1),
                    attr_if(:y < 0,
                        [set_attr(:y, 0)],
                        []
                    ),
                    wait(1)
                ]),
                % Down arrow (40) - move down
                loop([
                    wait_key_held(40),
                    move_delta(0, 0, 1),
                    attr_if(:y > 61,
                        [set_attr(:y, 61)],
                        []
                    ),
                    wait(1)
                ])
            ]),
            log("parallel_all completed!")
        ])
    ),

    log("end of spawn_player.pl")
].