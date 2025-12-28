[
    
    log("Game loaded successfully!"),
    
    spawn([
        set_attr(type, player),
        set_attr(x, 10),
        set_attr(y, 10),
        set_attr(displayChar, 64),  % '@'
        log("Player spawned!"),
        parallel_all([
            loop([
                wait_key_held(39),
                move_delta(1, 1, 0)
            ]),
            loop([
                wait_key_held(37),
                move_delta(1, -1, 0)
            ]),
            loop([
                wait_key_held(38),
                move_delta(1, 0, -1)
            ]),
            loop([
                wait_key_held(40),
                move_delta(1, 0, 1)
            ])
        ]),
        log("parallel_all completed!")
    ]),
    
    define_action(spawn_tower(X, Y), list([
        spawn([
            set_attr(type, tower),
            set_attr(x, X),
            set_attr(y, Y),
            set_attr(displayChar, 84),  % 'T'
            loop([
                wait(5),
                spawn([
                    set_attr(type, proj),
                    set_attr(x, X),
                    set_attr(y, Y),
                    set_attr(displayChar, 42),  % '*'
                    move_delta(20, 0, -1)
                ])
            ])
        ])
    ])),

    log("define_action"),
    
    spawn_tower(5, 19), log("spawn_tower(5, 19)"),
    spawn_tower(10, 19), log("spawn_tower(10, 19)"),
    spawn_tower(15, 19), log("spawn_tower(15, 19)"),
    
    spawn([
        set_attr(type, static),
        set_attr(x, 0),
        set_attr(y, 0),
        loop([
            wait(8),
            spawn([
                set_attr(type, enemy),
                set_attr(x, 0),
                set_attr(y, 10),
                set_attr(displayChar, 69),  % 'E'
                move_delta(30, 1, 0)
            ])
        ])
    ])
].
