[
    log("spawn_enemy.pl loaded successfully!"),

    define_action(spawn_child(X, Y),
        spawn([
            set_attr(type, enemy),
            set_attr(displayChar, 35),  % '#'
            fork([
                wait_until(parent_id/dead),
                % wait_until(parent_id/hp <= 0),
                % or something similar.
                despawn
            ]),
            fork([
                loop([
                    wait_until(collision_id),
                    decr(parent_id/hp, 1),
                    wait(1)
                ])
            ]),
            loop([
                copy_attr(parent_id/x, x),
                copy_attr(parent_id/y, y),
                move_delta(0, X, Y),
                wait(1)
            ])
        ])
    ),

    define_action(spawn_enemy(X, Y),
        spawn([
            set_attr(type, enemy),
            set_attr(hp, 10),
            set_attr(displayChar, 35),  % '#'
            set_attr(x, X),
            set_attr(y, Y),
 
            %   #
            %  ###
            % #####
            %  ###
            %   #

            % row 1
            spawn_child(0, -2),

            % row 2
            spawn_child(-1, -1),
            spawn_child(0, -1),
            spawn_child(1, -1),

            % row 3
            spawn_child(-2, 0),
            spawn_child(-1, 0),
            % spawn_child(0, 0),
            spawn_child(1, 0),
            spawn_child(2, 0),

            % row 4
            spawn_child(-1, 1),
            spawn_child(0, 1),
            spawn_child(1, 1),

            % row 5
            spawn_child(0, 2),

            % fork([
                % I want to wait til hp is 0 here and
                % or children wait directly for parent_id/hp
                % to become 0 or less.
                % despawn parent + children
                % set_attr(dead, 1)
            % ]),

            repeat(30, [
                wait(6),
                move_delta(0, 0, 1)
            ])

            % despawn % currently causes error in children
        ])
    )
].