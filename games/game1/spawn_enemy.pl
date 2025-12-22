[
    log("spawn_enemy.pl loaded successfully!"),

    define_action(spawn_child(X, Y),
        spawn([
            set_attr(.type, enemy),
            set_attr(.displayChar, 35),  % '#'
            fork([
                wait_until(
                    % default(.parent_id.hp, -1) < 1
                    .parent_id.hp < 1
                ),
                despawn
            ]),
            fork([
                loop([
                    wait_until(exists(.collision_id)),
                    decr(.parent_id.hp, 1),
                    wait(1)
                ])
            ]),
            loop([
                % TODO: copy_attr cause:
                % execute_action: copy_attr
                % % Warning: initialization failed for: main,halt
                % ?- 
                copy_attr(.parent_id.x, .x),
                copy_attr(.parent_id.y, .y),
                move_delta(0, X, Y),
                wait(1)
            ])
        ])
    ),
    
    log("spawn_child custom action was defined"),

    define_action(spawn_enemy(X, Y),
        spawn([
            set_attr(.type, enemy),
            set_attr(.hp, 10),
            set_attr(.displayChar, 35),  % '#'
            set_attr(.x, X),
            set_attr(.y, Y),
 
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

            fork([
                wait_until(.hp < 1),
                % TODO: Without this wait(1) the parent will
                % despawn before the children, and all will
                % crash when the children try to access the
                % parent's attributes.
                % Could probably be solved in different ways
                % one would be to instead let the parent
                % do:
                % for_each(child_id <- child_ids, [
                %     despawn(child_id)
                % ])
                % or something like that. another would be
                % to have the code ot crash when querying
                % stuff that doesn't exist.
                wait(1),
                despawn
            ]),

            repeat(30, [
                wait(6),
                move_delta(0, 0, 1)
            ])

            % despawn % currently causes error in children
        ])
    ),

    log("spawn_enemy custom action was defined")
].