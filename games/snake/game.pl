% games/snake/snake.pl
% Simple Snake Game for Prologinator
% Head spawns body segments with TTL
% Body despawns when TTL reaches 0
% Based on game1 spawn_player.pl pattern
% Respects 60-character line limit

[
    log("=== SNAKE GAME LOADED ==="),

    % ==========================================
    % BODY SEGMENT
    % ==========================================
    % Spawned by head each frame with TTL
    % Despawns when TTL reaches 0

    define_action(body_behavior,
        fork([
            loop([
                decr(:ttl, 1),
                attr_if(:ttl < 1,
                    [despawn]
                ),
                wait
            ])
        ])
    ),

    define_action(spawn_body_segment,
        spawn([
            set_attr(:type, body),
            set_attr(:displayChar, 111),
            copy_attr(
                :parent_id:x,
                :x
            ),
            copy_attr(
                :parent_id:y,
                :y
            ),
            set_attr(:ttl, 5),
            body_behavior
        ])
    ),

    % ==========================================
    % SNAKE HEAD
    % ==========================================

    define_action(spawn_head,
        spawn([
            log("Spawning snake head"),
            set_attr(:type, head),
            set_attr(:displayChar, 64),  % '@'
            set_attr(:x, 10),
            set_attr(:y, 10),
            set_attr(:dx, 1),   % Start moving right
            set_attr(:dy, 0),
            
            % Main movement loop (always runs)
            fork([
                loop([
                    incr(:x, :dx),
                    incr(:y, :dy),
                    % Wrap around
                    attr_if(:x < 0,
                        [set_attr(:x, 63)]
                    ),
                    attr_if(:x > 63,
                        [set_attr(:x, 0)]
                    ),
                    attr_if(:y < 0,
                        [set_attr(:y, 31)]
                    ),
                    attr_if(:y > 31,
                        [set_attr(:y, 0)]
                    ),
                    spawn_body_segment,
                    wait
                ])
            ]),
            
            % Keyboard direction control
            parallel_all([
                % Up arrow (38)
                loop([
                    wait_key_held(38),
                    set_attr(:dx, 0),
                    set_attr(:dy, -1),
                    wait
                ]),
                % Down arrow (40)
                loop([
                    wait_key_held(40),
                    set_attr(:dx, 0),
                    set_attr(:dy, 1),
                    wait
                ]),
                % Left arrow (37)
                loop([
                    wait_key_held(37),
                    set_attr(:dx, -1),
                    set_attr(:dy, 0),
                    wait
                ]),
                % Right arrow (39)
                loop([
                    wait_key_held(39),
                    set_attr(:dx, 1),
                    set_attr(:dy, 0),
                    wait
                ])
            ]),
            
            log("Controls initialized")
        ])
    ),

    % ==========================================
    % GAME INITIALIZATION
    % ==========================================

    spawn_head,
    log("=== SNAKE GAME RUNNING ===")
].