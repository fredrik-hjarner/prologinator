% Input Timeline for Comprehensive Demo
% Shows events vs state - makes player move around

input_timeline([
    % Start moving right (arrow 39)
    5-[event(key(39), down)],
    10-[event(key(39), up)],
    
    % Move right again
    15-[event(key(39), down)],
    20-[event(key(39), up)],
    
    % Move up (arrow 38)
    25-[event(key(38), down)],
    30-[event(key(38), up)],
    
    % Move up more
    35-[event(key(38), down)],
    40-[event(key(38), up)],
    
    % Move left (arrow 37) - held for continuous movement
    45-[event(key(37), down)],
    55-[event(key(37), up)],
    
    % Move down (arrow 40)
    60-[event(key(40), down)],
    65-[event(key(40), up)],
    
    % Move down more
    70-[event(key(40), down)],
    75-[event(key(40), up)],
    
    % Move right again
    80-[event(key(39), down)],
    85-[event(key(39), up)],
    
    % Space (32) - shoot
    90-[event(key(32), down)],
    91-[event(key(32), up)],
    
    % Move up
    95-[event(key(38), down)],
    100-[event(key(38), up)],
    
    % Move right - held
    105-[event(key(39), down)],
    120-[event(key(39), up)],
    
    % Move left - held
    125-[event(key(37), down)],
    140-[event(key(37), up)],
    
    % Multiple keys - diagonal movement attempt
    145-[event(key(39), down),  % Right
        event(key(38), down)], % Up
    155-[event(key(39), up),
        event(key(38), up)],
    
    % Move down - held
    160-[event(key(40), down)],
    175-[event(key(40), up)],
    
    % Final movement - right
    180-[event(key(39), down)],
    190-[event(key(39), up)]
]).

