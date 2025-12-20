% Input Timeline for Comprehensive Demo
% Shows events vs state - makes player move around

input_timeline([
    % Start moving right (arrow 39)
    1-[event(key(39), down)],
    100-[event(key(39), up)],
    
    % Move left (arrow 37) - held for continuous movement
    101-[event(key(37), down)],
    200-[event(key(37), up)]
]).

