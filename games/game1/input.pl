% Input Timeline for Comprehensive Demo
% Shows events vs state - makes player move around

input_timeline([
% move right (arrow 39)
1-[event(key(39), down)],
10-[event(key(39), up)],

% Move left (arrow 37)
11-[event(key(37), down)],
30-[event(key(37), up)],

% move right again
31-[event(key(39), down)],
50-[event(key(39), up)],

% move left again
51-[event(key(37), down)],
70-[event(key(37), up)],

% right again
71-[event(key(39), down)],
90-[event(key(39), up)],

% left again
91-[event(key(37), down)],
110-[event(key(37), up)]
]).

