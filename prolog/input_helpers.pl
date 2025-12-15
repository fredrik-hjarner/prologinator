% Input Helper Predicates
% Utilities for checking key events in actions

% ==========================================================
% Key Event Queries (this frame only)
% ==========================================================

% key_down(+KeyCode)
% True if KeyCode had a 'down' event this frame
key_down(KeyCode) -->
    ctx_events(Events),
    {member(event(key(KeyCode), down), Events)}.

% Verbose alias
key_pressed_this_frame(KeyCode) -->
    key_down(KeyCode).

key_down_this_frame(KeyCode) -->
    key_down(KeyCode).

% key_up(+KeyCode)
% True if KeyCode had an 'up' event this frame
key_up(KeyCode) -->
    ctx_events(Events),
    {member(event(key(KeyCode), up), Events)}.

% Verbose alias
key_released_this_frame(KeyCode) -->
    key_up(KeyCode).

% TODO: Not sure I like aliases. Also naming could be better
key_up_this_frame(KeyCode) -->
    key_up(KeyCode).

% ==========================================================
% Key State Queries (current state)
% ==========================================================

% key_held(+KeyCode)
% True if KeyCode is currently being held down
% (not just pressed this frame, but held since
%  some past frame)
key_held(KeyCode) -->
    ctx_held(Keys),
    {member(KeyCode, Keys)}.

% Alias
key_is_held(KeyCode) -->
    key_held(KeyCode).

