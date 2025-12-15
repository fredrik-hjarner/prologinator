% Input Helper Predicates
% Utilities for checking key events in actions

% ==========================================================
% Key Event Queries (this frame only)
% ==========================================================

% key_down(+Ctx, +KeyCode)
% True if KeyCode had a 'down' event this frame
key_down(Ctx, KeyCode) :-
    ctx_events(Events, Ctx),
    member(event(key(KeyCode), down), Events).

% Verbose alias
key_pressed_this_frame(Ctx, KeyCode) :-
    key_down(Ctx, KeyCode).

key_down_this_frame(Ctx, KeyCode) :-
    key_down(Ctx, KeyCode).

% key_up(+Ctx, +KeyCode)
% True if KeyCode had an 'up' event this frame
key_up(Ctx, KeyCode) :-
    ctx_events(Events, Ctx),
    member(event(key(KeyCode), up), Events).

% Verbose alias
key_released_this_frame(Ctx, KeyCode) :-
    key_up(Ctx, KeyCode).

% TODO: Not sure I like aliases. Also naming could be better
key_up_this_frame(Ctx, KeyCode) :-
    key_up(Ctx, KeyCode).

% ==========================================================
% Key State Queries (current state)
% ==========================================================

% key_held(+Ctx, +KeyCode)
% True if KeyCode is currently being held down
% (not just pressed this frame, but held since
%  some past frame)
key_held(Ctx, KeyCode) :-
    ctx_held(Keys, Ctx),
    member(KeyCode, Keys).

% Alias
key_is_held(Ctx, KeyCode) :-
    key_held(Ctx, KeyCode).

