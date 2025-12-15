% Input Helper Predicates
% Utilities for checking key events in actions

% ==========================================================
% Key Event Queries (this frame only)
% ==========================================================

% key_down(+KeyCode, +CtxIn, -CtxOut)
% True if KeyCode had a 'down' event this frame
key_down(KeyCode, CtxIn, CtxOut) :-
    ctx_events(Events, CtxIn, CtxIn),
    member(event(key(KeyCode), down), Events),
    CtxOut = CtxIn.

% Verbose alias
key_pressed_this_frame(KeyCode, CtxIn, CtxOut) :-
    key_down(KeyCode, CtxIn, CtxOut).

key_down_this_frame(KeyCode, CtxIn, CtxOut) :-
    key_down(KeyCode, CtxIn, CtxOut).

% key_up(+KeyCode, +CtxIn, -CtxOut)
% True if KeyCode had an 'up' event this frame
key_up(KeyCode, CtxIn, CtxOut) :-
    ctx_events(Events, CtxIn, CtxIn),
    member(event(key(KeyCode), up), Events),
    CtxOut = CtxIn.

% Verbose alias
key_released_this_frame(KeyCode, CtxIn, CtxOut) :-
    key_up(KeyCode, CtxIn, CtxOut).

% TODO: Not sure I like aliases. Also naming could be better
key_up_this_frame(KeyCode, CtxIn, CtxOut) :-
    key_up(KeyCode, CtxIn, CtxOut).

% ==========================================================
% Key State Queries (current state)
% ==========================================================

% key_held(+KeyCode, +CtxIn, -CtxOut)
% True if KeyCode is currently being held down
% (not just pressed this frame, but held since
%  some past frame)
key_held(KeyCode, CtxIn, CtxOut) :-
    ctx_held(Keys, CtxIn, CtxIn),
    member(KeyCode, Keys),
    CtxOut = CtxIn.

% Alias
key_is_held(KeyCode, CtxIn, CtxOut) :-
    key_held(KeyCode, CtxIn, CtxOut).

