% Input Helper Predicates
% Utilities for checking key events in actions

:- module(input_helpers, [
    % Short forms (recommended):
    key_down/2,
    key_up/2,
    key_held/2,
    % Verbose aliases:
    key_pressed_this_frame/2,
    key_released_this_frame/2,
    key_down_this_frame/2,
    key_up_this_frame/2,
    key_is_held/2
]).

:- use_module(library(lists), [member/2]).
:- use_module('./types/accessors', [
    ctx_events/2,
    ctx_held/2
]).

% ==========================================================
% Key Event Queries (this frame only)
% ==========================================================

% key_down(+Ctx, +KeyCode)
% True if KeyCode had a 'down' event this frame
key_down(Ctx, KeyCode) :-
    ctx_events(Ctx, Events),
    member(event(key(KeyCode), down), Events).

% Verbose alias
key_pressed_this_frame(Ctx, KeyCode) :-
    key_down(Ctx, KeyCode).

key_down_this_frame(Ctx, KeyCode) :-
    key_down(Ctx, KeyCode).

% key_up(+Ctx, +KeyCode)
% True if KeyCode had an 'up' event this frame
key_up(Ctx, KeyCode) :-
    ctx_events(Ctx, Events),
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
    ctx_held(Ctx, Keys),
    member(KeyCode, Keys).

% Alias
key_is_held(Ctx, KeyCode) :-
    key_held(Ctx, KeyCode).

