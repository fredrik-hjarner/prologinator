% Type Declarations Module
% Defines all regular types for the game engine

:- module(types, []).

% ============================================================================
% Type Declarations (type checking predicates)
% ============================================================================

% :- regtype game_state/1 # "Game state structure".

% game_state(game_state(Frame, Objects, Status, Score, NextID, Keyframe, Commands, RevHints)) :-
%     int(Frame),
%     list(game_object, Objects),
%     game_status(Status),
%     int(Score),
%     int(NextID),
%     keyframe(Keyframe),
%     list(command, Commands),
%     list(rev_hint, RevHints).

% :- regtype game_object/1 # "Game object structure".

% game_object(game_object(ID, Type, attrs(Attrs), Actions, Colls)) :-
%     atm(ID),
%     atm(Type),
%     list(attr, Attrs),
%     list(action, Actions),
%     list(collision, Colls).

% :- regtype game_status/1 # "Game status: playing, won, or lost".

% game_status(playing).
% game_status(won).
% game_status(lost).

% :- regtype keyframe/1 # "Keyframe structure".

% keyframe(keyframe(Frame, Objects)) :-
%     int(Frame),
%     list(game_object, Objects).

% :- regtype rev_hint/1 # "Reverse hints: information completeness for reversal".

% rev_hint(despawned(ID, Attrs)) :-
%     atm(ID),
%     list(attr, Attrs).

% :- regtype command/1 # "Commands: side-effect requests (spawn, state changes)".

% command(spawn_request(Type, Pos, Actions)) :-
%     atm(Type),
%     pos(Pos),
%     list(action, Actions).
% command(state_change(Change)) :-
%     term(Change).

% :- regtype action/1 # "Action types".

% action(wait_frames(N)) :- int(N).
% action(move_to(X, Y, Frames)) :- int(X), int(Y), int(Frames).
% action(despawn).
% action(spawn(Type, Pos, Actions)) :-
%     atm(Type),
%     pos(Pos),
%     list(action, Actions).
% action(loop(Actions)) :- list(action, Actions).
% action(trigger_state_change(Change)) :- term(Change).
% action(parallel(Children)) :- list(action, Children).
% action(parallel_running(Children)) :- list(action, Children).

% :- regtype pos/1 # "Position: pos(X, Y)".

% pos(pos(X, Y)) :-
%     int(X),
%     int(Y).

% :- regtype attr/1 # "Attribute types".

% attr(pos(X, Y)) :- int(X), int(Y).
% attr(A) :- atm(A). % Other attributes as atoms

% :- regtype collision/1 # "Collision types (to be defined)".

% collision(C) :- term(C). % Placeholder - define specific collision types

