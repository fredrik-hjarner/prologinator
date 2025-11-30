% Simple ASCII Tower Defense Game
% Usage: just run game (or: ciaosh -l game.pl -e "main" -t halt)

:- module(game, [main/0], []).

:- use_module(library(write), [write/1]).
:- use_module(engine(io_basic), [nl/0]).
:- use_module(library(system), [pause/1]).
:- use_module(library(lists), [member/2]).
:- use_module('./engine', [tick/2]).
:- use_module('./types', [game_state/1, game_object/1]).

% ============================================================================
% Main Game Loop
% ============================================================================
main :-
    % Initial game state: one tower at position (5, 5)
    InitialState = game_state(
        0,
        [game_object(tower_0, tower, attrs([pos(5, 5)]), [], [])],
        playing,
        0,
        1,
        keyframe(0, []),
        []
    ),
    game_loop(InitialState).

game_loop(State) :-
    State = game_state(_, _, Status, _, _, _, _),
    ( Status = playing ->
        render(State),
        tick(State, NewState),
        pause(2),
        game_loop(NewState)
    ;
        render(State),
        write('Game Over!'), nl
    ).

% ============================================================================
% ASCII Rendering
% ============================================================================
render(State) :-
    % Clear screen (ANSI escape code)
    write('\033[2J\033[H'),
    
    State = game_state(Frame, Objects, Status, Score, _, _, _),
    
    % Header
    write('=== Tower Defense ==='), nl,
    write('Frame: '), write(Frame), write(' | Score: '), write(Score), write(' | Status: '), write(Status), nl,
    write('================================'), nl, nl,
    
    % Render grid (10x10 for simplicity)
    render_grid(Objects),
    nl.

render_grid(Objects) :-
    % Grid is 10x10, positions 0-9
    between(0, 9, Y),
    between(0, 9, X),
    ( member(game_object(_, _, attrs(Attrs), _, _), Objects),
      member(pos(X, Y), Attrs) ->
        get_symbol(Objects, X, Y, Symbol)
    ;
        Symbol = '.'
    ),
    write(Symbol),
    ( X = 9 -> nl ; write(' ') ),
    fail.
render_grid(_).

get_symbol(Objects, X, Y, Symbol) :-
    member(game_object(_, Type, attrs(Attrs), _, _), Objects),
    member(pos(X, Y), Attrs),
    ( Type = tower ->
        Symbol = 'T'
    ; Type = enemy ->
        Symbol = 'E'
    ; Type = proj ->
        Symbol = '*'
    ;
        Symbol = '?'
    ),
    !.
get_symbol(_, _, _, '.').

% ============================================================================
% Helper: between/3 (if not available)
% ============================================================================
between(Low, High, Low) :- Low =< High.
between(Low, High, X) :-
    Low < High,
    Low1 is Low + 1,
    between(Low1, High, X).

