% Simple ASCII Tower Defense Game
% Usage: just run game (or: ciaosh -l game.pl -e "main" -t halt)

:- module(game, [main/0], []).

:- use_module(library(write), [write/1]).
:- use_module(engine(io_basic), [nl/0]).
:- use_module(engine(stream_basic), [flush_output/0]).
:- use_module(library(stream_utils), [get_line/1]).
:- use_module(library(lists), [member/2]).
:- use_module('./engine', [tick/2]).
:- use_module('./types', [game_state/1, game_object/1]).

% ============================================================================
% Main Game Loop
% ============================================================================
main :-
    % Initial game state:
    % - Multiple towers at the bottom row that shoot projectiles periodically
    % - A spawner that creates enemies every 5 frames
    InitialState = game_state(
        0,
        [
            % Towers at bottom row (y=19)
            game_object(tower_0, tower, attrs([pos(5, 19)]), [
                loop([
                    wait_frames(3),
                    spawn(proj, pos(5, 19), [
                        move_to(5, 0, 20)
                    ])
                ])
            ], []),
            game_object(tower_1, tower, attrs([pos(10, 19)]), [
                loop([
                    wait_frames(3),
                    spawn(proj, pos(10, 19), [
                        move_to(10, 0, 20)
                    ])
                ])
            ], []),
            game_object(tower_2, tower, attrs([pos(15, 19)]), [
                loop([
                    wait_frames(3),
                    spawn(proj, pos(15, 19), [
                        move_to(15, 0, 20)
                    ])
                ])
            ], []),
            % Enemy spawner
            game_object(spawner_0, static, attrs([]), [
                loop([
                    wait_frames(5),
                    spawn(enemy, pos(0, 10), [
                        move_to(19, 10, 30)
                    ])
                ])
            ], [])
        ],
        playing,
        0,
        4,
        keyframe(0, []),
        []
    ),
    game_loop(InitialState, []).

game_loop(State, History) :-
    State = game_state(_, _, Status, _, _, _, _),
    ( Status = playing ->
        render(State),
        write('Press: f=forward, r=reverse, q=quit'), nl,
        flush_output,
        get_line(Line),
        ( Line = end_of_file ->
            % EOF, quit
            State = game_state(F, Objs, _, Score, NextID, KF, Hints),
            NewState = game_state(F, Objs, lost, Score, NextID, KF, Hints),
            NewHistory = History
        ; Line = [Code|_] ->
            handle_input(Code, State, History, NewState, NewHistory)
        ;
            % Empty line, just continue
            NewState = State,
            NewHistory = History
        ),
        game_loop(NewState, NewHistory)
    ;
        render(State),
        write('Game Over!'), nl
    ).

handle_input(0'f, State, History, NewState, NewHistory) :-
    % Forward: tick and add to history
    tick(State, NewState),
    NewHistory = [State|History].
handle_input(0'r, State, History, NewState, NewHistory) :-
    % Reverse: go back to previous state
    ( History = [PrevState|RestHistory] ->
        NewState = PrevState,
        NewHistory = RestHistory
    ;
        % No history, stay at current state
        NewState = State,
        NewHistory = History
    ).
handle_input(0'q, State, History, NewState, NewHistory) :-
    % Quit: set status to lost to exit loop
    State = game_state(F, Objs, _, Score, NextID, KF, Hints),
    NewState = game_state(F, Objs, lost, Score, NextID, KF, Hints),
    NewHistory = History.
handle_input(_, State, History, State, History).

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
    % Grid is 20x20, positions 0-19
    between(0, 19, Y),
    between(0, 19, X),
    ( member(game_object(_, _, attrs(Attrs), _, _), Objects),
      member(pos(X, Y), Attrs) ->
        get_symbol(Objects, X, Y, Symbol)
    ;
        Symbol = '.'
    ),
    write(Symbol),
    ( X = 19 -> nl ; write(' ') ),
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

