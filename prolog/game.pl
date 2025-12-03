% Simple ASCII Tower Defense Game
% Usage: just run game (or: ciaosh -l game.pl -e "main" -t halt)

:- module(game, [main/0]).

:- use_module(library(lists), [member/2]).
:- use_module(library(between), [between/3]).
:- use_module(library(charsio), [get_single_char/1]).
:- use_module('./engine', [tick/2]).
:- use_module('./types', [game_state_type/1, game_object_type/1]).

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
            game_object(0, tower, attrs([pos(5, 19)]), [
                loop([
                    wait_frames(3),
                    spawn(proj, pos(5, 19), [
                        move_to(5, 0, 20)
                    ])
                ])
            ], []),
            game_object(1, tower, attrs([pos(10, 19)]), [
                loop([
                    wait_frames(3),
                    spawn(proj, pos(10, 19), [
                        move_to(10, 0, 20)
                    ])
                ])
            ], []),
            game_object(2, tower, attrs([pos(15, 19)]), [
                loop([
                    wait_frames(3),
                    spawn(proj, pos(15, 19), [
                        move_to(15, 0, 20)
                    ])
                ])
            ], []),
            % Enemy spawner
            game_object(3, static, attrs([]), [
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
        [],
        []
    ),
    game_loop(InitialState, []).

game_loop(State, History) :-
    State = game_state(_, _, Status, _, _, _, _),
    ( Status = playing ->
        render(State),
        write('Press: f=forward, r=reverse, q=quit'), nl,
        flush_output,
        get_single_char(Char),
        ( Char = end_of_file ->
            % EOF, quit
            State = game_state(F, Objs, _, Score, NextID, Commands, RevHints),
            NewState = game_state(F, Objs, lost, Score, NextID, Commands, RevHints),
            NewHistory = History
        ;
            handle_input(Char, State, History, NewState, NewHistory)
        ),
        game_loop(NewState, NewHistory)
    ;
        render(State),
        write('Game Over!'), nl
    ).

handle_input(Char, State, History, NewState, NewHistory) :-
    (   char_code(Char, 102) ->  % 'f'
        % Forward: tick and add to history
        tick(State, NewState),
        NewHistory = [State|History]
    ;   char_code(Char, 114) ->  % 'r'
        % Reverse: go back to previous state
        ( History = [PrevState|RestHistory] ->
            NewState = PrevState,
            NewHistory = RestHistory
        ;
            % No history, stay at current state
            NewState = State,
            NewHistory = History
        )
    ;   char_code(Char, 113) ->  % 'q'
        % Quit: set status to lost to exit loop
        State = game_state(F, Objs, _, Score, NextID, Commands, RevHints),
        NewState = game_state(F, Objs, lost, Score, NextID, Commands, RevHints),
        NewHistory = History
    ;
        % Unknown input, stay at current state
        NewState = State,
        NewHistory = History
    ).

% ============================================================================
% ASCII Rendering
% ============================================================================
render(State) :-
    % Clear screen (ANSI escape code)
    char_code(Esc, 27),  % ESC character
    write(Esc), write('[2J'), write(Esc), write('[H'),
    
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
    render_grid_rows(Objects, 0).

render_grid_rows(Objects, Y) :-
    Y =< 19,
    render_grid_row(Objects, Y, 0),
    Y1 is Y + 1,
    render_grid_rows(Objects, Y1).
render_grid_rows(_, Y) :-
    Y > 19.

render_grid_row(Objects, Y, X) :-
    X =< 19,
    ( member(game_object(_, _, attrs(Attrs), _, _), Objects),
      member(pos(X, Y), Attrs) ->
        get_symbol(Objects, X, Y, Symbol)
    ;
        char_code(Symbol, 46)  % '.'
    ),
    write(Symbol),
    ( X = 19 -> nl ; write(' ') ),
    X1 is X + 1,
    render_grid_row(Objects, Y, X1).
render_grid_row(_, _, X) :-
    X > 19.

get_symbol(Objects, X, Y, Symbol) :-
    member(game_object(_, Type, attrs(Attrs), _, _), Objects),
    member(pos(X, Y), Attrs),
    (   Type = tower -> char_code(Symbol, 84)  % 'T'
    ;   Type = enemy -> char_code(Symbol, 69)  % 'E'
    ;   Type = proj -> char_code(Symbol, 42)   % '*'
    ;   char_code(Symbol, 63)                 % '?'
    ).
get_symbol(_, _, _, Dot) :-
    char_code(Dot, 46).  % '.'

