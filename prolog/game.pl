% Simple ASCII Tower Defense Game
% Usage: just run game (or: ciaosh -l game.pl -e "main" -t
% halt)

:- module(game, [main/0]).

:- use_module(library(lists), [member/2]).
:- use_module(library(between), [between/3]).
:- use_module(library(charsio), [get_single_char/1]).
:- use_module('./engine', [tick/2]).
:- use_module('./types/constraints', [
    state_constraint/1,
    object_constraint/1
]).
:- use_module('./types/accessors', [
    ctx_status/2,
    ctx_status_ctx/3,
    obj_attrs/2,
    obj_type_attrs/3
]).

% ==========================================================
% Main Game Loop
% ==========================================================
main :-
    % Initial game state:
    % - Multiple towers at the bottom row that shoot
    %   projectiles periodically
    % - A spawner that creates enemies every 5 frames
    InitialContext = ctx(state(
        frame(0),
        objects([
            % Towers at bottom row (y=19)
            object(
                id(0), type(tower), attrs([pos(5, 19)]),
                actions([
                    loop([
                        wait_frames(3),
                        spawn(proj, pos(5, 19), [
                            move_to(5, 0, 20)
                        ])
                    ])
                ]), collisions([])
            ),
            object(
                id(1), type(tower), attrs([pos(10, 19)]),
                actions([
                    loop([
                        wait_frames(3),
                        spawn(proj, pos(10, 19), [
                            move_to(10, 0, 20)
                        ])
                    ])
                ]), collisions([])
            ),
            object(
                id(2), type(tower), attrs([pos(15, 19)]),
                actions([
                    loop([
                        wait_frames(3),
                        spawn(proj, pos(15, 19), [
                            move_to(15, 0, 20)
                        ])
                    ])
                ]), collisions([])
            ),
            % Enemy spawner
            object(
                id(3), type(static), attrs([]),
                actions([
                    loop([
                        wait_frames(5),
                        spawn(enemy, pos(0, 10), [
                            move_to(19, 10, 30)
                        ])
                    ])
                ]), collisions([])
            )
        ]),
        status(playing),
        score(0),
        next_id(4),
        commands([]),
        rev_hints([])
    )),
    game_loop(ctx_in(InitialContext), []).

game_loop(ctx_in(Ctx), History) :-
    ctx_status(Ctx, Status),
    ( Status = playing ->
        render(ctx_in(Ctx)),
        write('Press: f=forward, r=reverse, q=quit'), nl,
        flush_output,
        get_single_char(Char),
        ( Char = end_of_file ->
            % EOF, quit
            ctx_status_ctx(Ctx, lost, NewCtx),
            NewHistory = History
        ;
            handle_input(
                Char,
                ctx_in(Ctx),
                History,
                ctx_out(NewCtx),
                NewHistory
            )
        ),
        game_loop(ctx_in(NewCtx), NewHistory)
    ;
        render(ctx_in(Ctx)),
        write('Game Over!'), nl
    ).

handle_input(
    Char, ctx_in(Ctx), History, ctx_out(NewCtx), NewHistory
) :-
    (   char_code(Char, 102) ->  % 'f'
        % Forward: tick and add to history
        tick(ctx_in(Ctx), ctx_out(NewCtx)),
        NewHistory = [ctx_in(Ctx)|History]
    ;   char_code(Char, 114) ->  % 'r'
        % Reverse: go back to previous state
        ( History = [ctx_in(PrevCtx)|RestHistory] ->
            NewCtx = PrevCtx,
            NewHistory = RestHistory
        ;
            % No history, stay at current state
            NewCtx = Ctx,
            NewHistory = History
        )
    ;   char_code(Char, 113) ->  % 'q'
        % Quit: set status to lost to exit loop
        ctx_status_ctx(Ctx, lost, NewCtx),
        NewHistory = History
    ;
        % Unknown input, stay at current state
        NewCtx = Ctx,
        NewHistory = History
    ).

% ==========================================================
% ASCII Rendering
% ==========================================================
render(ctx_in(Ctx)) :-
    % Clear screen (ANSI escape code)
    char_code(Esc, 27),  % ESC character
    write(Esc), write('[2J'), write(Esc), write('[H'),
    
    Ctx = ctx(State),
    State = state(
        frame(Frame),
        objects(Objects),
        status(Status),
        score(Score),
        _,
        _,
        _
    ),
    
    % Header
    write('=== Tower Defense ==='), nl,
    write('Frame: '), write(Frame),
    write(' | Score: '), write(Score),
    write(' | Status: '), write(Status), nl,
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
    ( member(Obj, Objects),
      obj_attrs(Obj, Attrs),
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
    member(Obj, Objects),
    obj_type_attrs(Obj, Type, Attrs),
    member(pos(X, Y), Attrs),
    (   Type = tower -> char_code(Symbol, 84)  % 'T'
    ;   Type = enemy -> char_code(Symbol, 69)  % 'E'
    ;   Type = proj -> char_code(Symbol, 42)   % '*'
    ;   char_code(Symbol, 63)                 % '?'
    ).
get_symbol(_, _, _, Dot) :-
    char_code(Dot, 46).  % '.'

