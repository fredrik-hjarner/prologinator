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
:- use_module('./types/accessors').
:- use_module('./types/adv_accessors', [
    ctx_attr_val/3
]).
:- use_module(library(assoc)).

% ==========================================================
% Main Game Loop
% ==========================================================
main :-
    % Initial game state:
    % - Multiple towers at the bottom row that shoot
    %   projectiles periodically
    % - A spawner that creates enemies every 5 frames
    % Initialize attribute store
    empty_assoc(AttrStore0),
    put_assoc(0, AttrStore0,
              [attr(x, 5), attr(y, 19)], AttrStore1),
    put_assoc(1, AttrStore1,
              [attr(x, 10), attr(y, 19)], AttrStore2),
    put_assoc(2, AttrStore2,
              [attr(x, 15), attr(y, 19)], AttrStore3),
    InitialContext = ctx(state(
        frame(0),
        objects([
            % Tower 1: Burst fire (3 shots in quick
            %   succession)
            object(
                id(0), type(tower),
                actions([
                    loop([
                        wait(5),
                        repeat(3, [
                            spawn(proj, 5, 19, [
                                move_delta(20, 0, -1)
                            ]),
                            wait(1)
                        ])
                    ])
                ]), collisions([])
            ),
            % Tower 2: Diagonal shots
            object(
                id(1), type(tower),
                actions([
                    loop([
                        wait(4),
                        spawn(proj, 10, 19, [
                            move_delta(15, 1, -1)
                        ]),
                        wait(2),
                        spawn(proj, 10, 19, [
                            move_delta(15, -1, -1)
                        ])
                    ])
                ]), collisions([])
            ),
            % Tower 3: Rapid fire burst
            object(
                id(2), type(tower),
                actions([
                    loop([
                        wait(6),
                        repeat(5, [
                            spawn(proj, 15, 19, [
                                move_delta(25, 0, -1)
                            ]),
                            wait(1)
                        ])
                    ])
                ]), collisions([])
            ),
            % Enemy spawner: Creates enemies with varied
            %   patterns
            object(
                id(3), type(static),
                actions([
                    loop([
                        wait(8),
                        spawn(enemy, 0, 10, [
                            move_delta(30, 1, 0)
                        ]),
                        wait(3),
                        spawn(enemy, 0, 5, [
                            % Zigzag pattern
                            repeat(3, [
                                move_delta(5, 2, 0),
                                move_delta(5, 2, 1)
                            ])
                        ]),
                        wait(2),
                        spawn(enemy, 0, 15, [
                            move_delta(25, 1, -1)
                        ])
                    ])
                ]), collisions([])
            )
        ]),
        attrs(AttrStore3),
        status(playing),
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
    
    ctx_frame(Ctx, Frame),
    ctx_status(Ctx, Status),
    ctx_objs(Ctx, Objects),
    
    % Header
    write('=== Tower Defense ==='), nl,
    write('Frame: '), write(Frame),
    write(' | Status: '), write(Status), nl,
    write('================================'), nl, nl,
    
    % Render grid (20x20)
    render_grid(Ctx, Objects),
    nl.

render_grid(Ctx, Objects) :-
    % Grid is 20x20, positions 0-19
    render_grid_rows(Ctx, Objects, 0).

render_grid_rows(Ctx, Objects, Y) :-
    Y =< 19,
    render_grid_row(Ctx, Objects, Y, 0),
    Y1 is Y + 1,
    render_grid_rows(Ctx, Objects, Y1).
render_grid_rows(_, _, Y) :-
    Y > 19.

render_grid_row(Ctx, Objects, Y, X) :-
    X =< 19,
    ( member(Obj, Objects),
      obj_id(Obj, ID),
      ctx_attr_val(Ctx, ID/x, X),
      ctx_attr_val(Ctx, ID/y, Y) ->
        get_symbol(Obj, Symbol)
    ;
        char_code(Symbol, 46)  % '.'
    ),
    write(Symbol),
    ( X = 19 -> nl ; write(' ') ),
    X1 is X + 1,
    render_grid_row(Ctx, Objects, Y, X1).
render_grid_row(_, _, _, X) :-
    X > 19.

get_symbol(Obj, Symbol) :-
    obj_type(Obj, Type),
    (   Type = tower -> char_code(Symbol, 84)  % 'T'
    ;   Type = enemy -> char_code(Symbol, 69)  % 'E'
    ;   Type = proj -> char_code(Symbol, 42)   % '*'
    ;   char_code(Symbol, 63)                 % '?'
    ).
get_symbol(_, Dot) :-
    char_code(Dot, 46).  % '.'

