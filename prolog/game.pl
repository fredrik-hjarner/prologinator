% Simple ASCII Tower Defense Game
% Usage: just run game (or: ciaosh -l game.pl -e "main" -t
% halt)

:- module(game, [main/0]).

:- use_module(library(lists), [member/2]).
:- use_module(library(between), [between/3]).
:- use_module(library(charsio), [
    get_single_char/1
]).
:- use_module(library(os), [getenv/2]).
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
    catch(
        ( % Get game file from environment (required)
          ( catch(getenv("GAME", GameFile), _, fail) ->
              % GameFile is already a list of chars
              % (Scryer string)
              true
          ;
              throw('GAME environment variable missing')
          ),
          
          % Create root object that loads the game
          empty_assoc(AttrStore0),
          put_assoc(0, AttrStore0,
                    [attr(x, 0), attr(y, 0)],
                    AttrStore1),
          
          InitialContext = ctx(state(
              frame(0),
              objects([
                  object(
                      id(0),
                      type(static),
                      actions([load(GameFile)]),
                      collisions([])
                  )
              ]),
              attrs(AttrStore1),
              status(playing),
              next_id(1),
              commands([])
          )),
          game_loop(ctx_in(InitialContext), [])
        ),
        Error,
        ( write('Fatal error in main: '),
          write(Error), nl,
          halt(1)
        )
    ).

game_loop(ctx_in(Ctx), History) :-
    catch(
        ( ctx_status(Ctx, Status),
          ( Status = playing ->
            render(ctx_in(Ctx)),
              write('Press: f=forward, r=reverse, q=quit'),
              nl,
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
          )
        ),
        Error,
        ( write('Error in game_loop: '),
          write(Error), nl,
          halt(1)
        )
    ).

handle_input(
    Char, ctx_in(Ctx), History, ctx_out(NewCtx), NewHistory
) :-
    (   char_code(Char, 102) ->  % 'f'
        % Forward: tick and add to history
        catch(
            tick(ctx_in(Ctx), ctx_out(NewCtx)),
            Error,
            ( write('Error during tick: '),
              write(Error), nl,
              throw(Error)
            )
        ),
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
    % COMMENTED OUT for debugging
    % char_code(Esc, 27),  % ESC character
    % write(Esc), write('[2J'), write(Esc), write('[H'),
    
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
    % Build position map once: pos(X, Y) -> Symbol
    build_position_map(Ctx, Objects, PosMap),
    % Grid is 20x20, positions 0-19
    render_grid_rows(PosMap, 0).

build_position_map(Ctx, Objects, PosMap) :-
    empty_assoc(EmptyMap),
    build_position_map_loop(Ctx, Objects, EmptyMap, PosMap).

build_position_map_loop(_, [], Map, Map).
build_position_map_loop(Ctx, [Obj|Rest], MapIn, MapOut) :-
    obj_id(Obj, ID),
    ( ctx_attr_val(Ctx, ID/x, X),
      ctx_attr_val(Ctx, ID/y, Y),
      get_symbol(Obj, Symbol) ->
        Pos = pos(X, Y),
        put_assoc(Pos, MapIn, Symbol, MapTemp)
    ;
        MapTemp = MapIn
    ),
    build_position_map_loop(Ctx, Rest, MapTemp, MapOut).

render_grid_rows(PosMap, Y) :-
    Y =< 19,
    render_grid_row(PosMap, Y, 0),
    Y1 is Y + 1,
    render_grid_rows(PosMap, Y1).
render_grid_rows(_, Y) :-
    Y > 19.

render_grid_row(PosMap, Y, X) :-
    X =< 19,
    Pos = pos(X, Y),
    ( get_assoc(Pos, PosMap, Symbol) ->
        true
    ;
        char_code(Symbol, 46)  % '.'
    ),
    write(Symbol),
    ( X = 19 -> nl ; write(' ') ),
    X1 is X + 1,
    render_grid_row(PosMap, Y, X1).
render_grid_row(_, _, X) :-
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

