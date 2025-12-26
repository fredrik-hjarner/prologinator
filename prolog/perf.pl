% Simple ASCII Tower Defense Game
% Usage: `just perf`

% Performance stats:
% 2025-12-20 01:01:    300 frames in 33s
% 2025-12-20 05:39:    300 frames in 15.5s
% 2025-12-21 02:17:    300 frames in 14s
% 2025-12-26 03:59:    300 frames in 21.5s (switched to
%                      Trealla which seems a bit slower)

:- use_module('../build/prologinator.pl').
:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(library(charsio)).

% ==========================================================
% main_perf Game Loop
% ==========================================================
main_perf :-
    catch(
        ( % Game file (hardcoded)
          atom_chars('games/game1/game.pl', GameFile),
          
          % Load input timeline (hardcoded)
          ( % Consult loads into user module
            consult('games/game1/input.pl'),
            % Call from user module
            ( catch(user:input_timeline(TimelineList), _, 
                    fail) ->
                list_to_assoc(TimelineList, Timeline)
            ;
                throw('input_timeline not found in file')
            )
          ;
            empty_assoc(Timeline)
          ),
          
          % Create root object that loads the game
          empty_assoc(AttrStore0),
          put_assoc(0, AttrStore0,
                    [attr(type, static),
                     attr(x, 0), attr(y, 0)],
                    AttrStore1),
          ctx_with_objs_input([
              object(id(0))
          ], [], [], InitialContext0),
          ctx_set_attrs(AttrStore1, InitialContext0,
                        InitialContext1),
          % Add actions to actionstore
          ctx_actionstore(ActionStore0, InitialContext1,
                          InitialContext1),
          put_assoc(0, ActionStore0, [[load(GameFile)]],
                    ActionStore1),
          ctx_set_actionstore(ActionStore1, InitialContext1,
                              InitialContext),
          
          % Run 200 frames automatically
          run_frames_perf(InitialContext, Timeline, 0,
                          FinalCtx),
          
          % Render final result
          render_perf(FinalCtx, FinalCtx),
          
          % Halt with success
          halt(0)
        ),
        Error,
        ( write('Fatal error in main_perf: '),
          write(Error), nl,
          halt(1)
        )
    ).

% ==========================================================
% Run Frames Automatically
% ==========================================================
% Run N frames (200 total), then return final context

run_frames_perf(Ctx, Timeline, Frame, FinalCtx) :-
    Frame < 4000,
    !,
    Frame1 is Frame + 1,
    
    % Get events for this frame (if any)
    ( get_assoc(Frame1, Timeline, Events) ->
        true
    ;
        Events = []
    ),
    
    % Set empty keys_held for performance test
    KeysHeld = [],
    
    % Inject input into context
    ctx_set_input(
        input(events(Events), held(KeysHeld)),
        Ctx,
        CtxWithInput
    ),
    
    % Tick
    catch(
        tick(CtxWithInput, NewCtx),
        Error,
        ( write('Error during tick: '),
          write(Error), nl,
          throw(Error)
        )
    ),
    
    % Continue with next frame
    run_frames_perf(NewCtx, Timeline, Frame1, FinalCtx).

run_frames_perf(Ctx, _, Frame, Ctx) :-
    Frame >= 200.

% ==========================================================
% ASCII Rendering
% ==========================================================
render_perf(Ctx, Ctx) :-
    % Clear screen (ANSI escape code)
    % COMMENTED OUT for debugging
    % char_code(Esc, 27),  % ESC character
    % write(Esc), write('[2J'), write(Esc), write('[H'),
    
    ctx_frame(Frame, Ctx, Ctx),
    ctx_status(Status, Ctx, Ctx),
    ctx_objs(Objects, Ctx, Ctx),
    length(Objects, ObjCount),
    
    % Header
    write('=== Tower Defense ==='), nl,
    write('Frame: '), write(Frame),
    write(' | Status: '), write(Status),
    write(' | Objects: '), write(ObjCount), nl,
    write('================================'), nl, nl,
    
    % Render grid (20x20)
    render_grid_perf(Objects, Ctx, Ctx),
    nl.

render_grid_perf(Objects, CtxIn, CtxOut) :-
    % Build position map once: pos(X, Y) -> Symbol
    build_position_map_perf(Objects, PosMap, CtxIn, CtxOut),
    % Grid is 20x20, positions 0-19
    render_grid_rows_perf(PosMap, 0).

build_position_map_perf(Objects, PosMap, CtxIn, CtxOut) :-
    empty_assoc(EmptyMap),
    build_position_map_loop_perf(
        Objects, EmptyMap, PosMap, CtxIn, CtxOut
    ).

build_position_map_loop_perf([], Map, Map, CtxIn, CtxOut) :-
    CtxOut = CtxIn.
build_position_map_loop_perf(
    [Obj|Rest], MapIn, MapOut, CtxIn, CtxOut
) :-
    obj_id(Obj, ID),
    ( ctx_attr_val(ID/x, X, CtxIn, CtxIn),
      ctx_attr_val(ID/y, Y, CtxIn, CtxIn),
      get_symbol_perf(ID, Symbol, CtxIn, CtxMid) ->
        Pos = pos(X, Y),
        put_assoc(Pos, MapIn, Symbol, MapTemp)
    ;
        MapTemp = MapIn,
        CtxMid = CtxIn
    ),
    build_position_map_loop_perf(
        Rest, MapTemp, MapOut, CtxMid, CtxOut
    ).

render_grid_rows_perf(PosMap, Y) :-
    Y =< 19,
    render_grid_row_perf(PosMap, Y, 0),
    Y1 is Y + 1,
    render_grid_rows_perf(PosMap, Y1).
render_grid_rows_perf(_, Y) :-
    Y > 19.

render_grid_row_perf(PosMap, Y, X) :-
    X =< 19,
    Pos = pos(X, Y),
    ( get_assoc(Pos, PosMap, CharCode) ->
        put_code(CharCode)
    ;
        put_code(46)  % '.'
    ),
    ( X = 19 -> nl ; put_code(32) ),  % space
    X1 is X + 1,
    render_grid_row_perf(PosMap, Y, X1).
render_grid_row_perf(_, _, X) :-
    X > 19.

get_symbol_perf(ID, Symbol, CtxIn, CtxOut) :-
    % Get displayChar attribute (character code as integer)
    % If attribute doesn't exist, predicate fails
    % (object not displayed)
    ctx_attr_val(ID/displayChar, Symbol, CtxIn, CtxIn),
    integer(Symbol),
    CtxOut = CtxIn.

