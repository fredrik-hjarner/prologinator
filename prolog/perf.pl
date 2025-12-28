

:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(library(charsio)).

main_perf :-
    catch(
        ( % Game file (hardcoded)
          atom_chars('games/game1/game.pl', GameFile),
          
          ( % Consult loads into user module
            consult('games/game1/input.pl'),
            ( catch(user:input_timeline(TimelineList), _, 
                    fail) ->
                list_to_assoc(TimelineList, Timeline)
            ;
                throw('input_timeline not found in file')
            )
          ;
            empty_assoc(Timeline)
          ),
          
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
          ctx_actionstore(ActionStore0, InitialContext1,
                          InitialContext1),
          put_assoc(0, ActionStore0, [[load(GameFile)]],
                    ActionStore1),
          ctx_set_actionstore(ActionStore1, InitialContext1,
                              InitialContext),
          
          statistics(runtime, [Start|_]),
          
          run_frames_perf(InitialContext, Timeline, 0,
                          FinalCtx),
          
          statistics(runtime, [End|_]),
          
          render_perf(FinalCtx, FinalCtx),
          
          DurationMs is End - Start,
          DurationSec is DurationMs / 1000,
          format(
              "~nExecution Time: ~w ms (~w s)~n~n",
              [DurationMs, DurationSec]
          ),

          halt(0)
        ),
        Error,
        ( write('Fatal error in main_perf: '),
          write(Error), nl,
          halt(1)
        )
    ).


run_frames_perf(Ctx, Timeline, Frame, FinalCtx) :-
    Frame < 5,
    !,
    Frame1 is Frame + 1,
    
    ( get_assoc(Frame1, Timeline, Events) ->
        true
    ;
        Events = []
    ),
    
    KeysHeld = [],
    
    ctx_set_input(
        input(events(Events), held(KeysHeld)),
        Ctx,
        CtxWithInput
    ),
    
    catch(
        tick(CtxWithInput, NewCtx),
        Error,
        ( write('Error during tick: '),
          write(Error), nl,
          throw(Error)
        )
    ),
    
    run_frames_perf(NewCtx, Timeline, Frame1, FinalCtx).

run_frames_perf(Ctx, _, Frame, Ctx) :-
    Frame >= 5.

render_perf(Ctx, Ctx) :-
    
    ctx_frame(Frame, Ctx, Ctx),
    ctx_status(Status, Ctx, Ctx),
    ctx_objs(Objects, Ctx, Ctx),
    length(Objects, ObjCount),
    
    write('=== Tower Defense ==='), nl,
    write('Frame: '), write(Frame),
    write(' | Status: '), write(Status),
    write(' | Objects: '), write(ObjCount), nl,
    write('================================'), nl, nl,
    
    render_grid_perf(Objects, Ctx, Ctx),
    nl.

render_grid_perf(Objects, CtxIn, CtxOut) :-
    build_position_map_perf(Objects, PosMap, CtxIn, CtxOut),
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
    ctx_attr_val(ID/displayChar, Symbol, CtxIn, CtxIn),
    integer(Symbol),
    CtxOut = CtxIn.

