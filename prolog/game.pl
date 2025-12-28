
main :-
    catch(
        (
            ( catch(getenv("GAME", RawGame), _, fail) ->
                atom_chars(GameName, RawGame) 
            ;
                throw('GAME environment variable missing')
            ),

            atom_concat('games/', GameName, GameDir),
            atom_concat(GameDir, '/game.pl', GameFileAtom),
            atom_concat(GameDir, '/input.pl',InputFileAtom),

            atom_chars(GameFileAtom, GameFile),

            (   catch(consult(InputFileAtom), _, fail),
                catch(
                    input_timeline(TimelineList),
                    _,
                    fail
                )
            ->
                list_to_assoc(TimelineList, Timeline)
            ;
                throw('input_timeline not found in file')
            ),

            empty_assoc(AttrStore0),
            put_assoc(
                0,
                AttrStore0,
                [ attr(type, static),
                  attr(x, 0),
                  attr(y, 0)
                ],
                AttrStore1
            ),

            ctx_with_objs_input(
                [object(id(0))],
                [],
                [],
                InitialContext0
            ),
            ctx_set_attrs(
                AttrStore1, InitialContext0, InitialContext1
            ),

            ctx_actionstore(
                ActionStore0,
                InitialContext1,
                InitialContext1
            ),
            put_assoc(
                0,
                ActionStore0,
                [[load(GameFile)]],
                ActionStore1
            ),
        
            ctx_set_actionstore(
                ActionStore1,
                InitialContext1,
                InitialContext
            ),

            game_loop(
                ctx_in(InitialContext),
                [],
                input_timeline(Timeline),
                keys_held([])
            )
        ),
        Error,
        (
            write('Fatal error in main: '),
            write(Error), nl,
            halt(1)
        )
    ).



game_loop(
    ctx_in(Ctx),
    History,
    input_timeline(Timeline),
    keys_held(KeysHeld)
) :-
    catch(
        (
            ctx_status(Status, Ctx, Ctx),
            ( Status = playing ->
                render(Ctx, Ctx),
                write(
                    'Press: f=forward, r=reverse, q=quit'
                ), nl,
                flush_output,
                get_single_char(Char),
                ( Char = end_of_file ->
                    ctx_set_status(lost, Ctx, NewCtx),
                    NewHistory = History,
                    NewKeysHeld = KeysHeld
                ;
                    handle_input(
                        Char,
                        ctx_in(Ctx),
                        History,
                        input_timeline(Timeline),
                        keys_held(KeysHeld),
                        ctx_out(NewCtx),
                        NewHistory,
                        keys_held(NewKeysHeld)
                    )
                ),

                game_loop(
                    ctx_in(NewCtx),
                    NewHistory,
                    input_timeline(Timeline),
                    keys_held(NewKeysHeld)
                )
            ;
                render(Ctx, Ctx),
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
    Char, 
    ctx_in(Ctx), 
    History,
    input_timeline(Timeline),
    keys_held(KeysHeld),
    ctx_out(NewCtx), 
    NewHistory,
    keys_held(NewKeysHeld)
) :-
    (   char_code(Char, 102) ->  % 'f'
        ctx_frame(Frame, Ctx, Ctx),
        Frame1 #= Frame + 1,
        
        ( get_assoc(Frame1, Timeline, Events) ->
            true
        ;
            Events = []
        ),
        
        apply_events(Events, KeysHeld, NewKeysHeld),
        
        ctx_set_input(
          input(events(Events), held(NewKeysHeld)),
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
        NewHistory = [ctx_in(Ctx)|History]
    ;   char_code(Char, 114) ->  % 'r'
        ( History = [ctx_in(PrevCtx)|RestHistory] ->
            NewCtx = PrevCtx,
            NewHistory = RestHistory,
            ctx_input(
                input(_, held(NewKeysHeld)),
                PrevCtx, PrevCtx
            )
        ;
            NewCtx = Ctx,
            NewHistory = History,
            NewKeysHeld = KeysHeld
        )
    ;   char_code(Char, 113) ->  % 'q'
        ctx_set_status(lost, Ctx, NewCtx),
        NewHistory = History,
        NewKeysHeld = KeysHeld
    ;
        NewCtx = Ctx,
        NewHistory = History,
        NewKeysHeld = KeysHeld
    ).

apply_events([], Keys, Keys).
apply_events(
    [event(key(K), down)|Rest], 
    KeysIn, 
    KeysOut
) :-
    ( member(K, KeysIn) ->
        KeysTemp = KeysIn
    ;
        KeysTemp = [K|KeysIn]
    ),
    apply_events(Rest, KeysTemp, KeysOut).
apply_events(
    [event(key(K), up)|Rest], 
    KeysIn, 
    KeysOut
) :-
    ( select(K, KeysIn, KeysTemp) ->
        true
    ;
        KeysTemp = KeysIn
    ),
    apply_events(Rest, KeysTemp, KeysOut).

render(Ctx, Ctx) :-
    

    ctx_frame(Frame, Ctx, Ctx),
    ctx_status(Status, Ctx, Ctx),
    ctx_objs(Objects, Ctx, Ctx),
    length(Objects, ObjCount),
    
    write('=== Tower Defense ==='), nl,
    write('Frame: '), write(Frame),
    write(' | Status: '), write(Status),
    write(' | Objects: '), write(ObjCount), nl,
    write('================================'), nl, nl,
    
    render_grid(Objects, Ctx, Ctx),
    nl.

render_grid(Objects, CtxIn, CtxOut) :-
    build_position_map(Objects, PosMap, CtxIn, CtxOut),
    render_grid_rows(PosMap, 0).

build_position_map(Objects, PosMap, CtxIn, CtxOut) :-
    empty_assoc(EmptyMap),
    build_position_map_loop(
        Objects, EmptyMap, PosMap, CtxIn, CtxOut
    ).

build_position_map_loop([], Map, Map, CtxIn, CtxOut) :-
    CtxOut = CtxIn.
build_position_map_loop(
    [Obj|Rest], MapIn, MapOut, CtxIn, CtxOut
) :-
    obj_id(Obj, ID),
    ( ctx_attr_val(ID/x, X, CtxIn, CtxIn),
      ctx_attr_val(ID/y, Y, CtxIn, CtxIn),
      get_symbol(ID, Symbol, CtxIn, CtxMid) ->
        Pos = pos(X, Y),
        put_assoc(Pos, MapIn, Symbol, MapTemp)
    ;
        MapTemp = MapIn,
        CtxMid = CtxIn
    ),
    build_position_map_loop(
        Rest, MapTemp, MapOut, CtxMid, CtxOut
    ).

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
    ( get_assoc(Pos, PosMap, CharCode) ->
        put_code(CharCode)
    ;
        put_code(46)  % '.'
    ),
    ( X = 19 -> nl ; put_code(32) ),  % space
    X1 is X + 1,
    render_grid_row(PosMap, Y, X1).
render_grid_row(_, _, X) :-
    X > 19.

get_symbol(ID, Symbol, CtxIn, CtxOut) :-
    ctx_attr_val(ID/displayChar, Symbol, CtxIn, CtxIn),
    integer(Symbol),
    CtxOut = CtxIn.

