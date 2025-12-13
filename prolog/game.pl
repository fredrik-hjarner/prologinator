% Simple ASCII Tower Defense Game
% Usage: just run game (or: ciaosh -l game.pl -e "main" -t
% halt)

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
          
          % Load input timeline (optional)
          ( catch(getenv("INPUTS", InputFile), _, fail) ->
              % Convert InputFile (list of chars) to atom
              atom_chars(InputFileAtom, InputFile),
              % Consult loads into user module
              consult(InputFileAtom),
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
                    [attr(x, 0), attr(y, 0)],
                    AttrStore1),
          
          InitialContext = ctx(
              state(
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
              ),
              input(
                  events([]),  % No events at frame 0
                  held([])     % No keys held initially
              )
          ),
          
          game_loop(
            ctx_in(InitialContext), 
            [],
            input_timeline(Timeline),
            keys_held([])  % Initial key state
          )
        ),
        Error,
        ( write('Fatal error in main: '),
          write(Error), nl,
          halt(1)
        )
    ).

game_loop(
    ctx_in(Ctx), 
    History,
    input_timeline(Timeline),
    keys_held(KeysHeld)  % Current key state
) :-
    catch(
        ( ctx_status(Ctx, Status),
          ( Status = playing ->
              render(ctx_in(Ctx)),
              write('Press: f=forward, r=reverse, \
q=quit'),
              nl,
              flush_output,
              get_single_char(Char),
              ( Char = end_of_file ->
                  % EOF, quit
                  ctx_status_ctx(Ctx, lost, NewCtx),
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
        % Forward: lookup events, update keys, tick
        ctx_frame(Ctx, Frame),
        Frame1 #= Frame + 1,
        
        % Get events for next frame
        ( get_assoc(Frame1, Timeline, Events) ->
            true
        ;
            Events = []
        ),
        
        % Update keys_held based on events
        apply_events(Events, KeysHeld, NewKeysHeld),
        
        % Inject input(events, held) into context
        ctx_input_ctx(
          Ctx,
          input(events(Events), held(NewKeysHeld)),
          CtxWithInput
        ),
        
        % Tick
        catch(
            tick(
              ctx_in(CtxWithInput), 
              ctx_out(NewCtx)
            ),
            Error,
            ( write('Error during tick: '),
              write(Error), nl,
              throw(Error)
            )
        ),
        NewHistory = [ctx_in(Ctx)|History]
    ;   char_code(Char, 114) ->  % 'r'
        % Reverse: go back
        ( History = [ctx_in(PrevCtx)|RestHistory] ->
            NewCtx = PrevCtx,
            NewHistory = RestHistory,
            % Reconstruct keys_held from PrevCtx
            ctx_input(PrevCtx, 
                      input(_, held(NewKeysHeld)))
        ;
            NewCtx = Ctx,
            NewHistory = History,
            NewKeysHeld = KeysHeld
        )
    ;   char_code(Char, 113) ->  % 'q'
        % Quit
        ctx_status_ctx(Ctx, lost, NewCtx),
        NewHistory = History,
        NewKeysHeld = KeysHeld
    ;
        % Unknown input
        NewCtx = Ctx,
        NewHistory = History,
        NewKeysHeld = KeysHeld
    ).

% apply_events(+Events, +KeysIn, -KeysOut)
% Update key state based on frame events
apply_events([], Keys, Keys).
apply_events(
    [event(key(K), down)|Rest], 
    KeysIn, 
    KeysOut
) :-
    % Add K if not present
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
    % Remove K if present
    ( select(K, KeysIn, KeysTemp) ->
        true
    ;
        KeysTemp = KeysIn
    ),
    apply_events(Rest, KeysTemp, KeysOut).

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
    length(Objects, ObjCount),
    
    % Header
    write('=== Tower Defense ==='), nl,
    write('Frame: '), write(Frame),
    write(' | Status: '), write(Status),
    write(' | Objects: '), write(ObjCount), nl,
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
      get_symbol(Ctx, ID, Symbol) ->
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

get_symbol(Ctx, ID, Symbol) :-
    % Get displayChar attribute (character code as integer)
    % If attribute doesn't exist, predicate fails
    % (object not displayed)
    ctx_attr_val(Ctx, ID/displayChar, Symbol),
    integer(Symbol).

