:- module(engine, [tick/2, tick_object/4, yields/1]).

:- use_module(library(clpz)).
:- use_module(library(lists), [
    append/3,
    select/3,
    member/2,
    maplist/5,
    list_to_set/2,
    length/2
]).
:- use_module('./third_party/exclude', [exclude/3]).
:- use_module('./execute_action', [execute_action/5]).
:- use_module('./types/accessors'). % import all. so many...
:- use_module('./types/validation', [
    context_validation/1,
    state_validation/1,
    object_validation/1
]).
:- use_module('./util/util', [partition/4, flatten/2]).
:- use_module('./collisions', [detect_collisions/3]).

% ==========================================================
% Yielding Actions
% ==========================================================
% Bidirectional: works forward (check if action yields) and
% backward (generate yielding actions)
% Modes: yields(?Action) - can be called with Action bound
% or unbound
% :- pred yields(?Action) # "Checks if an action yields \
% control, or generates yielding actions.".

% yields tells whether an action "yields" or not.
% THis is how it works now: AFTER an action has been
% executed, we check if it yields. If it does yield then
% we DONT executa the next action. However it it does NOT
% yield then we execute the next action etc until EITHER
% the game object cease to exist or we come to a yielding
% action.
yields(wait_frames(N)) :- N #> 0.
yields(move_to(_, _, Frames)) :- Frames #> 0.
yields(parallel_running(_)).

% ==========================================================
% Execution Model: tick_object
% ==========================================================

% TODO: Add a tick_object wrapper that wraps a
% tick_object_impl. though refactorings are needed first...

% % forward execution
% :- pred tick_object(+ObjIn, -ObjOut, -AllCommands,
% -AllRevHints).

% tick_object/4 threads the context.
% Cmds and RevHints are now appended directly to CtxNew.
tick_object(
    ctx_old(Ctx),
    ctx_new(Ctx),
    obj_old(Obj),
    obj_new([Obj])
) :-
    obj_acns(Obj, []), % guard.
    !. % TODO: Can this cut hurt bidirectionality?

tick_object(
    ctx_old(CtxOld),
    ctx_new(CtxNew),
    obj_old(ObjOld),
    obj_new(ObjNew)
) :-
    object_validation(ObjOld),
    obj_acns(ObjOld, [Act|_Rest]),
    execute_action(
        ctx_old(CtxOld),
        ctx_new(CtxTemp),
        action(Act),
        obj_old(ObjOld),
        obj_new(ObjTempList)
    ),
    ( ObjTempList = [] ->
        ObjNew = [],
        CtxNew = CtxTemp
    ; yields(Act) ->
        ObjNew = ObjTempList,
        CtxNew = CtxTemp
    ;
        % Action finished immediately (did not yield),
        % recurse
        ObjTempList = [ObjTemp],
        tick_object(
            ctx_old(CtxTemp),
            ctx_new(CtxNew),
            obj_old(ObjTemp),
            obj_new(ObjNew)
        )
    ).

% ==========================================================
% Main Tick Function
% ==========================================================
tick(ctx_in(CtxIn), ctx_out(CtxOut)) :-
    context_validation(CtxIn),
    
    % The Pipeline:
    % 1. Tick Physics (Moves objects, generates raw
    % commands/hints)
    tick_all_objects(ctx_in(CtxIn), ctx_out(CtxPhys)),
    context_validation(CtxPhys),
    
    % 2. Resolve Collisions (Updates objects, adds
    % collision hints)
    resolve_collisions(CtxPhys, CtxColl),
    context_validation(CtxColl),
    
    % 3. Resolve Spawns (Consumes spawn commands, updates
    % objects & ID)
    resolve_spawns(CtxColl, CtxSpawn),
    context_validation(CtxSpawn),
    
    % 4. Resolve Status (Consumes state commands, updates
    % status)
    resolve_status(CtxSpawn, CtxFinal),
    context_validation(CtxFinal),
    
    % 5. Increment Frame
    increment_frame(CtxFinal, CtxOut).

% ==========================================================
% Pipeline Stages
% ==========================================================

resolve_collisions(CtxIn, CtxOut) :-
    ctx_objs_cmds_revhints(CtxIn, Objs, Cmds, Revs1),
    detect_collisions(Objs, NewObjs, Revs2),
    append(Revs1, Revs2, AllRevs),
    ctx_objs_cmds_revhints_ctx(
        CtxIn,
        NewObjs, Cmds, AllRevs,
        CtxOut
    ).

resolve_spawns(CtxIn, CtxOut) :-
    ctx_objs_nextid_cmds(
        CtxIn, CurrentObjs, NextID, AllCmds
    ),
    
    partition(
        is_spawn_request, AllCmds, SpawnReqs, OtherCmds
    ),
    
    process_spawn_requests(
        SpawnReqs, NextID, NewObjs, NewNextID
    ),
    append(CurrentObjs, NewObjs, FinalObjs),
    
    % Update context: New Objects, New ID, Remaining
    % Commands
    ctx_objs_nextid_cmds_ctx(
        CtxIn,
        FinalObjs, NewNextID, OtherCmds,
        CtxOut
    ).

resolve_status(CtxIn, CtxOut) :-
    ctx_status_cmds(CtxIn, Status, Cmds),
    
    partition(
        is_state_change, Cmds, StatusCmds, RemainingCmds
    ),
    
    apply_state_changes(StatusCmds, Status, NewStatus),
    
    ctx_status_cmds_ctx(
        CtxIn,
        NewStatus, RemainingCmds,
        CtxOut
    ).

increment_frame(CtxIn, CtxOut) :-
    ctx_frame(CtxIn, F),
    F1 #= F + 1,
    ctx_frame_ctx(CtxIn, F1, CtxOut).

% ==========================================================
% Tick Helpers
% ==========================================================

% Replaces maplist/flatten/append with a threaded fold
tick_all_objects(ctx_in(CtxIn), ctx_out(CtxOut)) :-
    ctx_objs(CtxIn, Objects),
    tick_objects_loop(
        Objects, CtxIn, FinalObjects, CtxTemp
    ),
    % Update objects in the resulting context
    ctx_objs_ctx(CtxTemp, FinalObjects, CtxOut).

% Custom recursive loop to thread Context and build
% Object list
tick_objects_loop([], Ctx, [], Ctx).
tick_objects_loop([Obj|Rest], CtxIn, FinalObjs, CtxOut) :-
    tick_object(
        ctx_old(CtxIn),
        ctx_new(CtxTemp),
        obj_old(Obj),
        obj_new(ObjResultList)
    ),
    tick_objects_loop(Rest, CtxTemp, RestObjs, CtxOut),
    append(ObjResultList, RestObjs, FinalObjs).

% ==========================================================
% Spawn Processing (from Addendum 3)
% ==========================================================
is_spawn_request(spawn_request(_, _, _)).

process_spawn_requests([], ID, [], ID).
process_spawn_requests(
    [spawn_request(Type, Pos, Acts)|Rest],
    IDIn,
    [object(
        id(ObjID),
        type(Type),
        attrs([Pos]),
        actions(Acts),
        collisions([])
    )|RestObjs],
    IDOut
) :-
    % Generate ID: just use the integer directly
    ObjID = IDIn,
    
    IDIn1 #= IDIn + 1,
    
    process_spawn_requests(Rest, IDIn1, RestObjs, IDOut).

% ==========================================================
% State Change Processing
% ==========================================================
is_state_change(state_change(_)).

apply_state_changes([], Status, Status).
apply_state_changes(
    [state_change(game_over(lost))|_],
    _, lost
) :- !.
apply_state_changes(
    [state_change(game_over(won))|Rest],
    Status, FinalStatus
) :-
    ( Status = lost ->
        FinalStatus = lost
    ;
        apply_state_changes(Rest, won, FinalStatus)
    ).
apply_state_changes(
    [_|Rest], Status, FinalStatus
) :-
    apply_state_changes(Rest, Status, FinalStatus).

