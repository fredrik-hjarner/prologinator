:- module(engine, [tick/2, tick_object/6, yields/1]).

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
:- use_module('./execute_action', [execute_action/7]).
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

tick_object(
    ctx_old(Ctx),
    ctx_new(Ctx),
    obj_old(Obj),
    obj_new([Obj]),
    cmds_new([]),
    revhints_new([])
) :-
    obj_acns(Obj, []), % guard.
    !. % TODO: Can this cut hurt bidirectionality?

% Observe that ObjNew is either [object] or [].
% It's empty list when the object have been removed.
tick_object(
    ctx_old(CtxOld),
    ctx_new(CtxNew),
    obj_old(ObjOld),
    obj_new(ObjNew),
    cmds_new(CmdsNew),
    revhints_new(RevHintsNew)
) :-
    object_validation(ObjOld),
    obj_acns(ObjOld, [Act|_Rest]),
    execute_action(
        ctx_old(CtxOld),
        ctx_new(CtxTemp),
        action(Act),
        obj_old(ObjOld),
        obj_new(ObjTempList),
        cmds_new(C1),
        revhints_new(R1)
    ),
    ( ObjTempList = [] ->
        ObjNew = [],
        CmdsNew = C1,
        RevHintsNew = R1,
        CtxNew = CtxTemp
    ; yields(Act) ->
        ObjNew = ObjTempList,
        CmdsNew = C1,
        RevHintsNew = R1,
        CtxNew = CtxTemp
    ;
        ObjTempList = [ObjTemp],
        tick_object(
            ctx_old(CtxTemp),
            ctx_new(CtxNew),
            obj_old(ObjTemp),
            obj_new(ObjNew),
            cmds_new(C2),
            revhints_new(R2)
        ),
        append(C1, C2, CmdsNew),
        append(R1, R2, RevHintsNew)
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

% Helper predicate for maplist - wraps tick_object
tick_object_with_ctx(
    CtxOld, CtxNew, ObjOld, ObjNew, Cmds, RevHints
) :-
    tick_object(
        ctx_old(CtxOld),
        ctx_new(CtxNew),
        obj_old(ObjOld),
        obj_new(ObjNew),
        cmds_new(Cmds),
        revhints_new(RevHints)
    ).

% TODO: It got messy here with context... need to refactor.
tick_all_objects(ctx_in(CtxIn), ctx_out(CtxOut)) :-
    ctx_objs_cmds_revhints(
        CtxIn, Objects, OldCommands, OldRevHints
    ),
    maplist(
        tick_object_with_ctx(CtxIn, _CtxTemp),
        Objects, % "input"
        TempObjectLists, % collected output (list of lists)
        CommandLists, % collected "output"
        RevHintLists % collected "output"
    ),
    % flatten/2 flattens lists, append/3 combines
    flatten(CommandLists, NewCommands),
    append(OldCommands, NewCommands, AllCommands),
    % flatten/2 flattens list of lists, append/3 combines
    flatten(RevHintLists, NewRevHints),
    append(OldRevHints, NewRevHints, AllRevHints),
    % Flatten list of lists and filter out empty lists
    flatten(TempObjectLists, FinalObjects),
    ctx_objs_cmds_revhints_ctx(
        CtxIn,
        FinalObjects, AllCommands, AllRevHints,
        CtxOut
    ).

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

