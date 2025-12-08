:- module(engine, [tick/2, tick_object/4, yields/1]).

:- use_module(library(clpz)).
:- use_module(library(lists), [
    append/3,
    member/2
]).
:- use_module('./execute_action', [execute_action/5]).
:- use_module('./types/accessors'). % import all. so many...
:- use_module('./types/validation', [
    context_validation/1,
    state_validation/1,
    object_validation/1
]).
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
yields(wait(N)) :- N #> 0.
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
    
    % 1. Tick Physics & Logic
    % Iterates by ID, allowing new spawns to be picked up
    % immediately.
    tick_all_objects(ctx_in(CtxIn), ctx_out(CtxPhys)),
    context_validation(CtxPhys),
    
    % 2. Resolve Collisions
    % Removes objects, adds collision hints.
    resolve_collisions(CtxPhys, CtxColl),
    context_validation(CtxColl),
    
    % 3. Increment Frame
    increment_frame(CtxColl, CtxOut).

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

increment_frame(CtxIn, CtxOut) :-
    ctx_frame(CtxIn, F),
    F1 #= F + 1,
    ctx_frame_ctx(CtxIn, F1, CtxOut).

% ==========================================================
% Tick Logic (The ID Cursor)
% ==========================================================

tick_all_objects(ctx_in(CtxIn), ctx_out(CtxOut)) :-
    % Start the loop with ID -1 (assuming IDs start at 0
    % or 1)
    tick_objects_loop(-1, CtxIn, CtxOut).

% The loop finds the next object with ID > LastID
tick_objects_loop(LastID, CtxIn, CtxOut) :-
    ( find_next_object(CtxIn, LastID, TargetObj) ->
        % Found an object to tick
        obj_id(TargetObj, TargetID),
        
        tick_object(
            ctx_old(CtxIn),
            ctx_new(CtxTemp), % Contains spawns/side-effects
            obj_old(TargetObj),
            obj_new(ObjResultList)
        ),
        
        % Update the context with the result of this
        % specific object (Replace TargetObj with
        % ObjResultList in CtxTemp)
        update_object_in_context(
            CtxTemp, TargetID, ObjResultList, CtxNext
        ),
        
        % Recurse using the current TargetID as the new
        % floor
        tick_objects_loop(TargetID, CtxNext, CtxOut)
    ;
        % No more objects with ID > LastID
        CtxOut = CtxIn
    ).

% Helper: Finds the first object in context with ID > MinID
% Since the list is sorted, this is effectively finding
% the next one.
find_next_object(Ctx, MinID, Obj) :-
    ctx_objs(Ctx, Objects),
    member(Obj, Objects),
    obj_id(Obj, ID),
    ID > MinID,
    !. % Take the first one we find (lowest ID > MinID)

% Helper: Replaces the object with TargetID with the
% NewList (which is [Obj] or [])
update_object_in_context(
    CtxIn, TargetID, NewList, CtxOut
) :-
    ctx_objs(CtxIn, Objects),
    replace_by_id(Objects, TargetID, NewList, NewObjects),
    ctx_objs_ctx(CtxIn, NewObjects, CtxOut).

% replace_by_id(+CurrentList, +TargetID, +ReplacementList,
% -NewList)
replace_by_id([], _, _, []).
replace_by_id([Obj|Rest], TargetID, Replacement, Result) :-
    obj_id(Obj, ID),
    ( ID = TargetID ->
        append(Replacement, Rest, Result)
    ;
        Result = [Obj|NewRest],
        replace_by_id(Rest, TargetID, Replacement, NewRest)
    ).


