:- module(engine, [tick/2, tick_object/4, yields/2]).

:- use_module(library(clpz)).
:- use_module(library(lists), [
    append/3,
    member/2
]).
:- use_module(library(reif), [if_/3, (=)/3]).
:- use_module(library(dif), [dif/2]).
:- use_module('./execute_action', [execute_action/5]).
:- use_module('./types/accessors'). % import all. so many...
:- use_module('./types/validation', [
    context_validation/1,
    state_validation/1,
    object_validation/1
]).
:- use_module('./collisions', [detect_collisions/2]).

% ==========================================================
% Yielding Actions (Reified)
% ==========================================================
% Bidirectional: works forward (check if action yields) and
% backward (generate yielding actions)
% Modes: yields(?Action, ?T) - can be called with Action
% bound or unbound, T is true or false
% :- pred yields(?Action, ?T) # "Reified: T=true if action
% yields, T=false otherwise. Can generate yielding actions
% when Action is unbound and T=true.".

% yields tells whether an action "yields" or not.
% This is how it works now: AFTER an action has been
% executed, we check if it yields. If it does yield then
% we DONT execute the next action. However it it does NOT
% yield then we execute the next action etc until EITHER
% the game object cease to exist or we come to a yielding
% action.
% Reified version: always takes a truth value T.
% Fully relational: pattern matching + CLP constraints.

% wait(N) yields when N > 0
yields(wait(N), true) :- N #> 0.
yields(wait(N), false) :- N #=< 0.

% move_to(_, _, Frames) yields when Frames > 0
yields(move_to(_, _, Frames), true) :- Frames #> 0.
yields(move_to(_, _, Frames), false) :- Frames #=< 0.

% parallel_all_running(_) always yields
yields(parallel_all_running(_), true).
% parallel_race_running(_) always yields
yields(parallel_race_running(_), true).

% Non-yielding actions: despawn, spawn, loop,
% trigger_state_change, parallel_all, noop, list, set_attr,
% repeat, define_action
yields(despawn, false).
yields(spawn(_, _, _, _), false).
yields(loop(_), false).
yields(trigger_state_change(_), false).
yields(parallel_all(_), false).
% parallel_race(_) always yields false
%   (will be unwrapped immediately if
%   any child finishes)
yields(parallel_race(_), false).
yields(noop, false).
yields(list(_), false).
yields(set_attr(_, _), false).
% repeat(Times, Actions) does NOT yield
%   (expands immediately into action list)
yields(repeat(_, _), false).
% define_action(_, _) does NOT yield
%   (expands immediately, stores definition)
yields(define_action(_, _), false).

% move_delta(Frames, _, _) yields when Frames > 0
yields(move_delta(Frames, _, _), true) :-
    Frames #> 0.
yields(move_delta(Frames, _, _), false) :-
    Frames #=< 0.

% ==========================================================
% Execution Model: tick_object
% ==========================================================

% TODO: Add a tick_object wrapper that wraps a
% tick_object_impl. though refactorings are needed first...

% % forward execution
% :- pred tick_object(+ObjIn, -ObjOut, -AllCommands,
% -AllRevHints).

% tick_object/4 threads the context.
% Cmds are now appended directly to CtxNew.
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
    if_(=(ObjTempList, []),
        ( ObjNew = [],
          CtxNew = CtxTemp
        ),
        if_(yields(Act),
            ( ObjNew = ObjTempList,
              CtxNew = CtxTemp
            ),
            ( % Action finished immediately (did not yield),
              % recurse
              ObjTempList = [ObjTemp],
              tick_object(
                  ctx_old(CtxTemp),
                  ctx_new(CtxNew),
                  obj_old(ObjTemp),
                  obj_new(ObjNew)
              )
            )
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
    detect_collisions(ctx_old(CtxPhys), ctx_new(CtxColl)),
    context_validation(CtxColl),
    
    % 3. Increment Frame
    increment_frame(CtxColl, CtxOut).

% ==========================================================
% Pipeline Stages
% ==========================================================

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


