% Action Execution Module
% Handles execution of all game actions

:- module(execute_action, [execute_action/5]).

:- use_module(library(clpz)).
:- use_module(library(lists), [
    append/2,
    append/3,
    select/3,
    member/2,
    findall/3
]).
:- use_module('./types/validation', [action_validation/1]).
:- use_module('./types/accessors').

% :- use_module('xod/xod', [validate/2]).
% :- use_module('./types/validation2').

% execute_action_impl/5 clauses are intentionally separated
% by other code
:- discontiguous(execute_action_impl/5).

% ==========================================================
% execute_action/5
% ==========================================================

% Forward execution (normal game)
% :- pred execute_action(+Action, +ObjIn, -ObjOut,
%     -Commands, -RevHints) 
%     :: (action(Action), game_object(ObjIn)) 
%     => (action(Action), game_object(ObjOut),
%     list(command, Commands), list(rev_hint, RevHints))
%     + is_det.

% Reverse execution (undo/replay)
% :- pred execute_action(-Action, -ObjIn, +ObjOut,
%     +Commands, +RevHints) + is_det.

% Action inference - which must the action have been?
% :- pred execute_action(-Action, +ObjIn, +ObjOut,
%     +Commands, +RevHints).

% Validation (consistency check)
% :- pred execute_action(+Action, +ObjIn, +ObjOut,
%     +Commands, +RevHints).

% Wrapper: validates action then delegates to implementation
% Now threads Context directly to accumulate side effects.
execute_action(
    ctx_old(CtxOld),
    ctx_new(CtxNew),
    action(Action),
    obj_old(ObjIn),
    obj_new(ObjOut)
) :-
    action_validation(Action),
    % validate(Action, action_schema),
    execute_action_impl(
        ctx_old(CtxOld),
        ctx_new(CtxNew),
        action(Action),
        obj_old(ObjIn),
        obj_new(ObjOut)
    ).

% ==========================================================
% execute_action_impl/5
% ==========================================================
% Internal implementation (no validation)
% ----------------------------------------------------------
% Basic Actions: wait_frames
% ----------------------------------------------------------

execute_action_impl(
    ctx_old(Ctx),
    ctx_new(Ctx), % Context unchanged
    action(wait_frames(N)),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    obj_acns(ObjIn, [_|Rest]),
    ( N #> 1 ->
        N1 #= N - 1,
        NewActions = [wait_frames(N1)|Rest]
    ;
        % N = 1, wait is done
        NewActions = Rest
    ),
    obj_acns_obj(ObjIn, NewActions, ObjOut).

% ----------------------------------------------------------
% Basic Actions: move_to
% ----------------------------------------------------------

execute_action_impl(
    ctx_old(Ctx),
    ctx_new(Ctx), % Context unchanged
    action(move_to(TargetX, TargetY, Frames)),
    obj_old(object(
        id(ID),
        type(Type),
        attrs(Attrs),
        actions([_|Rest]),
        Colls
    )),
    obj_new([object(
        id(ID),
        type(Type),
        attrs(NewAttrs),
        actions(NewActions),
        Colls
    )])
) :-
    select(pos(CurrentX, CurrentY), Attrs, RestAttrs),
    % Compute step using integer division
    DX #= (TargetX - CurrentX) // Frames,
    DY #= (TargetY - CurrentY) // Frames,
    NewX #= CurrentX + DX,
    NewY #= CurrentY + DY,
    % Label at the boundary where we need ground values for
    % game objects
    (ground(TargetX), ground(TargetY), ground(CurrentX),
     ground(CurrentY), ground(Frames) ->
        labeling([], [NewX, NewY])
    ;
        true
    ),
    NewAttrs = [pos(NewX, NewY)|RestAttrs],
    ( Frames #> 1 ->
        Frames1 #= Frames - 1,
        NewActions = [move_to(TargetX, TargetY, Frames1)|
            Rest]
    ;
        NewActions = Rest  % Arrived
    ).

% ----------------------------------------------------------
% Basic Actions: despawn
% ----------------------------------------------------------

execute_action_impl(
    ctx_old(CtxIn),
    ctx_new(CtxOut),
    action(despawn),
    obj_old(object(id(ID), _, attrs(Attrs), _, _)),
    obj_new([])
) :-
    ctx_revhints(CtxIn, Revs),
    append(Revs, [despawned(ID, Attrs)], NewRevs),
    ctx_revhints_ctx(CtxIn, NewRevs, CtxOut).

% ----------------------------------------------------------
% Compound Actions: spawn
% ----------------------------------------------------------

% IMMEDIATELY spawns the object into the context
execute_action_impl(
    ctx_old(CtxIn),
    ctx_new(CtxOut),
    action(spawn(Type, Pos, Actions)),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    obj_acns(ObjIn, [_|Rest]),
    obj_acns_obj(ObjIn, Rest, ObjOut),
    
    % 1. Generate ID
    ctx_nextid(CtxIn, ID),
    NextID #= ID + 1,
    ctx_nextid_ctx(CtxIn, NextID, CtxTemp1),
    
    % 2. Create Object
    NewObj = object(
        id(ID), type(Type), attrs([Pos]),
        actions(Actions), collisions([])
    ),
    
    % 3. Append to Context
    % Since ID is increasing, appending to end keeps the
    % list sorted.
    ctx_objs(CtxTemp1, CurrentSpawns),
    append(CurrentSpawns, [NewObj], NewSpawns),
    ctx_objs_ctx(CtxTemp1, NewSpawns, CtxOut).

% ----------------------------------------------------------
% Compound Actions: loop
% ----------------------------------------------------------

execute_action_impl(
    ctx_old(Ctx),
    ctx_new(Ctx),
    action(loop(Actions)),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    obj_acns(ObjIn, [_|Rest]),
    append(Actions, [loop(Actions)], Expanded),
    append(Expanded, Rest, NewActions),
    obj_acns_obj(ObjIn, NewActions, ObjOut).

% ----------------------------------------------------------
% Compound Actions: trigger_state_change
% ----------------------------------------------------------

% IMMEDIATELY updates status in context
execute_action_impl(
    ctx_old(CtxIn),
    ctx_new(CtxOut),
    action(trigger_state_change(Change)),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    obj_acns(ObjIn, [_|Rest]),
    obj_acns_obj(ObjIn, Rest, ObjOut),
    
    ctx_status(CtxIn, CurrentStatus),
    update_status(Change, CurrentStatus, NewStatus),
    ctx_status_ctx(CtxIn, NewStatus, CtxOut).

update_status(game_over(lost), _, lost).
update_status(game_over(won), lost, lost).
update_status(game_over(won), _, won).
% update_status(_, S, S). % fallback

% ----------------------------------------------------------
% Compound Actions: parallel
% ----------------------------------------------------------

execute_action_impl(
    ctx_old(CtxOld),
    ctx_new(CtxNew),
    action(parallel(ChildActions)),
    obj_old(ObjIn),
    obj_new(MaybeObjectOut)
) :-
    ObjIn = object(_, _, _, actions([_|Rest]), _),
    tick_parallel_children(
        ctx_old(CtxOld),
        ctx_new(CtxNew),
        ChildActions, % list of actions that run in parallel
        ObjIn,
        ObjFinal,
        ChildResults
    ),
    obj_attrs(ObjFinal, AttrsOut),
    ( member([], ChildResults) ->
        % Any child despawned, so despawn the whole parallel
        MaybeObjectOut = []
    ;
        % All children are alive. Filter out "Done" ones.
        % Look inside objects returned in ChildResults.
        findall( % pick out
            Action,
            (
                member([ChildObj], ChildResults),
                ChildObj = object(
                    _, _, _, actions([Action|_]), _
                )
            ),
            RemainingActions
        ),
        ( RemainingActions = [] ->
            % All children finished naturally.
            obj_attrs_acns_obj(
                ObjIn, AttrsOut, Rest, NewObj
            ),
            MaybeObjectOut = [NewObj]
        ;
            % Some children are still running.
            obj_attrs_acns_obj(
                ObjIn,
                AttrsOut,
                [parallel_running(RemainingActions)|Rest],
                NewObj
            ),
            MaybeObjectOut = [NewObj]
        )
    ).

execute_action_impl(
    ctx_old(CtxOld),
    ctx_new(CtxNew),
    action(parallel_running(Children)),
    obj_old(Obj),
    obj_new(NewObj)
) :-
    execute_action(
        ctx_old(CtxOld),
        ctx_new(CtxNew),
        action(parallel(Children)),
        obj_old(Obj),
        obj_new(NewObj)
    ).

% ==========================================================
% tick_parallel_children/6
% ==========================================================

% :- pred tick_parallel_children(
%     ?Children, % actions_list
%     ?ObjIn, % game_object
%     ?ObjFinal, % game_object with final attrs
%     ?UpdatedChildren, % list of result lists
%     ?AllCommands,
%     ?AllRevHints).

tick_parallel_children(
    ctx_old(Ctx),
    ctx_new(Ctx),
    [], ObjIn, ObjIn, []
).

tick_parallel_children(
    ctx_old(CtxOld),
    ctx_new(CtxNew),
    [Child|RestChildren],
    ObjIn,
    ObjFinal,
    [ChildResult|RestResults]
) :-
    ObjIn = object(
        id(ID), type(Type), attrs(AttrsIn), actions(_),
        collisions(Colls)
    ),

    % 1. Construct input object for this child action
    ChildObjIn = object(
        id(ID), type(Type), attrs(AttrsIn),
        actions([Child]), collisions(Colls)
    ),

    % 2. Execute the action
    execute_action(
        ctx_old(CtxOld),
        ctx_new(CtxTemp),
        action(Child),
        obj_old(ChildObjIn),
        obj_new(ResultList)
    ),

    % 3. Determine attributes for the NEXT child.
    %    If this child killed the object, we pass the OLD
    %    attributes to the next sibling so it can at least
    %    attempt to run (e.g., to generate its own
    %    commands/hints).
    ( ResultList = [object(_, _, attrs(AttrsNext), _, _)] ->
        true
    ;
        ResultList = [],
        AttrsNext = AttrsIn
    ),

    % 4. Build object for next child
    NextObjIn = object(
        id(ID), type(Type), attrs(AttrsNext), actions(_),
        collisions(Colls)
    ),

    % 5. Recurse
    tick_parallel_children(
        ctx_old(CtxTemp),
        ctx_new(CtxNew),
        RestChildren,
        NextObjIn,
        ObjFinal,
        RestResults
    ),
    
    % 6. Build output
    ChildResult = ResultList.

