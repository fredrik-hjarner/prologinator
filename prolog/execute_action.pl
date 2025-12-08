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
:- use_module('./util/util', [select_many/3]).

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
% Basic Actions: wait
% ----------------------------------------------------------

execute_action_impl(
    ctx_old(Ctx),
    ctx_new(Ctx), % Context unchanged
    action(wait(N)),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    obj_acns(ObjIn, [_|Rest]),
    wait_continue(N, Rest, NewActions),
    obj_acns_obj(ObjIn, NewActions, ObjOut).

% Helper: Continue or finish wait action based on remaining
% frames
wait_continue(N, Rest, [wait(N1)|Rest]) :-
    N #> 1,
    N1 #= N - 1.
wait_continue(N, Rest, Rest) :- N #=< 1.

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
    % Extract current x and y separately
    select_many([x(CurrX), y(CurrY)], Attrs, Attrs2),
    % Compute step using integer division
    DX #= (TargetX - CurrX) // Frames,
    DY #= (TargetY - CurrY) // Frames,
    NewX #= CurrX + DX,
    NewY #= CurrY + DY,
    % Label at the boundary where we need ground values for
    % game objects
    (ground(TargetX), ground(TargetY), ground(CurrX),
     ground(CurrY), ground(Frames) ->
        labeling([], [NewX, NewY])
    ;
        true
    ),
    NewAttrs = [x(NewX), y(NewY)|Attrs2],
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
% Basic Actions: noop
% ----------------------------------------------------------

execute_action_impl(
    ctx_old(Ctx),
    ctx_new(Ctx),
    action(noop),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    obj_acns(ObjIn, [_|Rest]),
    obj_acns_obj(ObjIn, Rest, ObjOut).

% ----------------------------------------------------------
% Basic Actions: set_attr
% ----------------------------------------------------------

execute_action_impl(
    ctx_old(Ctx),
    ctx_new(Ctx),
    action(set_attr(Name, Value)),
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
        actions(Rest),
        Colls
    )])
) :-
    % Build attribute term to search for
    functor(Attr, Name, 1),
    % Try to remove old attribute if it exists
    (select(Attr, Attrs, AttrsWithoutOld) ->
        AttrsAfterRemoval = AttrsWithoutOld
    ;
        AttrsAfterRemoval = Attrs
    ),
    % Create new attribute and prepend
    NewAttr =.. [Name, Value],
    NewAttrs = [NewAttr|AttrsAfterRemoval].

% ----------------------------------------------------------
% Compound Actions: spawn
% ----------------------------------------------------------

% IMMEDIATELY spawns the object into the context
execute_action_impl(
    ctx_old(CtxIn),
    ctx_new(CtxOut),
    action(spawn(Type, X, Y, Actions)),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    obj_acns(ObjIn, [_|Rest]),
    obj_acns_obj(ObjIn, Rest, ObjOut),
    
    % 1. Generate ID
    ctx_nextid(CtxIn, ID),
    NextID #= ID + 1,
    ctx_nextid_ctx(CtxIn, NextID, CtxTemp1),
    
    % 2. Create Object with x/y attributes
    NewObj = object(
        id(ID), type(Type), attrs([x(X), y(Y)]),
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
% Compound Actions: list
% ----------------------------------------------------------

execute_action_impl(
    ctx_old(Ctx),
    ctx_new(Ctx),
    action(list(Actions)),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    obj_acns(ObjIn, [_|Rest]),
    append(Actions, Rest, NewActions),
    obj_acns_obj(ObjIn, NewActions, ObjOut).

% ----------------------------------------------------------
% Compound Actions: repeat
% ----------------------------------------------------------

% repeat(+Times, +Actions)
% Mode: repeat(+Times, +Actions)
% Description: Execute action list N times, then
%   continue
% Yields: false (expands immediately)

execute_action_impl(
    ctx_old(Ctx),
    ctx_new(Ctx),
    action(repeat(Times, Acts)),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    obj_acns(ObjIn, [_|Rest]),
    Times #> 0,
    Times1 #= Times - 1,
    % Build next action(s)
    ( Times1 #> 0 ->
        % More reps: add another repeat
        NextRepeat = [repeat(Times1, Acts)]
    ;
        % Last rep: no more repeat
        NextRepeat = []
    ),
    % Queue: Actions + NextRepeat + Rest
    append(Acts, NextRepeat, Tail),
    append(Tail, Rest, NewActions),
    obj_acns_obj(ObjIn, NewActions, ObjOut).

% ----------------------------------------------------------
% Basic Actions: move_delta
% ----------------------------------------------------------

% move_delta(+Frames, +DX, +DY)
% Mode: move_delta(+Frames, +DX, +DY)
% Description: Relative movement: move by (DX, DY)
%   each frame for Frames frames
% Yields: true when Frames > 0

execute_action_impl(
    ctx_old(Ctx),
    ctx_new(Ctx),
    action(move_delta(Frames, DX, DY)),
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
    % Extract current position
    select_many([x(CurrX), y(CurrY)], Attrs, Attrs2),
    % Apply delta
    NewX #= CurrX + DX,
    NewY #= CurrY + DY,
    % Label for grounding
    ( ground(CurrX), ground(CurrY), ground(DX),
      ground(DY) ->
        labeling([], [NewX, NewY])
    ;
        true
    ),
    % Update attributes
    NewAttrs = [x(NewX), y(NewY)|Attrs2],
    % Continue or arrive
    ( Frames #> 1 ->
        Frames1 #= Frames - 1,
        NewActions = [move_delta(Frames1, DX, DY)|Rest]
    ;
        NewActions = Rest
    ).

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
    action(parallel_all(ChildActions)),
    obj_old(ObjIn),
    obj_new(MaybeObjectOut)
) :-
    obj_acns(ObjIn, [_|Rest]),
    tick_all(
        CtxOld,
        CtxNew,
        ChildActions,
        ObjIn,
        ObjFinal,
        ChildResults
    ),
    ( member([], ChildResults) ->
        MaybeObjectOut = []
    ;
        filter_running_actions(
            ChildResults, RemainingActions
        ),
        obj_attrs(ObjFinal, AttrsOut),
        ( RemainingActions = [] ->
            obj_attrs_acns_obj(
                ObjIn, AttrsOut, Rest, NewObj
            ),
            MaybeObjectOut = [NewObj]
        ;
            obj_attrs_acns_obj(
                ObjIn,
                AttrsOut,
                [
                    parallel_all_running(RemainingActions)
                    |Rest
                ],
                NewObj
            ),
            MaybeObjectOut = [NewObj]
        )
    ).

execute_action_impl(
    ctx_old(CtxOld),
    ctx_new(CtxNew),
    action(parallel_all_running(Children)),
    obj_old(Obj),
    obj_new(NewObj)
) :-
    execute_action(
        ctx_old(CtxOld),
        ctx_new(CtxNew),
        action(parallel_all(Children)),
        obj_old(Obj),
        obj_new(NewObj)
    ).

% ----------------------------------------------------------
% Compound Actions: parallel_race
% ----------------------------------------------------------

% parallel_race(+ChildActions)
% Mode: parallel_race(+ChildActions)
% Description: Execute all children in
%   parallel. Stop when ANY child
%   finishes (despawns or action
%   completes).
% Yields: false

execute_action_impl(
    ctx_old(CtxOld),
    ctx_new(CtxNew),
    action(parallel_race(ChildActions)),
    obj_old(ObjIn),
    obj_new(MaybeObjectOut)
) :-
    obj_acns(ObjIn, [_|Rest]),
    % Execute children until one finishes or despawns.
    % Status indicates the outcome:
    %   - done(FinalObj): A child finished/despawned.
    %     Stop immediately, use FinalObj's attrs.
    %   - continue(RunningKids, FinalObj): No child done
    %     yet. Continue racing with RunningKids next tick.
    tick_race(CtxOld, CtxNew, ChildActions, ObjIn, Status),
    ( Status = done(FinalObj) ->
        % Race finished: check if child despawned
        obj_id(ObjIn, ParentID),
        ctx_revhints(CtxNew, RevHints),
        ( member(despawned(ParentID, _), RevHints) ->
            % Child despawned: parent must also despawn
            MaybeObjectOut = []
        ;
            % Child finished normally: proceed with parent
            %   actions
            obj_attrs(FinalObj, AttrsOut),
            obj_attrs_acns_obj(
                ObjIn, AttrsOut, Rest, NewObj
            ),
            MaybeObjectOut = [NewObj]
        )
    ; Status = continue(RunningKids, FinalObj) ->
        % Race continues: wrap remaining children in
        %   parallel_race_running for next tick
        obj_attrs(FinalObj, AttrsOut),
        obj_attrs_acns_obj(
            ObjIn,
            AttrsOut,
            [parallel_race_running(RunningKids)|Rest],
            NewObj
        ),
        MaybeObjectOut = [NewObj]
    ).

% Handle parallel_race_running triggered
%   from race
%
% parallel_race_running(+ChildActions)
% Mode: parallel_race_running(+ChildActions)
% Description: Continue racing when
%   children didn't all finish

execute_action_impl(
    ctx_old(CtxOld),
    ctx_new(CtxNew),
    action(parallel_race_running(Children)),
    obj_old(Obj),
    obj_new(NewObj)
) :-
    execute_action_impl(
        ctx_old(CtxOld),
        ctx_new(CtxNew),
        action(parallel_race(Children)),
        obj_old(Obj),
        obj_new(NewObj)
    ).

% ==========================================================
% Helpers for Parallel Execution
% ==========================================================

% tick_all(+CIn, -COut, +Actions, +ObjIn,
%   -ObjFinal, -Results)
% Executes ALL children sequentially, continuing even
%   if one despawns or finishes. All children get their
%   turn to execute in this tick.
%
% Unlike tick_race, this does NOT stop early. Every
%   child is executed regardless of what happens to
%   previous children.

tick_all(C, C, [], Obj, Obj, []).
tick_all(
    CIn, COut, [Act|Acts], ObjIn, ObjFinal, [Res|Results]
) :-
    run_child(CIn, CTemp, Act, ObjIn, Res),
    update_obj_attrs(ObjIn, Res, NextObj),
    tick_all(CTemp, COut, Acts, NextObj, ObjFinal, Results).

% tick_race(+CIn, -COut, +Actions, +ObjIn, -Status)
% Executes children sequentially until one finishes
%   or despawns, then stops immediately.
%
% Status indicates the outcome:
%   - done(FinalObj): A child finished (actions empty)
%     or despawned (result is []). Execution stops here.
%     FinalObj has attrs updated from the winning child.
%   - continue(RunningKids, FinalObj): No child finished
%     yet.
%     RunningKids is the list of actions still running.
%     FinalObj has attrs from all children processed so far.
%
% This implements "fail-fast" behavior: as soon as any
%   child completes, remaining children are NOT executed
%   in this tick.

tick_race(C, C, [], Obj, continue([], Obj)).
tick_race(CIn, COut, [Act|Acts], ObjIn, Status) :-
    run_child(CIn, CTemp, Act, ObjIn, Res),
    ( is_child_done(Res) ->
        % Winner found: stop immediately, don't process
        %   remaining children
        COut = CTemp,
        update_obj_attrs(ObjIn, Res, FinalObj),
        Status = done(FinalObj)
    ;
        % This child still running: continue with next
        %   sibling
        update_obj_attrs(ObjIn, Res, NextObj),
        tick_race(CTemp, COut, Acts, NextObj, RecStatus),
        ( RecStatus = continue(RestKids, FinalObj) ->
            % Race still going: collect this child's action
            extract_action(Res, NewAct),
            Status = continue([NewAct|RestKids], FinalObj)
        ;
            % A later child finished: propagate the done
            %   status up
            Status = RecStatus
        )
    ).

% run_child(+CIn, -COut, +Act, +Obj, -Res)
run_child(CIn, COut, Act, Obj, Res) :-
    obj_id(Obj, ID),
    obj_type(Obj, T),
    obj_attrs(Obj, A),
    obj_collisions(Obj, C),
    Child = object(
        id(ID),
        type(T),
        attrs(A),
        actions([Act]),
        collisions(C)
    ),
    execute_action(
        ctx_old(CIn),
        ctx_new(COut),
        action(Act),
        obj_old(Child),
        obj_new(Res)
    ).

% update_obj_attrs(+Obj, +ResultList, -NewObj)
% Updates object attributes based on child result.
%   - If result is an object, use its attrs
%   - If result is [] (despawned), keep old attrs
%     (so next child can still run with old state)

update_obj_attrs(
    Obj, [object(_, _, attrs(A), _, _)], New
) :-
    !,
    obj_attrs_obj(Obj, A, New).
update_obj_attrs(Obj, [], Obj).

% is_child_done(+ResultList)
% True if ResultList indicates the child is done:
%   - [] means child despawned
%   - [object(..., actions([]), ...)] means child
%     finished (no remaining actions)

is_child_done([]).
is_child_done([object(_, _, _, actions([]), _)]).

% extract_action(+ResultList, -Action)
% Extracts the next action from a running child's
%   result. The child is still running if it has
%   actions remaining.

extract_action([object(_, _, _, actions([A|_]), _)], A).

% filter_running_actions(+Results, -Actions)
% Recursively filters child results to extract actions
%   from children that are still running.
% Finished children (actions([])) and despawned
%   children ([]) are skipped.

filter_running_actions([], []).
filter_running_actions([Res|Rs], Acc) :-
    ( Res = [object(_, _, _, actions([A|_]), _)] ->
        % Child still running: include its action
        Acc = [A|RestAcc],
        filter_running_actions(Rs, RestAcc)
    ;
        % Child done or despawned: skip it
        filter_running_actions(Rs, Acc)
    ).

