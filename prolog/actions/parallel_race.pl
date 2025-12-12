% parallel_race action implementation

:- module(execute_action_parallel_race, []).

:- use_module(library(lists), [
    append/3
]).
:- use_module('../types/accessors').

:- multifile(execute_action:execute_action_impl/5).
:- discontiguous(execute_action:execute_action_impl/5).

% parallel_race(+ChildActions)
% Mode: parallel_race(+ChildActions)
% Description: Execute all children in
%   parallel. Stop when ANY child
%   finishes (despawns or action
%   completes).
% Yields: false

execute_action:execute_action_impl(
    ctx_old(CtxOld),
    ctx_new(CtxNew),
    action(parallel_race(ChildActions)),
    obj_old(ObjIn),
    result(Status, ObjOut)
) :-
    execute_parallel_race(
        CtxOld, CtxNew, ChildActions, ObjIn, Status, ObjOut
    ).

% ==========================================================
% execute_parallel_race/6
% ==========================================================
execute_parallel_race(
    CtxOld, CtxNew, ChildActions, ObjIn, FinalStatus, ObjOut
) :-
    obj_acns(ObjIn, [_|Rest]),
    % Execute children until one finishes or despawns.
    % Status indicates the outcome:
    %   - done(FinalObj): A child finished/despawned.
    %     Stop immediately, use FinalObj's attrs.
    %   - continue(RunningKids, FinalObj): No child done
    %     yet. Continue racing with RunningKids next tick.
    tick_race(CtxOld, CtxNew, ChildActions, ObjIn, Status),
    ( Status = done(_FinalObj, Despawned) ->
        ( Despawned = true ->
            % Child despawned: parent must also despawn
            FinalStatus = despawned,
            ObjOut = _
        ;
            % Child finished normally: proceed with parent
            %   actions (attributes already in store by ID)
            obj_acns_obj(ObjIn, Rest, ObjOut),
            FinalStatus = completed
        )
    ; Status = continue(RunningKids, _FinalObj) ->
        % Race continues: wrap remaining children in
        %   parallel_race_running for next tick
        % (attributes already in store by ID)
        obj_acns_obj(
            ObjIn,
            [parallel_race_running(RunningKids)|Rest],
            ObjOut
        ),
        FinalStatus = completed
    ).

% Handle parallel_race_running triggered
%   from race
%
% parallel_race_running(+ChildActions)
% Mode: parallel_race_running(+ChildActions)
% Description: Continue racing when
%   children didn't all finish

execute_action:execute_action_impl(
    ctx_old(CtxOld),
    ctx_new(CtxNew),
    action(parallel_race_running(Children)),
    obj_old(Obj),
    result(Status, ObjOut)
) :-
    execute_parallel_race_running(
        CtxOld, CtxNew, Children, Obj, Status, ObjOut
    ).

% ==========================================================
% execute_parallel_race_running/6
% ==========================================================
execute_parallel_race_running(
    CtxOld, CtxNew, Children, Obj, Status, ObjOut
) :-
    execute_action:execute_action(
        ctx_old(CtxOld),
        ctx_new(CtxNew),
        action(parallel_race(Children)),
        obj_old(Obj),
        result(Status, ObjOut)
    ).

% ==========================================================
% Helpers for parallel_race
% ==========================================================

% tick_race(+CIn, -COut, +Actions, +ObjIn, -Status)
% Executes children sequentially until one finishes
%   or despawns, then stops immediately.
%
% Status indicates the outcome:
%   - done(FinalObj, Despawned): A child finished
%     (actions empty) or despawned (result is []).
%     Execution stops here.
%     FinalObj has attrs updated from the winning child.
%     Despawned is true if child despawned,
%     false if finished.
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
        ignore_child_result(ObjIn, Res, FinalObj),
        ( Res = result(despawned, _) ->
            Despawned = true
        ;
            Despawned = false
        ),
        Status = done(FinalObj, Despawned)
    ;
        % This child still running: continue with next
        %   sibling
        ignore_child_result(ObjIn, Res, NextObj),
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
    obj_collisions(Obj, C),
    % Create child object (attrs in centralized store)
    Child = object(
        id(ID),
        type(T),
        actions([Act]),
        collisions(C)
    ),
    execute_action:execute_action(
        ctx_old(CIn),
        ctx_new(COut),
        action(Act),
        obj_old(Child),
        result(Status, ObjTemp)
    ),
    Res = result(Status, ObjTemp).

% ignore_child_result(+Obj, +Result, -NewObj)
% Ignores child result and returns the parent object
% unchanged.
% Attributes are in centralized store, so no need to copy
% them.
% The child updates Ctx directly, which is threaded via
% tick_race.

ignore_child_result(Obj, result(_, _), Obj).

% is_child_done(+Result)
% True if Result indicates the child is done:
%   - result(despawned, _) means child despawned
%   - result(completed, object(..., actions([]), ...))
%     means child finished (no remaining actions)

is_child_done(result(despawned, _)).
is_child_done(result(
    completed, object(_, _, actions([]), _)
)).

% extract_action(+Result, -Action)
% Extracts the next action from a running child's
%   result. The child is still running if it has
%   actions remaining.

extract_action(
    result(yielded, object(_, _, actions([A|_]), _)), A
).

