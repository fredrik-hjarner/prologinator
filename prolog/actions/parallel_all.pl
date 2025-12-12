% parallel_all action implementation

:- module(execute_action_parallel_all, []).

:- use_module(library(lists), [
    append/3,
    member/2
]).
:- use_module('../types/accessors').

:- multifile(execute_action:execute_action_impl/5).
:- discontiguous(execute_action:execute_action_impl/5).

execute_action:execute_action_impl(
    ctx_old(CtxOld),
    ctx_new(CtxNew),
    action(parallel_all(ChildActions)),
    obj_old(ObjIn),
    result(Status, ObjOut)
) :-
    execute_parallel_all(
        CtxOld, CtxNew, ChildActions, ObjIn, Status, ObjOut
    ).

% ==========================================================
% execute_parallel_all/6
% ==========================================================
execute_parallel_all(
    CtxOld, CtxNew, ChildActions, ObjIn, Status, ObjOut
) :-
    obj_acns(ObjIn, [_|Rest]),
    tick_all(
        CtxOld,
        CtxNew,
        ChildActions,
        ObjIn,
        _ObjFinal,
        ChildResults
    ),
    ( member(result(despawned, _), ChildResults) ->
        Status = despawned,
        ObjOut = _
    ;
        filter_running_actions(
            ChildResults, RemainingActions
        ),
        ( RemainingActions = [] ->
            obj_acns_obj(ObjIn, Rest, ObjOut),
            Status = completed
        ;
            obj_acns_obj(
                ObjIn,
                [
                    parallel_all_running(RemainingActions)
                    |Rest
                ],
                ObjOut
            ),
            Status = completed
        )
    ).

execute_action:execute_action_impl(
    ctx_old(CtxOld),
    ctx_new(CtxNew),
    action(parallel_all_running(Children)),
    obj_old(Obj),
    result(Status, ObjOut)
) :-
    execute_parallel_all_running(
        CtxOld, CtxNew, Children, Obj, Status, ObjOut
    ).

% ==========================================================
% execute_parallel_all_running/6
% ==========================================================
execute_parallel_all_running(
    CtxOld, CtxNew, Children, Obj, Status, ObjOut
) :-
    execute_action:execute_action(
        ctx_old(CtxOld),
        ctx_new(CtxNew),
        action(parallel_all(Children)),
        obj_old(Obj),
        result(Status, ObjOut)
    ).

% ==========================================================
% Helpers for parallel_all
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
    run_parallel_all_child(CIn, CTemp, Act, ObjIn, Res),
    ignore_child_result(ObjIn, Res, NextObj),
    tick_all(CTemp, COut, Acts, NextObj, ObjFinal, Results).

% run_parallel_all_child(+CIn, -COut, +Act, +Obj, -Res)
run_parallel_all_child(CIn, COut, Act, Obj, Res) :-
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
% tick_all.

ignore_child_result(Obj, result(_, _), Obj).

% filter_running_actions(+Results, -Actions)
% Recursively filters child results to extract actions
%   from children that are still running.
% Finished children (actions([])) and despawned
%   children are skipped.

filter_running_actions([], []).
filter_running_actions([Res|Rs], Acc) :-
    ( Res = result(
        yielded, object(_, _, actions([A|_]), _)
    ) ->
        % Child still running: include its action
        Acc = [A|RestAcc],
        filter_running_actions(Rs, RestAcc)
    ;
        % Child done or despawned: skip it
        filter_running_actions(Rs, Acc)
    ).

