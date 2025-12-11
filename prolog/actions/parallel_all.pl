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
    obj_new(MaybeObjectOut)
) :-
    execute_parallel_all(
        CtxOld, CtxNew, ChildActions, ObjIn, MaybeObjectOut
    ).

% ==========================================================
% execute_parallel_all/5
% ==========================================================
execute_parallel_all(
    CtxOld, CtxNew, ChildActions, ObjIn, MaybeObjectOut
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
    ( member([], ChildResults) ->
        MaybeObjectOut = []
    ;
        filter_running_actions(
            ChildResults, RemainingActions
        ),
        ( RemainingActions = [] ->
            obj_acns_obj(ObjIn, Rest, NewObj),
            MaybeObjectOut = [NewObj]
        ;
            obj_acns_obj(
                ObjIn,
                [
                    parallel_all_running(RemainingActions)
                    |Rest
                ],
                NewObj
            ),
            MaybeObjectOut = [NewObj]
        )
    ).

execute_action:execute_action_impl(
    ctx_old(CtxOld),
    ctx_new(CtxNew),
    action(parallel_all_running(Children)),
    obj_old(Obj),
    obj_new(NewObj)
) :-
    execute_parallel_all_running(
        CtxOld, CtxNew, Children, Obj, NewObj
    ).

% ==========================================================
% execute_parallel_all_running/5
% ==========================================================
execute_parallel_all_running(
    CtxOld, CtxNew, Children, Obj, NewObj
) :-
    execute_action:execute_action(
        ctx_old(CtxOld),
        ctx_new(CtxNew),
        action(parallel_all(Children)),
        obj_old(Obj),
        obj_new(NewObj)
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
    update_obj_attrs(ObjIn, Res, NextObj),
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
        obj_new(Res)
    ).

% update_obj_attrs(+Obj, +ResultList, -NewObj)
% Updates object based on child result.
%   - If result is an object, use it
%   - If result is [] (despawned), keep old obj
%   Attributes are in centralized store, so no need to copy
%   them.

update_obj_attrs(
    Obj, [object(_, _, _, _)], Obj
).
update_obj_attrs(Obj, [], Obj).

% filter_running_actions(+Results, -Actions)
% Recursively filters child results to extract actions
%   from children that are still running.
% Finished children (actions([])) and despawned
%   children ([]) are skipped.

filter_running_actions([], []).
filter_running_actions([Res|Rs], Acc) :-
    ( Res = [object(_, _, actions([A|_]), _)] ->
        % Child still running: include its action
        Acc = [A|RestAcc],
        filter_running_actions(Rs, RestAcc)
    ;
        % Child done or despawned: skip it
        filter_running_actions(Rs, Acc)
    ).

