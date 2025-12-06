% Parallel Action Execution Module
% Handles parallel and parallel_running actions

:- module(parallel, []).

:- use_module(library(lists), [
    append/2,
    append/3,
    member/2,
    findall/3
]).
:- use_module('../types/accessors', [object_attrs/2]).
% Ensure execute_action is loaded so we can call it
% (via module qualification to avoid circular dependency)
:- use_module('../execute_action', [execute_action/6]).

% ==========================================================
% execute_action_impl clauses for parallel actions
% ==========================================================

execute_action:execute_action_impl(
    ctx_in(Ctx),
    parallel(ChildActions),
    ObjIn,
    MaybeObjectOut,
    AllCommands, % output
    AllRevHints % output
) :-
    ObjIn = object(
        id(ID), type(Type), _,
        actions([_|Rest]), collisions(Colls)
    ),
    tick_parallel_children(
        ctx_in(Ctx),
        ChildActions, % list of actions that run in parallel
        ObjIn,
        ObjFinal,
        ChildResults,
        AllCommands,
        AllRevHints
    ),
    object_attrs(ObjFinal, AttrsOut),
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
            MaybeObjectOut = [
                object(
                id(ID),
                type(Type),
                attrs(AttrsOut),
                actions(Rest),
                collisions(Colls)
            )]
        ;
            % Some children are still running.
            MaybeObjectOut = [object(
                id(ID),
                type(Type),
                attrs(AttrsOut),
                actions([
                    parallel_running(RemainingActions)|Rest
                ]),
                collisions(Colls)
            )]
        )
    ).

execute_action:execute_action_impl(
    ctx_in(Ctx),
    parallel_running(Children),
    Obj, NewObj, Commands, RevHints
) :-
    execute_action:execute_action(
        ctx_in(Ctx),
        parallel(Children),
        Obj,
        NewObj,
        Commands,
        RevHints
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
    ctx_in(_Ctx), [], ObjIn, ObjIn, [], [], []
).

tick_parallel_children(
    ctx_in(Ctx),
    [Child|RestChildren],
    ObjIn,
    ObjFinal,
    [ChildResult|RestResults],
    AllCommands,
    AllRevHints
) :-
    ObjIn = object(
        id(ID), type(Type), attrs(AttrsIn),
        actions(_), collisions(Colls)
    ),

    % 1. Construct input object for this child action
    ChildObjIn = object(
        id(ID), type(Type), attrs(AttrsIn),
        actions([Child]), collisions(Colls)
    ),

    % 2. Execute the action
    execute_action:execute_action(
        ctx_in(Ctx), Child, ChildObjIn, ResultList, C1, R1
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
        id(ID), type(Type), attrs(AttrsNext),
        actions(_), collisions(Colls)
    ),

    % 5. Recurse
    tick_parallel_children(
        ctx_in(Ctx),
        RestChildren,
        NextObjIn,
        ObjFinal,
        RestResults,
        C2,
        R2
    ),
    
    % 6. Build output
    ChildResult = ResultList,
    append(C1, C2, AllCommands),
    append(R1, R2, AllRevHints).

% ==========================================================
% Tests: parallel
% ==========================================================

test("parallel: backward test - can infer original \
wait_frames values from parallel_running state", (
    % N1 and N2 are unknown - should be inferred
    Action = parallel([wait_frames(N1), wait_frames(N2)]),
    ObjIn = object(
        id(1),
        type(static),
        attrs([]),
        actions([
            parallel([wait_frames(N1), wait_frames(N2)])
        ]),
        collisions([])
    ),
    ObjOut = [object(
        id(1),
        type(static),
        attrs([]),
        actions([
            parallel_running([
                wait_frames(2),
                wait_frames(3)
            ])
        ]),
        collisions([])
    )],
    Commands = [],
    RevHints = [],
    Ctx = ctx(state(
        frame(0),
        [],
        status(playing),
        score(0),
        next_id(1),
        [],
        []
    )),
    execute_action:execute_action(
        ctx_in(Ctx),
        Action,
        ObjIn,
        ObjOut,
        Commands,
        RevHints
    ),
    N1 = 3,  % Verify that N1 was correctly inferred
    N2 = 4   % Verify that N2 was correctly inferred
)).

