% ==========================================================
% THE PROLOGINATOR MONOLITH BUILD
% ==========================================================


% #define ENABLE_VALIDATION

% 1. Global Imports (Import all libraries ONCE)

:- use_module(library(assoc)).
:- use_module(library(charsio)).
:- use_module(library(clpz)).
:- use_module(library(dif)).
:- use_module(library(format)).
:- use_module(library(iso_ext)).
:- use_module(library(lists)).
:- use_module(library(os)).
:- use_module(library(time)).


% 1.5. Discontiguous Declarations (must be before any clauses)

:- discontiguous(builtin_action/1).

:- discontiguous(check_condition_impl/4).

:- dynamic(execute_action_impl/5).
:- discontiguous(execute_action_impl/5).



% 1.6. Dynamic Declarations (must be before any clauses)

:- dynamic(user_action/2).



% 1.7. Operators (must be before any clauses)

:- op(101, fy, '.').
:- op(100, yfx, '.').


% 2. Types (foundational - no dependencies on game logic)







ctx_frame(F, Ctx, Ctx) :-
    Ctx = ctx(state(frame(F), _, _, _, _, _, _), _).

ctx_objs(O, Ctx, Ctx) :-
    Ctx = ctx(state(_, objects(O), _, _, _, _, _), _).

ctx_attrs(A, Ctx, Ctx) :-
    Ctx = ctx(state(_, _, attrs(A), _, _, _, _), _).

ctx_cmds(commands(SC, FC), Ctx, Ctx) :-
    Ctx = ctx(state(_, _, _, _, _,
                    commands(SC, FC), _), _).

ctx_spawnCmds(SC, Ctx, Ctx) :-
    Ctx = ctx(state(_, _, _, _, _,
                    commands(spawn_cmds(SC),
                             fork_cmds(_)), _), _).

ctx_forkCmds(FC, Ctx, Ctx) :-
    Ctx = ctx(state(_, _, _, _, _,
                    commands(spawn_cmds(_),
                             fork_cmds(FC)), _), _).

ctx_status(S, Ctx, Ctx) :-
    Ctx = ctx(state(_, _, _, status(S), _, _, _), _).

ctx_nextid(N, Ctx, Ctx) :-
    Ctx = ctx(state(_, _, _, _, next_id(N), _, _), _).

ctx_actionstore(AS, Ctx, Ctx) :-
    Ctx = ctx(state(_, _, _, _, _, _,
                    actionstore(AS)), _).

ctx_input(I, Ctx, Ctx) :-
    Ctx = ctx(_, I).

ctx_events(E, Ctx, Ctx) :-
    Ctx = ctx(_, input(events(E), _)).

ctx_held(H, Ctx, Ctx) :-
    Ctx = ctx(_, input(_, held(H))).


ctx_objs_cmds(Objs, Cmds) -->
    ctx_objs(Objs),
    ctx_cmds(Cmds).

ctx_objs_attrs(Objs, Attrs) -->
    ctx_objs(Objs),
    ctx_attrs(Attrs).

ctx_status_cmds(Status, Cmds) -->
    ctx_status(Status),
    ctx_cmds(Cmds).

ctx_objs_nextid_cmds(Objs, NextID, Cmds) -->
    ctx_objs(Objs),
    ctx_nextid(NextID),
    ctx_cmds(Cmds).



ctx_set_frame(F, ctx(state(_, O, A, S, N, C, AS), I),
              ctx(state(frame(F), O, A, S, N, C, AS), I)).

ctx_set_objs(O, ctx(state(F, _, A, S, N, C, AS), I),
             ctx(state(F, objects(O), A, S, N, C, AS), I)).

ctx_set_attrs(A, ctx(state(F, O, _, S, N, C, AS), I),
              ctx(state(F, O, attrs(A), S, N, C, AS), I)).

ctx_set_cmds(commands(SC, FC),
             ctx(state(F, O, A, S, N, _, AS), I),
             ctx(state(F, O, A, S, N,
                       commands(SC, FC), AS), I)).

ctx_set_spawnCmds(SC,
                  ctx(state(F, O, A, S, N,
                            commands(spawn_cmds(_),
                                     fork_cmds(FC)), AS),
                      I),
                  ctx(state(F, O, A, S, N,
                            commands(spawn_cmds(SC),
                                     fork_cmds(FC)), AS),
                      I)).

ctx_set_forkCmds(FC,
                 ctx(state(F, O, A, S, N,
                           commands(spawn_cmds(SC),
                                    fork_cmds(_)), AS),
                     I),
                 ctx(state(F, O, A, S, N,
                           commands(spawn_cmds(SC),
                                    fork_cmds(FC)), AS),
                     I)).

ctx_set_status(S, ctx(state(F, O, A, _, N, C, AS), I),
               ctx(state(F, O, A, status(S), N, C, AS),
                   I)).

ctx_set_nextid(N, ctx(state(F, O, A, S, _, C, AS), I),
               ctx(state(F, O, A, S, next_id(N), C, AS),
                   I)).

ctx_set_actionstore(AS, ctx(state(F, O, A, S, N, C, _),
                            I),
                    ctx(state(F, O, A, S, N, C,
                              actionstore(AS)), I)).

ctx_set_input(I, ctx(S, _), ctx(S, I)).


ctx_set_objs_cmds(Objs, Cmds) -->
    ctx_set_objs(Objs),
    ctx_set_cmds(Cmds).

ctx_set_objs_attrs(Objs, Attrs) -->
    ctx_set_objs(Objs),
    ctx_set_attrs(Attrs).

ctx_set_status_cmds(Status, Cmds) -->
    ctx_set_status(Status),
    ctx_set_cmds(Cmds).

ctx_set_nextid_cmds(NextID, Cmds) -->
    ctx_set_nextid(NextID),
    ctx_set_cmds(Cmds).

ctx_set_objs_nextid_cmds(Objs, NextID, Cmds) -->
    ctx_set_objs(Objs),
    ctx_set_nextid(NextID),
    ctx_set_cmds(Cmds).




obj_id(object(id(ID)), ID).




empty_ctx(ctx(state(
    frame(0),
    objects([]),
    attrs(EmptyAttrs),
    status(playing),
    next_id(1),
    commands(spawn_cmds([]), fork_cmds([])),
    actionstore(EmptyActionStore)
), input(events([]), held([])))) :-
    empty_assoc(EmptyAttrs),
    empty_assoc(EmptyActionStore).

ctx_with_attrs(Attrs, Ctx) :-
    empty_ctx(Def),
    ctx_set_attrs(Attrs, Def, Ctx).

ctx_with_frame_attrs(Frame, Attrs, Ctx) :-
    empty_ctx(Def),
    ctx_set_frame(Frame, Def, Ctx1),
    ctx_set_attrs(Attrs, Ctx1, Ctx).

ctx_with_inputevents_inputheld(Events, Held, Ctx) :-
    empty_ctx(Def),
    ctx_set_input(input(events(Events), held(Held)), Def,
                  Ctx).

ctx_with_objs(Objects, Ctx) :-
    empty_ctx(Def),
    ctx_set_objs(Objects, Def, Ctx).

ctx_with_objs_input(Objects, Events, Held, Ctx) :-
    empty_ctx(Def),
    ctx_set_objs(Objects, Def, Ctx1),
    ctx_set_input(input(events(Events), held(Held)), Ctx1,
                  Ctx).

ctx_with_frame_objs_input(Frame, Objects, Events, Held,
                          Ctx) :-
    empty_ctx(Def),
    ctx_set_frame(Frame, Def, Ctx1),
    ctx_set_objs(Objects, Ctx1, Ctx2),
    ctx_set_input(input(events(Events), held(Held)), Ctx2,
                  Ctx).


empty_attr_store(EmptyAttrs) :-
    empty_assoc(EmptyAttrs).



ctx_attr_val(ObjectID/Key, Value) -->
    ctx_attrs(AttrStore),
    {
        gen_assoc(ObjectID, AttrStore, Attrs),
        member(attr(Key, Value), Attrs)
    }.

ctx_attr_val(ObjectID/Key, Value, Ctx) :-
    ctx_attr_val(ObjectID/Key, Value, Ctx, Ctx).

ctx_set_attr_val(ObjectID/Key, Value) -->
    ctx_attrs(AttrStoreIn),
    {
        set_attr_in_store_helper(
            AttrStoreIn, ObjectID, Key, Value, AttrStoreOut
        )
    },
    ctx_set_attrs(AttrStoreOut).

set_attr_in_store_helper(AttrStoreIn, ObjectID, Key,
                         Value, AttrStoreOut) :-
    ( gen_assoc(ObjectID, AttrStoreIn, OldAttrs) ->
        ( select(attr(Key, _), OldAttrs, Rest) ->
            true
        ;
            Rest = OldAttrs
        ),
        NewAttrs = [attr(Key, Value)|Rest],
        put_assoc(ObjectID, AttrStoreIn, NewAttrs,
                  AttrStoreOut)
    ;
        NewAttrs = [attr(Key, Value)],
        put_assoc(ObjectID, AttrStoreIn, NewAttrs,
                  AttrStoreOut)
    ).


obj_type(Obj, Type) -->
    {obj_id(Obj, ID)},
    ctx_attr_val(ID/type, Type).

obj_id_type(Obj, ID, Type) -->
    {obj_id(Obj, ID)},
    ctx_attr_val(ID/type, Type).


% 3. Utilities and Macros

catch_dcg(Goal, Catcher, Handler, S0, S) :-
    catch(
        call(Goal, S0, S),
        Catcher,
        Handler
    ).


partition(_Pred, [], [], []).
partition(Pred, [X|Xs], Yes, No) :-
    ( call(Pred, X) ->
        Yes = [X|YesRest],
        partition(Pred, Xs, YesRest, No)
    ;
        No = [X|NoRest],
        partition(Pred, Xs, Yes, NoRest)
    ).


select_many([], List, List).
select_many([Pattern|Patterns], List, Remaining) :-
    select(Pattern, List, ListWithoutPattern),
    select_many(Patterns, ListWithoutPattern, Remaining).


is_list([]).
is_list([_|T]) :- is_list(T).


% 4. Action Resolution and Builtins
resolve_action(
    _MyID, load(Path), load(Path)
) -->
    !,
    [].

resolve_action(
    MyID,
    wait(Frames),
    wait(Frames)
) -->
    !,
    [].

resolve_action(
    _MyID,
    loop(Actions),
    loop(Actions)
) -->
    !,
    [].

resolve_action(
    _MyID,
    loop(Running, Original),
    loop(Running, Original)
) -->
    !,
    [].

resolve_action(
    _MyID,
    log(Message),
    log(Message)
) -->
    !,
    [].


% 5. Execute Action





execute_action(
    actions_old([Action|Rest]),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    resolve_action(ID, Action, ResolvedAction),
    execute_action_resolved(
        actions_old([ResolvedAction|Rest]),
        obj_id(ID),
        result(Status, actions_new(ActionsOut))
    ).

execute_action_resolved(
    actions_old([Action|Rest]),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->

    ( {builtin_action(Action)} ->

        {functor(Action, Functor, _)},
        {format("executing `~w`~n", [Functor])},

        catch_dcg(
            execute_action_impl(
                actions_old([Action|Rest]),
                obj_id(ID),
                result(Status, actions_new(ActionsOut))
            ),
            Error,
            ( write('Error during execute_action_impl: '),
              write(Error), nl,
              throw(Error)
            )
        )
    ; {user_action(Action, Body)} -> % total prolog voodoo!
        execute_action(
            actions_old([Body|Rest]),
            obj_id(ID),
            result(Status, actions_new(ActionsOut))
        )
    ;
        {throw(unknown_action(Action))}
    ).




% 6. Action Implementations (all actions)
builtin_action(wait(_)).

execute_action_impl(
    actions_old([wait(N)|Rest]),
    obj_id(_ID),
    result(Status, actions_new(ActionsOut))
) -->
    execute_wait(N, Rest, Status, ActionsOut).

execute_wait(N, Rest, Status, ActionsOut) -->
    {
        ( N = 0 ->
            ActionsOut = Rest,
            Status = completed
        ; N = 1 ->
            ActionsOut = Rest,
            Status = yielded
        ; N #> 1 ->
            N1 #= N - 1,
            ActionsOut = [wait(N1)|Rest],
            Status = yielded
        )
    }.

builtin_action(log(_)).

execute_action_impl(
    actions_old([log(Msg)|Rest]),
    obj_id(_ID),
    result(completed, actions_new(Rest))
) -->
    execute_log(Msg).

execute_log(Msg) -->
    {format("~s~n", [Msg])}.



builtin_action(loop(_)).
builtin_action(loop(_, _)). % loop continuation


execute_action_impl(
    actions_old([loop(Actions)|Rest]),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    execute_loop_managed(
        Actions, Actions, Rest, ID, Status, ActionsOut
    ).

execute_action_impl(
    actions_old([loop(Running, Original)|Rest]),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    execute_loop_managed(
        Running, Original, Rest, ID, Status, ActionsOut
    ).

execute_loop_managed(
    Running, Original, Rest, ID, Status, ActionsOut
) -->
    tick_object(
        actions_old(Running),
        obj_id(ID),
        result(RunStatus, actions_new(RunRemaining))
    ),
    handle_loop_result(
        RunStatus,
        RunRemaining,
        Original,
        Rest,
        ID,
        Status,
        ActionsOut
    ).

handle_loop_result(
    despawned, _, _, _, _, despawned, []
) --> !, [].

handle_loop_result(
    yielded,
    RunRemaining,
    Original,
    Rest,
    _,
    yielded,
    [loop(RunRemaining, Original)|Rest]
) --> !, [].

handle_loop_result(
    completed, _, Original, Rest, ID, Status, ActionsOut
) -->
    execute_loop_managed(
        Original, Original, Rest, ID, Status, ActionsOut
    ).

builtin_action(load(_)).


execute_action_impl(
    actions_old([load(Path)|Rest]),
    obj_id(_ID),
    result(completed, actions_new(NewActions))
) -->
    execute_load(Path, Rest, NewActions).

execute_load(Path, Rest, NewActions) -->
    {
        atom_chars(PathAtom, Path),
        setup_call_cleanup(
            open(PathAtom, read, Stream),
            read_term(Stream, Actions, []),
            close(Stream)
        ),
        ( is_list(Actions) ->
            append(Actions, Rest, NewActions)
        ;
            throw(error(
                type_error(list, Actions),
                load/1
            ))
        )
    }.




% 7. Core Engine Components

tick_action_streams(ObjID, Status) -->
    ctx_actionstore(AcnStoreIn),
    ( {gen_assoc(ObjID, AcnStoreIn, AcnStreamsOld)} ->
        tick_action_streams_loop(
            obj_id(ObjID),
            left(AcnStreamsOld),
            accum_old([]),
            accum_new(AcnStreamsNew),
            result(Status)
        ),
        ({Status = despawned} ->
            (
                {del_assoc(
                    ObjID, AcnStoreIn, _, AcnStoreOut
                )},
                ctx_set_actionstore(AcnStoreOut)
            )
        ;
            {put_assoc(
                ObjID,
                AcnStoreIn,
                AcnStreamsNew,
                AcnStoreOut
            )},
            ctx_set_actionstore(AcnStoreOut)
        )
    ;
        {format(
            "ERROR: Object ~w not found in actionstore~n",
            [ObjID]
        )},
        {halt(1)}
    ).

tick_action_streams_loop(
    obj_id(_),
    left([]),
    accum_old(AccumRev),
    accum_new(Accum),
    result(not_despawned)
) -->
    { reverse(AccumRev, Accum) },
    !.

tick_action_streams_loop(
    obj_id(ObjId),
    left([StreamToProcess|StreamToProcessRest]),
    accum_old(AccumOld),
    accum_new(AccumNew),
    result(Result)
) -->
    tick_object(
        actions_old(StreamToProcess),
        obj_id(ObjId),
        result(TickStatus, actions_new(StreamAfterTick))
    ),
    collect_and_append_forks(
        StreamToProcessRest,
        StreamToProcessRestWithForkAcns
    ),
    ({TickStatus = despawned} -> 
        ({AccumNew = []}, {Result = despawned})
    ;
        ({TickStatus = completed} ->
            tick_action_streams_loop(
                obj_id(ObjId),
                left(StreamToProcessRestWithForkAcns),
                accum_old(AccumOld),
                accum_new(AccumNew),
                result(Result)
            )
        ; % yielded
            (
                {Accum = [StreamAfterTick | AccumOld]},
                tick_action_streams_loop(
                    obj_id(ObjId),
                    left(StreamToProcessRestWithForkAcns),
                    accum_old(Accum),
                    accum_new(AccumNew),
                    result(Result)
                )
            )
        )
    ).


collect_and_append_forks(
    StreamToProcessRest, % input
    StreamToProcessRestWithForkAcns % output
) -->
    ctx_forkCmds(ForkCmds),
    ( {ForkCmds = []} ->
        {StreamToProcessRestWithForkAcns
          = StreamToProcessRest}
    ;
        {extract_actions_from_fork_cmds(
            ForkCmds, NewStreams
        )},
        {append(
            StreamToProcessRest, % this was/is the input
            NewStreams, % new streams. from the fork_cmd:s.
            StreamToProcessRestWithForkAcns % output.
        )},
        ctx_set_forkCmds([])
    ).

extract_actions_from_fork_cmds(ForkCmds, ActionStreams) :-
    extract_actions_acc(ForkCmds, [], ActionStreams).

extract_actions_acc([], Acc, Acc).

extract_actions_acc(
    [fork_cmd(obj_id(_), actions(Actions)) | ForkCmdsRest], 
    Acc, 
    Result
) :-
    extract_actions_acc(
        ForkCmdsRest, [Actions|Acc], Result
    ).



tick_object(
    actions_old([]),
    obj_id(_ID),
    result(completed, actions_new([]))
) --> [].

tick_object(
    actions_old([Act|Rest]),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    execute_action(
        actions_old([Act|Rest]),
        obj_id(ID),
        result(ActStatus, actions_new(ActionsTemp))
    ),
    ( {ActStatus = despawned} ->
        {Status = despawned, ActionsOut = []}
    ; {ActStatus = yielded} ->
        {Status = yielded, ActionsOut = ActionsTemp}
    ; % ActStatus = completed
        tick_object(
            actions_old(ActionsTemp),
            obj_id(ID),
            result(Status, actions_new(ActionsOut))
        )
    ).




resolve_path(ObjID, Path, Value) -->
    ( {Path = .(InnerPath)} ->
        {TruePath = InnerPath}
    ;
        {TruePath = Path}
    ),

    (   % Case 1: Compound path using dot functor
        {TruePath = Head.Rest}
    ->
        resolve_path(ObjID, Head, NextID),
        resolve_path(NextID, Rest, Value)

    ;   % Case 2: Simple atom (Basecase: final attribute
        {atom(TruePath)}
    ->
        ctx_attr_val(ObjID/TruePath, Value)

    ;  % Case 3: Numbers, variables, compounds (that aren't
        {Value = TruePath}
    ).



strict_resolve_path(ObjID, .(Path), Value) -->
    !,
    strict_resolve_path(ObjID, Path, Value).

strict_resolve_path(
    ObjID, FirstAttr.RestPath, Value
) -->
    !,
    strict_resolve_path(ObjID, FirstAttr, NextID),
    strict_resolve_path(NextID, RestPath, Value).

strict_resolve_path(ObjID, Path, Value) -->
    {atom(Path)},
    !,
    ctx_attr_val(ObjID/Path, Value).

strict_resolve_path(_ObjID, Path, Path) --> [].



resolve_path_strict(ObjID, Path, Value) -->
    strict_resolve_path(ObjID, Path, Value).

resolve_path_to_attr(MyID, .(Path), Pair) -->
    !,
    resolve_path_to_attr(MyID, Path, Pair).

resolve_path_to_attr(MyID, AttrName, MyID/AttrName) -->
    {atom(AttrName)},  % Base case: simple attribute name
    !.

resolve_path_to_attr(MyID,
                     FirstAttr.RestPath,
                     FinalID/Key) -->
    !,
    resolve_path_strict(MyID, FirstAttr, NextID),
    resolve_path_to_attr(NextID, RestPath, FinalID/Key).

% 8. Main Engine
tick -->


    tick_all_objects,

    increment_frame.


increment_frame -->
    ctx_frame(F),
    {F1 #= F + 1},
    ctx_set_frame(F1).


tick_all_objects -->
    ctx_objs(ObjsQueue),
    tick_objects_loop(ObjsQueue).

tick_objects_loop([]) --> !.

tick_objects_loop([TargetObj|ObjsQueue]) -->
    {obj_id(TargetObj, TargetID)},
    tick_action_streams(TargetID, Status),
    ( {Status = despawned} ->
        update_object_in_context(TargetID, [])
    ;
        []
    ),
    process_spawn_commands(ObjsQueue, ObjsQueueNew),
    tick_objects_loop(ObjsQueueNew).

update_object_in_context(TargetID, NewList) -->
    ctx_objs(Objects),
    {replace_by_id(Objects, TargetID, NewList, NewObjects)},
    ctx_set_objs(NewObjects).

replace_by_id(
    [], % Objects
    _, % TargetID
    _, % Replacement
    [] % NewObjects
).
replace_by_id(
    [Obj|Rest],
    TargetID,
    Replacement,
    Result
) :-
    obj_id(Obj, ID),
    ( ID = TargetID ->
        ( Replacement = [] ->
            Result = Rest
        ;
            Replacement = [NewObj],
            Result = [NewObj|Rest]
        )
    ;
        Result = [Obj|NewRest],
        replace_by_id(Rest, TargetID, Replacement, NewRest)
    ).


process_spawn_commands(ObjsQueue, ObjsQueueNew) -->
    ctx_spawnCmds(SpawnCmds), 
    process_spawn_commands_loop(
        spawn_cmds(SpawnCmds),
        ObjsQueue,
        ObjsQueueNew,
        [],           % SpawnedObjs starts empty
        SpawnedObjs   % collected spawns in correct order
    ),
    ctx_objs(ObjsOld),
    {append(ObjsOld, SpawnedObjs, ObjsNew)},
    ctx_set_objs(ObjsNew),
    ctx_set_spawnCmds([]).

process_spawn_commands_loop(
    spawn_cmds([]),
    ObjsQueue,
    ObjsQueue,
    SpawnedObjs,
    SpawnedObjs
) --> !.
process_spawn_commands_loop(
    spawn_cmds([
        spawn_cmd(actions(SpawnObjActions))|CmdsRest
    ]),
    ObjsQueue,
    ObjsQueueNew,
    SpawnedObjsOld,
    SpawnedObjsNew
) -->
    ctx_nextid(NewID),
    {NextID #= NewID + 1},
    ctx_set_nextid(NextID),
    {NewObj = object(id(NewID))},

    {ObjsQueueAfterPrepend = [NewObj | ObjsQueue]},

    {SpawnedObjsAfterPrepend = [NewObj | SpawnedObjsOld]},

    ctx_actionstore(ActionStore),
    {put_assoc(
        NewID,
        ActionStore,
        [SpawnObjActions],
        NewActionStore
    )},
    ctx_set_actionstore(NewActionStore),
    process_spawn_commands_loop(
        spawn_cmds(CmdsRest),
        ObjsQueueAfterPrepend,
        ObjsQueueNew,
        SpawnedObjsAfterPrepend,
        SpawnedObjsNew
    ).



