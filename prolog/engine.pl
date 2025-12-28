tick -->
#ifdef ENABLE_VALIDATION
    context_validation,
#endif
#ifdef ENABLE_VALIDATION
    context_validation,
#endif
    tick_all_objects,
#ifdef ENABLE_VALIDATION
    context_validation,
#endif
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

