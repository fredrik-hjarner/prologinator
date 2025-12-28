
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

