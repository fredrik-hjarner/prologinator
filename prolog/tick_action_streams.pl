% ==========================================================
% Execution Model: tick_action_streams
% ==========================================================

% Area of responsibility:
% - recurse over all streams and call tick_object for each.
% - resposible for removing despawned objects from acnnstore
% tick_action_streams threads the context via DCG.
% Takes an object ID and processes all action streams for
% that object.
% Returns Status: despawned, not_despawned
tick_action_streams(ObjID, Status) -->
    ctx_actionstore(AcnStoreIn),
    ( {gen_assoc(ObjID, AcnStoreIn, AcnStreamsOld)} ->
        % Process streams and get updated list
        tick_action_streams_loop(
            obj_id(ObjID),
            left(AcnStreamsOld),
            accum_old([]),
            accum_new(AcnStreamsNew),
            result(Status)
        ),
        ({Status = despawned} ->
            (
                % TODO: Implement a dbg_write/dbg_format
                %       togglable via env.
                % {format(
                %   "INFO: Removing obj ~w from acnstore~n",
                %   [ObjID]
                % )},
                % remove object from actionstore
                {del_assoc(
                    ObjID, AcnStoreIn, _, AcnStoreOut
                )},
                ctx_set_actionstore(AcnStoreOut)
            )
        ;
            % Update actionstore
            {put_assoc(
                ObjID,
                AcnStoreIn,
                AcnStreamsNew,
                AcnStoreOut
            )},
            ctx_set_actionstore(AcnStoreOut)
        )
    ;
        % the object is missing action streams for it!
        % throw error
        {format(
            "ERROR: Object ~w not found in actionstore~n",
            [ObjID]
        )},
        {halt(1)}
    ).

% base case: no streams left to process
% AccumRev is in reverse order, reverse it here to get it
% into non-reverse order.
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
    % We store what we have left to process in Left.
    left([StreamToProcess|StreamToProcessRest]),
    % We build the new Action Streams List part by part and
    % store in Accum
    accum_old(AccumOld),
    accum_new(AccumNew),
    result(Result)
) -->
    tick_object(
        actions_old(StreamToProcess),
        obj_id(ObjId),
        result(TickStatus, actions_new(StreamAfterTick))
    ),
    % So at this point we run one stream until a stop state
    %     and some might have added fork_cmd:s (added in
    %     reverse with cons).
    % Check for fork commands and append to left (once,
    % before branching)
    collect_and_append_forks(
        StreamToProcessRest,
        StreamToProcessRestWithForkAcns
    ),
    ({TickStatus = despawned} -> 
        ({AccumNew = []}, {Result = despawned})
    ;
        ({TickStatus = completed} ->
            % recurse
            tick_action_streams_loop(
                obj_id(ObjId),
                % head processed. process rest (with forks).
                left(StreamToProcessRestWithForkAcns),
                % completed, so don't need to add any
                % resulting actions because there were none.
                accum_old(AccumOld),
                accum_new(AccumNew),
                result(Result)
            )
        ; % yielded
            (
                % add what was left after yield.
                % AccumOld is in reverse order, prepend to
                % build backwards (O(1))
                {Accum = [StreamAfterTick | AccumOld]},
                % recurse
                tick_action_streams_loop(
                    obj_id(ObjId),
                    % head processed. process rest (with
                    % forks).
                    left(StreamToProcessRestWithForkAcns),
                    accum_old(Accum),
                    accum_new(AccumNew),
                    result(Result)
                )
            )
        )
    ).

% ==========================================================
% Fork Command Processing
% ==========================================================

% Collect fork commands and append their actions to the left
% list. Removes processed commands from context.
collect_and_append_forks(
    StreamToProcessRest, % input
    StreamToProcessRestWithForkAcns % output
) -->
    ctx_forkCmds(ForkCmds),
    ( {ForkCmds = []} ->
        % No fork commands, nothing to append
        {StreamToProcessRestWithForkAcns
          = StreamToProcessRest}
    ;
        % Extract actions from all fork commands (all are
        % for this object)
        {extract_actions_from_fork_cmds(
            ForkCmds, NewStreams
        )},
        % Append new streams to left
        {append(
            StreamToProcessRest, % this was/is the input
            NewStreams, % new streams. from the fork_cmd:s.
            StreamToProcessRestWithForkAcns % output.
        )},
        % Clear fork commands (all processed)
        ctx_set_forkCmds([])
    ).

% Extract actions from fork commands
% Recurses.
% takes the fork_cmd:s (which are stored in revere) and
%     simply returns action streams.
% Simply "cons" them together.
%     So reversed twice i.e. put back in correct order.
% Wrapper
extract_actions_from_fork_cmds(ForkCmds, ActionStreams) :-
    extract_actions_acc(ForkCmds, [], ActionStreams).

% Base case: Assign Accumulator to Result
extract_actions_acc([], Acc, Acc).

% Recursive: Take Head, put it at front of Accumulator
% (Reversing logic)
extract_actions_acc(
    [fork_cmd(obj_id(_), actions(Actions)) | ForkCmdsRest], 
    Acc, 
    Result
) :-
    extract_actions_acc(
        ForkCmdsRest, [Actions|Acc], Result
    ).

