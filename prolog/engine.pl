% ==========================================================
% Main Tick Function
% ==========================================================
tick -->
#ifdef ENABLE_VALIDATION
    context_validation,
#endif
    % 1. Detect collisions BEFORE ticking
    % (so objects can react)
    detect_collisions,
#ifdef ENABLE_VALIDATION
    context_validation,
#endif
    % 2. Tick Physics & Logic
    % Iterates by ID, allowing new spawns to be picked up
    % immediately.
    tick_all_objects,
#ifdef ENABLE_VALIDATION
    context_validation,
#endif
    % 3. Increment Frame
    increment_frame.

% ==========================================================
% Pipeline Stages
% ==========================================================

increment_frame -->
    ctx_frame(F),
    {F1 #= F + 1},
    ctx_set_frame(F1).

% ==========================================================
% Tick Logic (The ID Cursor)
% ==========================================================

% The responsibilities for tick_all_objects is to iterate
% over all objects and execute tick_action_streams for each.
% It is also responsible for cleaning up objects in the
% objects list and cleaning up actions in actionstore.
% It is also responsible for reading spawn commands and
% spawning new objects.
tick_all_objects -->
    ctx_objs(ObjsQueue),
    % At first we have all objects left to process.
    tick_objects_loop(ObjsQueue).

% base case: no objects left to process
tick_objects_loop([]) --> !.

% The loop finds the next object with ID > LastID
tick_objects_loop([TargetObj|ObjsQueue]) -->
    % Found an object to tick
    % Process actions from actionstore
    tick_action_streams(obj(TargetObj), Status),
    % Update the context with the result of this
    % specific object based on status
    ( {Status = despawned} ->
        % Extract ID for removal
        {obj_id(TargetObj, TargetID)},
        % Remove object from context and actionstore
        % (cleaned by tick_action_streams)
        update_object_in_context(TargetID, [])
    ;
        % Keep object (yielded or completed)
        % No need to reconstruct - actions are in
        % actionstore
        []
    ),
    % So at this point we have processed all action streams
    %     of one specific object... and in the process
    %     the object might have spawned new objects...
    %     So we need to add them!
    % Process spawn commands ASAP so spawned objects
    % can execute in the same frame
    % So assuming `process_spawn_commands` works in a
    %     reasonable way it should simply add the new
    %     objects to the end.
    process_spawn_commands(ObjsQueue, ObjsQueueNew),
    % Recurse using the current TargetID as the new
    % floor
    tick_objects_loop(ObjsQueueNew).

% Helper: Replaces the object with TargetID with the
% NewList (which is [Obj] or [])
update_object_in_context(TargetID, NewList) -->
    ctx_objs(Objects),
    {replace_by_id(Objects, TargetID, NewList, NewObjects)},
    ctx_set_objs(NewObjects).

% replace_by_id(+CurrentList, +TargetID, +ReplacementList,
% -NewList)
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
        % MATCH FOUND: Determine operation based on
        % Replacement list
        ( Replacement = [] ->
            % CASE 1: Deletion
            % The result is simply the rest of the list.
            Result = Rest
        ;
            % CASE 2: Update (Optimization: Assume 1 item)
            % The result is the new object attached to the
            % rest.
            Replacement = [NewObj],
            Result = [NewObj|Rest]
        )
    ;
        % NO MATCH: Keep searching
        Result = [Obj|NewRest],
        replace_by_id(Rest, TargetID, Replacement, NewRest)
    ).

% ==========================================================
% Spawn Command Processing
% ==========================================================

% `process_spawn_commands` takes the ObjsQueue as input
%     which is the queue (list) of objects left to process
%     and it look through the spawn_cmd:s and add new
%     spawned object to the end of the end the the queue so
%     (TODO: Well actually it adds them to the front!)
%     that the game loop will process them (this frame).
% `process_spawn_commands` also adds the objects to the ctx.
% `process_spawn_commands` also adds to the actionstore.
process_spawn_commands(ObjsQueue, ObjsQueueNew) -->
    % At this point SpawnCmds are in reverse order!
    ctx_spawnCmds(SpawnCmds), 
    % Process all spawn commands
    process_spawn_commands_loop(
        spawn_cmds(SpawnCmds),
        ObjsQueue,
        ObjsQueueNew,
        [],           % SpawnedObjs starts empty
        SpawnedObjs   % collected spawns in correct order
    ),
    % add the spawned objects to the context
    ctx_objs(ObjsOld),
    {append(ObjsOld, SpawnedObjs, ObjsNew)},
    ctx_set_objs(ObjsNew),
    % Clear spawn commands after processing
    ctx_set_spawnCmds([]).

% Process each spawn command
% base case: no spawn commands left to process
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
    % Generate new ID
    ctx_nextid(NewID),
    {NextID #= NewID + 1},
    ctx_set_nextid(NextID),
    % Create object
    {NewObj = object(id(NewID))},

    % Add object to the left to process queue
    % so yea we both add it to the the left to process queue
    % and to the context.
    % TODO: prepend because it's faster but this causes
    % newly spawned objects to be processed first, which
    % might not be ideal since the user might rely on stuff
    % happening in a strict order.
    {ObjsQueueAfterPrepend = [NewObj | ObjsQueue]},

    % "collect" the object. we will add it to context but
    % we don't do it one per one becuase that's slow, we
    % collect them and then add them in bulk.
    % Since the spawn_cmd:s were added in reverse originally
    %     prepending here causes a double reverse i.e. order
    %     of objects in `SpawnedObjsAfterPrepend` we be
    %     in correct/normal order.
    {SpawnedObjsAfterPrepend = [NewObj | SpawnedObjsOld]},

    % Add actions to actionstore (wrap in list for single
    % stream)
    % tick_objects_loop will automatically pick up and
    % execute these actions
    ctx_actionstore(ActionStore),
    {put_assoc(
        NewID,
        ActionStore,
        [SpawnObjActions],
        NewActionStore
    )},
    ctx_set_actionstore(NewActionStore),
    % Process remaining commands
    process_spawn_commands_loop(
        spawn_cmds(CmdsRest),
        ObjsQueueAfterPrepend,
        ObjsQueueNew,
        SpawnedObjsAfterPrepend,
        SpawnedObjsNew
    ).

