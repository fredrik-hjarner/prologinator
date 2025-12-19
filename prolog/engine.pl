% ==========================================================
% Main Tick Function
% ==========================================================
tick -->
    context_validation,
    % 1. Detect collisions BEFORE ticking
    % (so objects can react)
    detect_collisions,
    context_validation,
    % 2. Tick Physics & Logic
    % Iterates by ID, allowing new spawns to be picked up
    % immediately.
    tick_all_objects,
    context_validation,
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
tick_all_objects -->
    % Start the loop with ID -1 (assuming IDs start at 0
    tick_objects_loop(-1).

% The loop finds the next object with ID > LastID
tick_objects_loop(LastID) -->
    ( find_next_object(LastID, TargetObj) ->
        % Found an object to tick
        {obj_id(TargetObj, TargetID)},
        % Process actions from actionstore
        tick_action_streams(TargetID, Status),
        % Update the context with the result of this
        % specific object based on status
        ( {Status = despawned} ->
            % Remove object from context and actionstore
            % (cleaned by tick_action_streams)
            update_object_in_context(TargetID, [])
        ;
            % Keep object (yielded or completed)
            % No need to reconstruct - actions are in
            % actionstore
            []
        ),
        % Process spawn commands ASAP so spawned objects
        % can execute in the same frame
        process_spawn_commands,
        % Recurse using the current TargetID as the new
        % floor
        tick_objects_loop(TargetID)
    ;
        []
    ).

% Helper: Finds the first object in context with ID > MinID
% Since the list is sorted, this is effectively finding
% the next one.
find_next_object(MinID, Obj) -->
    ctx_objs(Objects),
    {member(Obj, Objects),
     obj_id(Obj, ID),
     ID > MinID,
     !}. % Take the first one we find (lowest ID > MinID)

% Helper: Replaces the object with TargetID with the
% NewList (which is [Obj] or [])
update_object_in_context(TargetID, NewList) -->
    ctx_objs(Objects),
    {replace_by_id(Objects, TargetID, NewList, NewObjects)},
    ctx_set_objs(NewObjects).

% replace_by_id(+CurrentList, +TargetID, +ReplacementList,
% -NewList)
replace_by_id([], _, _, []).
replace_by_id([Obj|Rest], TargetID, Replacement, Result) :-
    obj_id(Obj, ID),
    ( ID = TargetID ->
        append(Replacement, Rest, Result)
    ;
        Result = [Obj|NewRest],
        replace_by_id(Rest, TargetID, Replacement, NewRest)
    ).

% ==========================================================
% Spawn Command Processing
% ==========================================================

% Process all spawn commands, creating objects and adding
% them to actionstore. Processes commands ASAP so spawned
% objects can execute in the same frame.
process_spawn_commands -->
    ctx_spawnCmds(SpawnCmds),
    ( {SpawnCmds = []} ->
        % No spawn commands, nothing to do
        []
    ;
        % Process all spawn commands
        process_spawn_commands_loop(SpawnCmds),
        % Clear spawn commands after processing
        ctx_set_spawnCmds([])
    ).

% Process each spawn command
process_spawn_commands_loop([]) --> [].
process_spawn_commands_loop(
    [spawn_cmd(actions(Actions))|Rest]
) -->
    % Generate new ID
    ctx_nextid(NewID),
    {NextID #= NewID + 1},
    ctx_set_nextid(NextID),
    % Create object
    {NewObj = object(id(NewID))},
    % Add object to context
    ctx_objs(ObjsOld),
    {append(ObjsOld, [NewObj], ObjsNew)},
    ctx_set_objs(ObjsNew),
    % Add actions to actionstore (wrap in list for single
    % stream)
    % tick_objects_loop will automatically pick up and
    % execute these actions
    ctx_actionstore(ActionStore),
    {put_assoc(
        NewID,
        ActionStore,
        [Actions],
        NewActionStore
    )},
    ctx_set_actionstore(NewActionStore),
    % Process remaining commands
    process_spawn_commands_loop(Rest).

