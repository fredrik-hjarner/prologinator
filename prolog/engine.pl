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

tick_all_objects -->
    % Start the loop with ID -1 (assuming IDs start at 0
    % or 1)
    tick_objects_loop(-1).

% The loop finds the next object with ID > LastID
tick_objects_loop(LastID) -->
    ( find_next_object(LastID, TargetObj) ->
        % Found an object to tick
        {obj_id(TargetObj, TargetID)},
        tick_object(
            obj_old(TargetObj),
            result(Status, ObjResult)
        ),
        % Update the context with the result of this
        % specific object based on status
        ( {Status = despawned} ->
            % Remove object from context
            update_object_in_context(TargetID, [])
        ;
            % Keep object (yielded or completed)
            update_object_in_context(TargetID, [ObjResult])
        ),
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

