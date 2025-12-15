% ==========================================================
% Main Tick Function
% ==========================================================
tick(CtxIn, CtxOut) :-
    context_validation(CtxIn),
    
    % 1. Detect collisions BEFORE ticking
    % (so objects can react)
    detect_collisions(CtxIn, CtxColl),
    context_validation(CtxColl),
    
    % 2. Tick Physics & Logic
    % Iterates by ID, allowing new spawns to be picked up
    % immediately.
    tick_all_objects(CtxColl, CtxPhys),
    context_validation(CtxPhys),
    
    % 3. Increment Frame
    increment_frame(CtxPhys, CtxOut).

% ==========================================================
% Pipeline Stages
% ==========================================================

increment_frame(CtxIn, CtxOut) :-
    ctx_frame(F, CtxIn),
    F1 #= F + 1,
    ctx_set_frame(F1, CtxIn, CtxOut).

% ==========================================================
% Tick Logic (The ID Cursor)
% ==========================================================

tick_all_objects(CtxIn, CtxOut) :-
    % Start the loop with ID -1 (assuming IDs start at 0
    % or 1)
    tick_objects_loop(-1, CtxIn, CtxOut).

% The loop finds the next object with ID > LastID
tick_objects_loop(LastID, CtxIn, CtxOut) :-
    ( find_next_object(LastID, TargetObj, CtxIn, _) ->
        % Found an object to tick
        obj_id(TargetObj, TargetID),
        
        tick_object(
            obj_old(TargetObj),
            result(Status, ObjResult),
            CtxIn,
            CtxTemp % Contains spawns/side-effects
        ),
        
        % Update the context with the result of this
        % specific object based on status
        ( Status = despawned ->
            % Remove object from context
            update_object_in_context(
                TargetID, [], CtxTemp, CtxNext
            )
        ;
            % Keep object (yielded or completed)
            update_object_in_context(
                TargetID, [ObjResult], CtxTemp, CtxNext
            )
        ),
        
        % Recurse using the current TargetID as the new
        % floor
        tick_objects_loop(TargetID, CtxNext, CtxOut)
    ;
        % No more objects with ID > LastID
        CtxOut = CtxIn
    ).

% Helper: Finds the first object in context with ID > MinID
% Since the list is sorted, this is effectively finding
% the next one.
find_next_object(MinID, Obj, Ctx, Ctx) :-
    ctx_objs(Objects, Ctx),
    member(Obj, Objects),
    obj_id(Obj, ID),
    ID > MinID,
    !. % Take the first one we find (lowest ID > MinID)

% Helper: Replaces the object with TargetID with the
% NewList (which is [Obj] or [])
update_object_in_context(
    TargetID, NewList, CtxIn, CtxOut
) :-
    ctx_objs(Objects, CtxIn),
    replace_by_id(Objects, TargetID, NewList, NewObjects),
    ctx_set_objs(NewObjects, CtxIn, CtxOut).

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

