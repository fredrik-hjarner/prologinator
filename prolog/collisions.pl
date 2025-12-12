% Collision Detection Module
% Handles collision detection between game objects

% ==========================================================
% Collision Detection (simplified - grid-based)
% ==========================================================
% -list(game_object), -list(hint)).

detect_collisions(ctx_old(CtxIn),ctx_new(CtxOut)) :-
    % Extract objects and attrs from context
    ctx_objs_attrs(CtxIn, Objects, AttrStore),
    
    % Find collisions
    findall(
        collision(ID1, ID2),
        (
            member(Obj1, Objects),
            obj_id(Obj1, ID1),
            member(Obj2, Objects),
            obj_id(Obj2, ID2),
            ID1 @< ID2,
            gen_assoc(ID1, AttrStore, A1),
            gen_assoc(ID2, AttrStore, A2),
            member(attr(x, X1), A1),
            member(attr(y, Y1), A1),
            member(attr(x, X2), A2),
            member(attr(y, Y2), A2),
            collides_at(X1, Y1, X2, Y2)
        ),
        Collisions
    ),
    
    % Handle collisions and get IDs to remove
    handle_collisions(
        Objects, Collisions, NewObjects, ToRemoveIDs
    ),
    
    % Clean up attributes for removed objects
    cleanup_attributes(
        AttrStore, ToRemoveIDs, NewAttrStore
    ),
    
    % Update context with new objects and cleaned attrs
    ctx_objs_attrs_ctx(
        CtxIn,
        NewObjects, NewAttrStore,
        CtxOut
    ).

collides_at(X, Y, X, Y).

% TODO: I dont like that what happens at collision is
% hard-coded like this.
%       should be dynamic.
handle_collisions(
    Objects,
    Collisions,
    NewObjects,
    ToRemoveIDs
) :-
    findall(
        (ProjID, EnemyID),
        (
            member(collision(ProjID, EnemyID), Collisions),
            member(Obj1, Objects),
            obj_id_type(Obj1, ProjID, proj),
            member(Obj2, Objects),
            obj_id_type(Obj2, EnemyID, enemy)
        ;
            member(collision(EnemyID, ProjID), Collisions),
            member(Obj1, Objects),
            obj_id_type(Obj1, EnemyID, enemy),
            member(Obj2, Objects),
            obj_id_type(Obj2, ProjID, proj)
        ),
        ToRemove
    ),
    findall(
        ID,
        (member((A, B), ToRemove), (ID = A ; ID = B)),
        AllIDs
    ),
    list_to_set(AllIDs, ToRemoveIDs),
    remove_objects(Objects, ToRemoveIDs, NewObjects).

% Clean up attributes for removed objects
cleanup_attributes(Attrs, [], Attrs).
cleanup_attributes(Attrs, [ID|Rest], Result) :-
    ( gen_assoc(ID, Attrs, _) ->
        del_assoc(ID, Attrs, _, Temp)
    ;
        Temp = Attrs
    ),
    cleanup_attributes(Temp, Rest, Result).

remove_objects([], _, []).
remove_objects([Obj|Rest], ToRemove, NewObjs) :-
    obj_id(Obj, ID),
    ( member(ID, ToRemove) ->
        remove_objects(Rest, ToRemove, NewObjs)
    ;
        NewObjs = [Obj|RestObjs],
        remove_objects(Rest, ToRemove, RestObjs)
    ).
