% Collision Detection Module
% Handles collision detection between game objects
% Sets collision_id attributes on colliding objects

% ==========================================================
% Main entry: runs before tick_all_objects
% ==========================================================
detect_collisions -->
    % 1. Clear old collision_id attributes
    clear_all_collisions,
    % 2. Find all position overlaps
    ctx_objs_attrs(Objects, AttrStore),
    {find_collision_pairs(Objects, AttrStore, Pairs)},
    % Pairs = [(5, 12), (5, 18), (8, 12), ...]
    % 3. Write collision_id for each object
    write_collision_ids(Pairs).

% ----------------------------------------------------------
% Step 1: Clear collision_id attributes
% ----------------------------------------------------------
clear_all_collisions -->
    ctx_attrs(StoreIn),
    ctx_objs(Objects),
    {foldl(clear_one_collision, 
          Objects, 
          StoreIn, 
          StoreOut)},
    ctx_set_attrs(StoreOut).

clear_one_collision(Obj, StoreIn, StoreOut) :-
    obj_id(Obj, ID),
    ( gen_assoc(ID, StoreIn, Attrs) ->
        % Remove collision_id attribute if it exists
        ( select(attr(collision_id, _), Attrs, NewAttrs) ->
            put_assoc(ID, StoreIn, NewAttrs, StoreOut)
        ;
            StoreOut = StoreIn
        )
    ;
        StoreOut = StoreIn
    ).

% ----------------------------------------------------------
% Step 2: Find all collision pairs
% ----------------------------------------------------------
find_collision_pairs(Objects, AttrStore, Pairs) :-
    findall(
        (ID1, ID2),
        (
            member(Obj1, Objects),
            obj_id(Obj1, ID1),
            member(Obj2, Objects),
            obj_id(Obj2, ID2),
            ID1 @< ID2,  % Avoid duplicates
            same_position(AttrStore, ID1, ID2)
        ),
        Pairs
    ).

same_position(AttrStore, ID1, ID2) :-
    gen_assoc(ID1, AttrStore, Attrs1),
    gen_assoc(ID2, AttrStore, Attrs2),
    member(attr(x, X), Attrs1),
    member(attr(y, Y), Attrs1),
    member(attr(x, X), Attrs2),
    member(attr(y, Y), Attrs2).

% ----------------------------------------------------------
% Step 3: Write collision_id (last wins)
% ----------------------------------------------------------
write_collision_ids([]) --> [].
write_collision_ids([(ID1, ID2)|Rest]) -->
    % Write both directions
    ctx_set_attr_val(ID1/collision_id, ID2),
    ctx_set_attr_val(ID2/collision_id, ID1),
    % Continue (later writes override)
    write_collision_ids(Rest).
