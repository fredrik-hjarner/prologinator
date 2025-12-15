% Collision Detection Module
% Handles collision detection between game objects
% Sets collision_id attributes on colliding objects

% ==========================================================
% Main entry: runs before tick_all_objects
% ==========================================================
detect_collisions(ctx_old(CtxIn), ctx_new(CtxOut)) :-
    % 1. Clear old collision_id attributes
    clear_all_collisions(CtxIn, Ctx1),
    
    % 2. Find all position overlaps
    ctx_objs_attrs(Objects, AttrStore, Ctx1),
    find_collision_pairs(Objects, AttrStore, Pairs),
    % Pairs = [(5, 12), (5, 18), (8, 12), ...]
    
    % 3. Write collision_id for each object
    write_collision_ids(Pairs, Ctx1, CtxOut).

% ----------------------------------------------------------
% Step 1: Clear collision_id attributes
% ----------------------------------------------------------
clear_all_collisions(CtxIn, CtxOut) :-
    ctx_attrs(StoreIn, CtxIn),
    ctx_objs(Objects, CtxIn),
    foldl(clear_one_collision, 
          Objects, 
          StoreIn, 
          StoreOut),
    ctx_set_attrs(StoreOut, CtxIn, CtxOut).

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
write_collision_ids([], Ctx, Ctx).
write_collision_ids([(ID1, ID2)|Rest], 
                    CtxIn, CtxOut) :-
    % Write both directions
    ctx_attr_val_ctx(CtxIn, ID1/collision_id, 
                     ID2, Ctx1),
    ctx_attr_val_ctx(Ctx1, ID2/collision_id, 
                     ID1, Ctx2),
    % Continue (later writes override)
    write_collision_ids(Rest, Ctx2, CtxOut).
