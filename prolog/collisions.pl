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
% Step 2: Find all collision pairs (optimized with
% spatial bucketing)
% ----------------------------------------------------------
% O(N) approach: Build position buckets, then extract pairs
% from buckets with multiple objects
find_collision_pairs(Objects, AttrStore, Pairs) :-
    % Build buckets: pos(X,Y) -> [ID1, ID2, ...]
    empty_assoc(EmptyBuckets),
    foldl(add_obj_to_bucket(AttrStore), Objects,
          EmptyBuckets, Buckets),
    % Extract collision pairs from buckets with 2+ objects
    collision_pairs_from_buckets(Buckets, Pairs).

% Add an object to the position bucket map
% Buckets: pos(X,Y) -> [ID1, ID2, ...] (reverse insertion
% order)
add_obj_to_bucket(AttrStore, Obj, BucketsIn, BucketsOut) :-
    obj_id(Obj, ID),
    ( gen_assoc(ID, AttrStore, Attrs),
      member(attr(x, X), Attrs),
      member(attr(y, Y), Attrs) ->
        Pos = pos(X, Y),
        ( get_assoc(Pos, BucketsIn, IDs) ->
            % Position exists: prepend ID to bucket
            put_assoc(Pos, BucketsIn, [ID|IDs], BucketsOut)
        ;
            % New position: create bucket with single ID
            put_assoc(Pos, BucketsIn, [ID], BucketsOut)
        )
    ;
        % Object has no position attributes, skip
        BucketsOut = BucketsIn
    ).

% Extract collision pairs from buckets
% Only buckets with 2+ objects have collisions
collision_pairs_from_buckets(Buckets, Pairs) :-
    findall(
        (ID1, ID2),
        (
            gen_assoc(_Pos, Buckets, IDs),
            % Bucket must have at least 2 objects
            IDs = [_,_|_],
            % Generate all pairs from this bucket
            select(ID1, IDs, Rest),
            member(ID2, Rest),
            % Ensure ID1 < ID2 to avoid duplicates
            ID1 @< ID2
        ),
        Pairs
    ).

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
