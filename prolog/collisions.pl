
detect_collisions -->
    clear_all_collisions,
    ctx_objs_attrs(Objects, AttrStore),
    {find_collision_pairs(Objects, AttrStore, Pairs)},
    write_collision_ids(Pairs).

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
        ( select(attr(collision_id, _), Attrs, NewAttrs) ->
            put_assoc(ID, StoreIn, NewAttrs, StoreOut)
        ;
            StoreOut = StoreIn
        )
    ;
        StoreOut = StoreIn
    ).

find_collision_pairs(Objects, AttrStore, Pairs) :-
    empty_assoc(EmptyBuckets),
    foldl(add_obj_to_bucket(AttrStore), Objects,
          EmptyBuckets, Buckets),
    collision_pairs_from_buckets(Buckets, Pairs).

add_obj_to_bucket(AttrStore, Obj, BucketsIn, BucketsOut) :-
    obj_id(Obj, ID),
    ( gen_assoc(ID, AttrStore, Attrs),
      member(attr(x, X), Attrs),
      member(attr(y, Y), Attrs) ->
        Pos = pos(X, Y),
        ( get_assoc(Pos, BucketsIn, IDs) ->
            put_assoc(Pos, BucketsIn, [ID|IDs], BucketsOut)
        ;
            put_assoc(Pos, BucketsIn, [ID], BucketsOut)
        )
    ;
        BucketsOut = BucketsIn
    ).

collision_pairs_from_buckets(Buckets, Pairs) :-
    findall(
        (ID1, ID2),
        (
            gen_assoc(_Pos, Buckets, IDs),
            IDs = [_,_|_],
            select(ID1, IDs, Rest),
            member(ID2, Rest),
            ID1 @< ID2
        ),
        Pairs
    ).

write_collision_ids([]) --> [].
write_collision_ids([(ID1, ID2)|Rest]) -->
    ctx_set_attr_val(ID1/collision_id, ID2),
    ctx_set_attr_val(ID2/collision_id, ID1),
    write_collision_ids(Rest).
