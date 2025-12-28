

ctx_attr_val(ObjectID/Key, Value) -->
    ctx_attrs(AttrStore),
    {
        gen_assoc(ObjectID, AttrStore, Attrs),
        member(attr(Key, Value), Attrs)
    }.

ctx_attr_val(ObjectID/Key, Value, Ctx) :-
    ctx_attr_val(ObjectID/Key, Value, Ctx, Ctx).

ctx_set_attr_val(ObjectID/Key, Value) -->
    ctx_attrs(AttrStoreIn),
    {
        set_attr_in_store_helper(
            AttrStoreIn, ObjectID, Key, Value, AttrStoreOut
        )
    },
    ctx_set_attrs(AttrStoreOut).

set_attr_in_store_helper(AttrStoreIn, ObjectID, Key,
                         Value, AttrStoreOut) :-
    ( gen_assoc(ObjectID, AttrStoreIn, OldAttrs) ->
        ( select(attr(Key, _), OldAttrs, Rest) ->
            true
        ;
            Rest = OldAttrs
        ),
        NewAttrs = [attr(Key, Value)|Rest],
        put_assoc(ObjectID, AttrStoreIn, NewAttrs,
                  AttrStoreOut)
    ;
        NewAttrs = [attr(Key, Value)],
        put_assoc(ObjectID, AttrStoreIn, NewAttrs,
                  AttrStoreOut)
    ).


obj_type(Obj, Type) -->
    {obj_id(Obj, ID)},
    ctx_attr_val(ID/type, Type).

obj_id_type(Obj, ID, Type) -->
    {obj_id(Obj, ID)},
    ctx_attr_val(ID/type, Type).
