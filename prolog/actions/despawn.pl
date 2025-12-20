execute_action_impl(
    actions_old([despawn|_]),
    obj_id(ID),
    result(despawned, actions_new([]))
) -->
    execute_despawn(ID).

execute_despawn(ID) -->
    % Remove object's attributes from store
    ctx_attrs(AttrStore),
    {
        ( gen_assoc(ID, AttrStore, _Attrs) ->
            del_assoc(ID, AttrStore, _, NewAttrStore)
        ;
            NewAttrStore = AttrStore
        )
    },
    ctx_set_attrs(NewAttrStore).


