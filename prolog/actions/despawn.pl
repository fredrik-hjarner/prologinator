% despawn action implementation

% TODO: Should send up a despawn command.
execute_action_impl(
    action(despawn),
    obj_old(object(id(ID), _)),
    result(despawned, _)
) -->
    execute_despawn(ID).

% ==========================================================
% execute_despawn/3
% ==========================================================
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

