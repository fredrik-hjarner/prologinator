% despawn action implementation


execute_action_impl(
    action(despawn),
    obj_old(object(id(ID), _, _, _)),
    result(despawned, _),
    CtxIn,
    CtxOut
) :-
    execute_despawn(ID, CtxIn, CtxOut).

% ==========================================================
% execute_despawn/3
% ==========================================================
execute_despawn(ID, CtxIn, CtxOut) :-
    % Remove object's attributes from store
    ctx_attrs(AttrStore, CtxIn, CtxIn),
    ( gen_assoc(ID, AttrStore, _Attrs) ->
        del_assoc(ID, AttrStore, _, NewAttrStore)
    ;
        NewAttrStore = AttrStore
    ),
    ctx_set_attrs(NewAttrStore, CtxIn, CtxOut).

