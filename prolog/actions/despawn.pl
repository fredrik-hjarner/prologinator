% despawn action implementation


execute_action_impl(
    ctx_old(CtxIn),
    ctx_new(CtxOut),
    action(despawn),
    obj_old(object(id(ID), _, _, _)),
    result(despawned, _)
) :-
    execute_despawn(CtxIn, CtxOut, ID).

% ==========================================================
% execute_despawn/3
% ==========================================================
execute_despawn(CtxIn, CtxOut, ID) :-
    % Remove object's attributes from store
    ctx_attrs(AttrStore, CtxIn),
    ( gen_assoc(ID, AttrStore, _Attrs) ->
        del_assoc(ID, AttrStore, _, NewAttrStore)
    ;
        NewAttrStore = AttrStore
    ),
    ctx_set_attrs(NewAttrStore, CtxIn, CtxOut).

