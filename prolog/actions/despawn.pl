% despawn action implementation

:- module(execute_action_despawn, []).

:- use_module('../types/accessors').
:- use_module('../types/adv_accessors').
:- use_module(library(assoc), [
    gen_assoc/3,
    del_assoc/4
]).

:- multifile(execute_action:execute_action_impl/5).
:- discontiguous(execute_action:execute_action_impl/5).

execute_action:execute_action_impl(
    ctx_old(CtxIn),
    ctx_new(CtxOut),
    action(despawn),
    obj_old(object(id(ID), _, _, _)),
    obj_new([])
) :-
    % Remove object's attributes from store
    ctx_attrs(CtxIn, AttrStore),
    ( gen_assoc(ID, AttrStore, _Attrs) ->
        del_assoc(ID, AttrStore, _, NewAttrStore)
    ;
        NewAttrStore = AttrStore
    ),
    ctx_attrs_ctx(CtxIn, NewAttrStore, CtxOut).

