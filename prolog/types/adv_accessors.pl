% Advanced Accessors Module
% Provides high-level helpers for working with the
% centralized attribute store. These predicates abstract
% away the details of storage and retrieval from assoc
% trees.

% ==========================================================
% Attribute Store Helpers
% ==========================================================

% Retrieves a specific attribute value for an object from
% the centralized attribute store.
% Fails if the object or attribute doesn't exist. Uses
% ctx_attrs to get the store, then looks up the object's
% attributes and finds the matching key-value.
% getter ctx_attr_val/4 for dcg use
% Bidirectional: ObjectID and Key can be non-ground.
% Uses attr(Key, Value) format internally for full
% bidirectionality.
ctx_attr_val(ObjectID/Key, Value, Ctx, Ctx) :-
    ctx_attrs(AttrStore, Ctx),
    gen_assoc(ObjectID, AttrStore, Attrs),
    member(attr(Key, Value), Attrs).

% getter ctx_attr_val/3 for non-dcg use
ctx_attr_val(ObjectID/Key, Value, Ctx) :-
    % utilize dcg version
    ctx_attr_val(ObjectID/Key, Value, Ctx, Ctx).

% Updates or creates an attribute for an object in the
% centralized store. Returns a new context with the updated
% attribute store. Replaces existing values for the same
% key if they exist, otherwise appends to the object's
% attribute list.
% ctx_set_attr_val(+ObjectID/Key, +Value, +CtxIn, -CtxOut)
ctx_set_attr_val(ObjectID/Key, Value, CtxIn, CtxOut) :-
    ctx_attrs(AttrStoreIn, CtxIn),
    set_attr_in_store_helper(AttrStoreIn, ObjectID, Key,
                             Value, AttrStoreOut),
    ctx_set_attrs(AttrStoreOut, CtxIn, CtxOut).

% Helper to update attribute in assoc tree
% Handles the low-level logic of updating the assoc tree:
% removes old attribute values if they exist, then adds the
% new value. Creates a new entry if the object isn't in
% the store yet. This is the internal implementation
% detail for ctx_attr_val_ctx/4.
% Uses attr(Key, Value) format for full bidirectionality.
set_attr_in_store_helper(AttrStoreIn, ObjectID, Key,
                         Value, AttrStoreOut) :-
    ( gen_assoc(ObjectID, AttrStoreIn, OldAttrs) ->
        % Remove old value if exists
        ( select(attr(Key, _), OldAttrs, Rest) ->
            true
        ;
            Rest = OldAttrs
        ),
        NewAttrs = [attr(Key, Value)|Rest],
        put_assoc(ObjectID, AttrStoreIn, NewAttrs,
                  AttrStoreOut)
    ;
        % Object doesn't exist in store, create it
        NewAttrs = [attr(Key, Value)],
        put_assoc(ObjectID, AttrStoreIn, NewAttrs,
                  AttrStoreOut)
    ).

