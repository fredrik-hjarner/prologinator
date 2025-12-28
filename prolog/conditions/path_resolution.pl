

resolve_path(ObjID, Path, Value) -->
    ( {Path = .(InnerPath)} ->
        {TruePath = InnerPath}
    ;
        {TruePath = Path}
    ),

    (   % Case 1: Compound path using dot functor
        {TruePath = Head.Rest}
    ->
        resolve_path(ObjID, Head, NextID),
        resolve_path(NextID, Rest, Value)

    ;   % Case 2: Simple atom (Basecase: final attribute
        {atom(TruePath)}
    ->
        ctx_attr_val(ObjID/TruePath, Value)

    ;  % Case 3: Numbers, variables, compounds (that aren't
        {Value = TruePath}
    ).



strict_resolve_path(ObjID, .(Path), Value) -->
    !,
    strict_resolve_path(ObjID, Path, Value).

strict_resolve_path(
    ObjID, FirstAttr.RestPath, Value
) -->
    !,
    strict_resolve_path(ObjID, FirstAttr, NextID),
    strict_resolve_path(NextID, RestPath, Value).

strict_resolve_path(ObjID, Path, Value) -->
    {atom(Path)},
    !,
    ctx_attr_val(ObjID/Path, Value).

strict_resolve_path(_ObjID, Path, Path) --> [].



resolve_path_strict(ObjID, Path, Value) -->
    strict_resolve_path(ObjID, Path, Value).

resolve_path_to_attr(MyID, .(Path), Pair) -->
    !,
    resolve_path_to_attr(MyID, Path, Pair).

resolve_path_to_attr(MyID, AttrName, MyID/AttrName) -->
    {atom(AttrName)},  % Base case: simple attribute name
    !.

resolve_path_to_attr(MyID,
                     FirstAttr.RestPath,
                     FinalID/Key) -->
    !,
    resolve_path_strict(MyID, FirstAttr, NextID),
    resolve_path_to_attr(NextID, RestPath, FinalID/Key).