% =========================================================
% Path Resolution Module
% =========================================================
% Resolves attribute paths strictly: tries to find attrs.
% If an attribute is missing, resolution fails (crash).
% Fallback behavior is only allowed via the explicit
% default/2 construct.
%
% New Path Structure (Input to resolve_path):
%   - Key (Atom): Resolves attribute Key on ObjID.
%   - ObjectSpec:Key: ObjectSpec resolves to NextID, then
%     Key is resolved on NextID.
%   - :Path: Wraps a path to indicate it is an attribute.
%
% NOTE: The : prefix is treated as syntactic sugar and is
% stripped automatically to resolve the underlying path.

% ==========================================================
% Main: resolve_path(+Ctx, +Obj, +Path, -Value)
% ==========================================================
% Resolves a path (attribute reference or literal) to its
% value.

resolve_path(Obj, Path, Value) -->
    {obj_id(Obj, ObjID)},
    resolve_path_id(ObjID, Path, Value).

% Internal: resolve_path_id works with IDs
resolve_path_id(ObjID, Path, Value) -->
    % Strip : prefix if present (syntactic sugar)
    ( {Path = :(InnerPath)} ->
        {TruePath = InnerPath}
    ;
        {TruePath = Path}
    ),

    (   % Case 1: Compound path using dot functor
        {TruePath = Head:Rest}
    ->
        resolve_path_id(ObjID, Head, NextID),
        % Then resolve Rest starting from that object.
        % Rest must resolve to the final attribute Key/Value
        resolve_path_id(NextID, Rest, Value)

    ;   % Case 2: Simple atom (Basecase: final attribute
        % Key)
        {atom(TruePath)}
    ->
        % MUST find an attribute. If ctx_attr_val fails,
        % the predicate fails (crash).
        ctx_attr_val(ObjID/TruePath, Value)

    ;  % Case 3: Numbers, variables, compounds (that aren't
       % dot):
       % pass through as-is (not attribute references)
        {Value = TruePath}
    ).


% ==========================================================
% Strict Path Resolution (for exists() checks)
% ==========================================================
% Unlike resolve_path, this fails if any attribute in the
% path doesn't exist. No fallback to literals.

strict_resolve_path(Obj, Path, Value) -->
    {obj_id(Obj, ObjID)},
    strict_resolve_path_id(ObjID, Path, Value).

% Internal: strict_resolve_path_id works with IDs
% 1. Handle : prefix (strip it and recurse)
strict_resolve_path_id(ObjID, :(Path), Value) -->
    !,
    strict_resolve_path_id(ObjID, Path, Value).

% 2. Compound path using dot functor (recursive step)
strict_resolve_path_id(
    ObjID, FirstAttr:RestPath, Value
) -->
    !,
    % First resolve Head to get the next object ID
    strict_resolve_path_id(ObjID, FirstAttr, NextID),
    % Then resolve Rest starting from that object
    strict_resolve_path_id(NextID, RestPath, Value).

% 3. Simple atom (base case for navigation, MUST exist)
strict_resolve_path_id(ObjID, Path, Value) -->
    {atom(Path)},
    !,
    % Fails if attribute missing
    ctx_attr_val(ObjID/Path, Value).

% 4. Pass through (Numbers/Variables/Non-path compounds)
% This must be the last clause. It handles literals that
% were passed in (e.g., numbers, unbound variables, or
% complex terms that aren't paths).
strict_resolve_path_id(_ObjID, Path, Path) --> [].


% ==========================================================
% DCG Wrappers for Strict Path Resolution (Used by
% resolve_action)
% ==========================================================

% resolve_path_strict(+Obj, +Path, -Value) -->
% Uses strict_resolve_path/4 internally
resolve_path_strict(Obj, Path, Value) -->
    strict_resolve_path(Obj, Path, Value).

% resolve_path_to_attr(+Obj, +Path, -Pair) -->
% Resolves the path down to the final object ID and
% attribute Key.
% Path can be a location path or wrapped in ..
resolve_path_to_attr(Obj, Path, Pair) -->
    {obj_id(Obj, ObjID)},
    resolve_path_to_attr_id(ObjID, Path, Pair).

% Internal: resolve_path_to_attr_id works with IDs
resolve_path_to_attr_id(ObjID, :(Path), Pair) -->
    !,
    resolve_path_to_attr_id(ObjID, Path, Pair).

resolve_path_to_attr_id(ObjID, AttrName, ObjID/AttrName) -->
    {atom(AttrName)},  % Base case: simple attribute name
    !.

resolve_path_to_attr_id(ObjID,
                        FirstAttr:RestPath,
                        FinalID/Key) -->
    !,
    % Navigate through FirstAttr to get NextID
    strict_resolve_path_id(ObjID, FirstAttr, NextID),
    % Continue resolving RestPath
    resolve_path_to_attr_id(NextID, RestPath, FinalID/Key).