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
%   - ObjectSpec.Key: ObjectSpec resolves to NextID, then
%     Key is resolved on NextID.
%   - .Path: Wraps a path to indicate it is an attribute.
%
% NOTE: The . prefix is treated as syntactic sugar and is
% stripped automatically to resolve the underlying path.

% ==========================================================
% Main: resolve_path(+Ctx, +ObjID, +Path, -Value)
% ==========================================================
% Resolves a path (attribute reference or literal) to its
% value.

resolve_path(Ctx, ObjID, Path, Value) :-
    % Strip . prefix if present (syntactic sugar)
    ( Path = .(InnerPath) ->
        TruePath = InnerPath
    ;
        TruePath = Path
    ),

    (   % Case 1: Compound path using dot functor
        TruePath = Head.Rest
    ->
        resolve_path(Ctx, ObjID, Head, NextID),
        % Then resolve Rest starting from that object.
        % Rest must resolve to the final attribute Key/Value
        resolve_path(Ctx, NextID, Rest, Value)

    ;   % Case 2: Simple atom (Basecase: final attribute
        % Key)
        atom(TruePath)
    ->
        % MUST find an attribute. If ctx_attr_val fails,
        % the predicate fails (crash).
        ctx_attr_val(ObjID/TruePath, Value, Ctx, Ctx)

    ;  % Case 3: Numbers, variables, compounds (that aren't
       % dot):
       % pass through as-is (not attribute references)
        Value = TruePath
    ).


% ==========================================================
% Strict Path Resolution (for exists() checks)
% ==========================================================
% Unlike resolve_path, this fails if any attribute in the
% path doesn't exist. No fallback to literals.

% 1. Handle . prefix (strip it and recurse)
strict_resolve_path(Ctx, ObjID, .(Path), Value) :-
    !,
    strict_resolve_path(Ctx, ObjID, Path, Value).

% 2. Compound path using dot functor (recursive step)
strict_resolve_path(
    Ctx, ObjID, FirstAttr.RestPath, Value
) :-
    !,
    % First resolve Head to get the next object ID
    strict_resolve_path(Ctx, ObjID, FirstAttr, NextID),
    % Then resolve Rest starting from that object
    strict_resolve_path(Ctx, NextID, RestPath, Value).

% 3. Simple atom (base case for navigation, MUST exist)
strict_resolve_path(Ctx, ObjID, Path, Value) :-
    atom(Path),
    !,
    % Fails if attribute missing
    ctx_attr_val(ObjID/Path, Value, Ctx, Ctx).

% 4. Pass through (Numbers/Variables/Non-path compounds)
% This must be the last clause. It handles literals that
% were passed in (e.g., numbers, unbound variables, or
% complex terms that aren't paths).
strict_resolve_path(_Ctx, _ObjID, Path, Path).


% ==========================================================
% DCG Wrappers for Strict Path Resolution (Used by
% resolve_action)
% ==========================================================

% resolve_path_strict(+ObjID, +Path, -Value) -->
% Uses strict_resolve_path/4 internally
resolve_path_strict(ObjID, Path, Value) -->
    ctx_get(Ctx),
    {strict_resolve_path(Ctx, ObjID, Path, Value)}.

% resolve_path_to_attr(+ObjID, +Path, -Pair) -->
% Resolves the path down to the final object ID and
% attribute Key.
% Path can be a location path or wrapped in ..
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
    % Navigate through FirstAttr to get NextID
    resolve_path_strict(MyID, FirstAttr, NextID),
    % Continue resolving RestPath
    resolve_path_to_attr(NextID, RestPath, FinalID/Key).