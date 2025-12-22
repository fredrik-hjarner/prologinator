% =========================================================
% Path Resolution Module
% =========================================================
% Resolves attribute paths strictly: tries to find attrs.
% If an attribute is missing, resolution fails (crash).
% Fallback behavior is only allowed via the explicit
% default/2 construct.
%
% Examples:
%   - resolve_path(Ctx, 1, hp, Value)
%       → looks up attribute hp on object 1
%   - resolve_path(Ctx, 1, parent_id@hp, Value)
%       → looks up parent_id to get NextID then hp on NextID
%   - resolve_path(Ctx, 1, enemy, Value)
%       → if no attribute 'enemy' exists, fails (crashes)

% ==========================================================
% Main: resolve_path(+Ctx, +ObjID, +Path, -Value)
% ==========================================================
% Resolves a path (attribute reference or literal) to its
% value.
%
% Three cases:
%   1. Compound path (A@B): recurse through chain
%   2. Simple atom: MUST resolve to attribute
%   3. Numbers/vars: pass through as-is

resolve_path(Ctx, ObjID, Path, Value) :-
    (   % Case 1: Compound path (A@B or A@B@C...)
        Path = (Head)@Rest
    ->
        % First resolve Head to get the next object ID
        resolve_path(Ctx, ObjID, Head, NextID),
        % Then resolve Rest starting from that object
        resolve_path(Ctx, NextID, Rest, Value)
    
    ;   % Case 2: Simple atom
        atom(Path)
    ->
        % MUST find an attribute. If ctx_attr_val fails,
        % the predicate fails (crash).
        ctx_attr_val(ObjID/Path, Value, Ctx, Ctx)
    
    ;   % Case 3: Numbers, variables, compounds:
        % pass through as-is (not attribute references)
        Value = Path
    ).
