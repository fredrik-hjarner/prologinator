% =========================================================
% Main Entry: check_condition(+Ctx, +ObjID, +Condition)
% ==========================================================
% Evaluates a condition in the context of a game object ID.
%
% Returns: true if condition holds, false otherwise
%
% Condition Forms:
%   and(List)         - All conditions in list must succeed
%   or(List)          - Any condition in list must succeed
%   not(Condition)    - Condition must fail
%   Item in Path      - Item (value/path) in list attribute
%   Comparison        - Operators: <, >, =<, >=, =, \=, etc.

check_condition(ObjID, Condition) -->
    % Original definition used Object but actions pass ObjID
    % We use ObjID directly here.
    check_condition_impl(ObjID, Condition).

% Implementation dispatcher
check_condition_impl(ObjID, and(Conditions)) -->
    check_and_conditions(Conditions, ObjID).
    
check_and_conditions([], _) --> [].
check_and_conditions([C|Cs], ObjID) -->
    check_condition_impl(ObjID, C),
    check_and_conditions(Cs, ObjID).

check_condition_impl(ObjID, or(Conditions)) -->
    !,
    % ANY condition must succeed
    {member(Condition, Conditions)},
    check_condition_impl(ObjID, Condition).

check_condition_impl(ObjID, not(Condition), Ctx, Ctx) :-
    !,
    % Negation: condition must fail
    \+ check_condition_impl(ObjID, Condition, Ctx, Ctx).

check_condition_impl(ObjID, Item in AttributePath) -->
    !,
    % List membership: check if Item is in the list at
    % AttributePath
    check_membership(ObjID, Item, AttributePath).

check_condition_impl(ObjID, Comparison) -->
    % Handle comparison operators (<, >, =, etc.)
    {is_comparison(Comparison)},
    !,
    check_comparison(ObjID, Comparison).

% NEW: Explicit existence check (strict - no fallback)
check_condition_impl(ObjID, exists(PathSpec)) -->
    !,
    % Rule: PathSpec MUST be an attribute reference (start
    % with :)
    ( {PathSpec = :(Path)} ->
        {PathToResolve = Path}
    ;
        % If it doesn't start with :, it's an invalid path
        % specification for exists/1 (bare atom or wrong
        % structure)
        {throw(
            error(invalid_path_spec(PathSpec), exists/1)
        )}
    ),
    % Use strict resolution: fails if attr doesn't exist
    strict_resolve_path(ObjID, PathToResolve, _Value).

check_condition_impl(_ObjID, Condition) -->
    % Unknown condition type
    {throw(error(
        unknown_condition(Condition),
        check_condition_impl/3
    ))}.

% ==========================================================
% Helper: is_comparison(+Term)
% ==========================================================
% Checks if a term is a comparison operator (has 2 args
% and uses a comparison functor).

is_comparison(Term) :-
    compound(Term),
    functor(Term, Op, 2),
    memberchk(Op, [
        % Standard Prolog operators
        <, >, =<, >=, =, \=,
        % CLP(FD) operators
        #<, #>, #=<, #>=, #=, #\=
    ]).

% ==========================================================
% Helper: check_comparison(+Ctx, +ObjID, +Comparison)
% ==========================================================
% Evaluates a comparison after path resolution and
% operator normalization.

check_comparison(ObjID, Comparison) -->
    % Decompose comparison into operator and operands
    {Comparison =.. [_Op, LeftSpec, RightSpec]},
    
    % Resolve both sides (using strict path resolution,
    % allowing default/2 for explicit fallback)
    resolve_condition_value(ObjID, LeftSpec, LeftValue),
    resolve_condition_value(ObjID, RightSpec, RightValue),
    
    % Build the resolved comparison
    {ResolvedComparison =.. [_Op, LeftValue, RightValue]},
    
    % Normalize operators (< becomes #<, etc.)
    % NOTE: normalize_comparison/2 is expected to be
    % available
    {normalize_comparison(ResolvedComparison, Normalized)},
    
    % Evaluate the normalized comparison
    {call(Normalized)}.

% ==========================================================
% Helper: resolve_condition_value(+Ctx, +ObjID,+Spec,-Value)
% ==========================================================
% Resolves a value specification (Path or default/2) for use
% in conditions.

% Helper to strip the prefix : operator if present
% If PathSpec is :(Path), we extract Path.
strip_prefix_at(:(Path), Path) :- !.
strip_prefix_at(Path, Path).

% Case 1: Explicit default/2 handling
resolve_condition_value(
    ObjID, default(ValueExpr, Fallback), Value
) -->
    !,
    % Check if ValueExpr is bare atom (e.g., default(hp, 0))
    ( {atom(ValueExpr), \+ compound(ValueExpr)} ->
        {throw(
            error(
                missing_at_prefix(ValueExpr),
                resolve_condition_value/4
            )
        )}
    ;
        []
    ),
    
    {strip_prefix_at(ValueExpr, PathSpec)},
    
    % Try path resolution. If it fails, use fallback.
    ( resolve_path(ObjID, PathSpec, Value) ->
        []
    ;
        % Path resolution failed (due to missing attribute)
        {Value = Fallback}
    ).

% Case 2: Standard path resolution
resolve_condition_value(ObjID, PathSpec, Value) -->
    % --- FIX: Enforce : prefix for attribute lookups ---
    ( {PathSpec = :(Path)} ->
        % Explicit attribute reference: resolve path
        resolve_path(ObjID, Path, Value)
    ; {atom(PathSpec)} ->
        % Bare atom used where attribute reference is
        % expected (e.g., hp)
        {throw(
            error(
                missing_at_prefix(PathSpec),
                resolve_condition_value/4
            )
        )}
    ;
        % Literal value (number, variable, or compound
        % literal like 1+2)
        % resolve_path will return PathSpec as Value (via
        % its literal passthrough clause)
        resolve_path(ObjID, PathSpec, Value)
    ).


% ==========================================================
% Strict Path Resolution (for exists() checks)
% ==========================================================
% NOTE: Definition moved to
% prolog/conditions/path_resolution.pl
% to avoid discontiguous warnings.

% ==========================================================
% Helper: check_membership(+Ctx, +ObjID, +Item, 
%                          +AttributePath)
% ==========================================================
% Checks if Item is a member of the list at AttributePath.
%
% Item can be:
%   - A literal value (sword, 42, etc.)
%   - An attribute path (parent_id:weapon, etc.)
%
% AttributePath must resolve to a list.
%
% Examples:
%   sword in inventory
%   → Item=sword, Path=inventory
%   → resolve inventory to list
%   → check member(sword, list)
%
%   parent_id:weapon in allowed_weapons
%   → Item=parent_id:weapon, Path=allowed_weapons
%   → resolve Item to weapon on parent
%   → resolve Path to list
%   → check membership

check_membership(ObjID, Item, AttributePath) -->
    % Resolve the item (could be path or literal)
    resolve_condition_value(ObjID, Item, ItemValue),
    
    % Resolve the attribute path to get the list
    resolve_condition_value(
        ObjID, AttributePath, ListValue
    ),
    
    % ListValue must be a list
    (   {is_list(ListValue)}
    ->
        % Check membership
        {member(ItemValue, ListValue)}
    ;
        % AttributePath didn't resolve to a list
        {throw(error(
            type_error(list, ListValue),
            check_membership/4
        ))}
    ).

% ==========================================================
% Notes on Design
% ==========================================================
%
% 1. Path Resolution Strategy
%    - STRICT: Missing attributes cause failure/crash.
%    - Fallback only via explicit default/2.
%
% 2. Operator Normalization
%    - Converts < to #< automatically
%    - Happens after path resolution
%    - Maintains bidirectionality via CLP(FD)
%
% 3. Composition (and/or/not)
%    - Standard logical composition
%    - Arbitrary nesting allowed
%    - Uses maplist/member for efficiency
%
% 4. List Membership (in operator)
%    - Works with literal items or paths
%    - Works with literal lists or attribute paths
%    - Provides referential transparency
%
% Example Complex Condition:
%   and([
%       or([
%           default(:hp, 0) < 0,
%           status = dead
%       ]),
%       not(exists(:invulnerable)),
%       sword in inventory,
%       parent_id:team = ally
%   ])
%
% All of these compose naturally without special syntax.
