% ==========================================================
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

check_condition(Ctx, ObjID, Condition) :-
    % Original definition used Object, but acns pass ObjID.
    % We use ObjID directly here.
    check_condition_impl(Ctx, ObjID, Condition).

% NEW: Explicit existence check (strict - no fallback)
check_condition_impl(Ctx, ObjID, exists(PathSpec)) :-
    !,
    % --- FIX: PathSpec MUST start with @ if it refers to an
    %  attribute path ---
    ( PathSpec = @(Path) ->
        PathToResolve = Path
    ;
        % If it doesn't start with @, it's an invalid path
        % specification for exists/1
        {throw(
            error(invalid_path_spec(PathSpec), exists/1)
        )}
    ),
    % Use strict resolution: fails if attr doesn't exist
    strict_resolve_path(Ctx, ObjID, PathToResolve, _Value).

% Implementation dispatcher
check_condition_impl(Ctx, ObjID, and(Conditions)) :-
    !,
    % ALL conditions must succeed
    maplist(check_condition_impl(Ctx, ObjID), 
            Conditions).

check_condition_impl(Ctx, ObjID, or(Conditions)) :-
    !,
    % ANY condition must succeed
    member(Condition, Conditions),
    check_condition_impl(Ctx, ObjID, Condition).

check_condition_impl(Ctx, ObjID, not(Condition)) :-
    !,
    % Negation: condition must fail
    \+ check_condition_impl(Ctx, ObjID, Condition).

check_condition_impl(Ctx, ObjID, Item in AttributePath) :-
    !,
    % List membership: check if Item is in the list at
    % AttributePath
    check_membership(Ctx, ObjID, Item, AttributePath).

check_condition_impl(Ctx, ObjID, Comparison) :-
    % Handle comparison operators (=, <, >, etc.)
    is_comparison(Comparison),
    !,
    check_comparison(Ctx, ObjID, Comparison).

check_condition_impl(_, _, Condition) :-
    % Unknown condition type
    throw(error(
        unknown_condition(Condition),
        check_condition_impl/3
    )).

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

check_comparison(Ctx, ObjID, Comparison) :-
    % Decompose comparison into operator and operands
    Comparison =.. [_Op, LeftSpec, RightSpec],
    
    % Resolve both sides (using strict path resolution,
    % allowing default/2 for explicit fallback)
    resolve_condition_value(
        Ctx, ObjID, LeftSpec, LeftValue
    ),
    resolve_condition_value(
        Ctx, ObjID, RightSpec, RightValue
    ),
    
    % Build the resolved comparison
    ResolvedComparison =.. [_Op, LeftValue, RightValue],
    
    % Normalize operators (< becomes #<, etc.)
    % NOTE: normalize_comparison/2 is expected to be
    % available
    normalize_comparison(ResolvedComparison, 
                         Normalized),
    
    % Evaluate the normalized comparison
    call(Normalized).

% ==========================================================
% Helper: resolve_condition_value(+Ctx, +ObjID, +Spec, -Val)
% ==========================================================
% Resolves a value specification (Path or default/2) for use
% in conditions.

% Helper to strip the prefix @ operator if present
strip_prefix_at(@(Path), Path) :- !.
strip_prefix_at(Path, Path).

% Case 1: Explicit default/2 handling
resolve_condition_value(
    Ctx, ObjID, default(ValueExpr, Fallback), Value
) :-
    !,
    strip_prefix_at(ValueExpr, PathSpec),
    
    % Try strict path resolution. If it fails, use fallback.
    % resolve_path/4 now crashes if the path is missing, but
    % we catch the failure here to implement the fallback.
    ( resolve_path(Ctx, ObjID, PathSpec, Value) ->
        true
    ;
        % Path resolution failed (due to missing attribute)
        Value = Fallback
    ).

% Case 2: Standard path resolution (will crash if path
% missing)
resolve_condition_value(Ctx, ObjID, PathSpec, Value) :-
    strip_prefix_at(PathSpec, Path),
    resolve_path(Ctx, ObjID, Path, Value).


% ==========================================================
% Strict Path Resolution (for exists() checks)
% ==========================================================
% Unlike resolve_path, this fails if any attribute in the
% path doesn't exist. No fallback to literals.

% 1. Compound path (recursive step)
strict_resolve_path(Ctx, ObjID, Head@Rest, Value) :-
    !,
    strict_resolve_path(Ctx, ObjID, Head, NextID),
    strict_resolve_path(Ctx, NextID, Rest, Value).

% 2. Simple atom (base case for navigation, MUST exist)
strict_resolve_path(Ctx, ObjID, Path, Value) :-
    atom(Path),
    !,
    % Fails if attribute missing
    ctx_attr_val(ObjID/Path, Value, Ctx, Ctx).

% 3. Pass through (Numbers/Variables/Non-path compounds)
strict_resolve_path(_Ctx, _ObjID, Path, Path).

% ==========================================================
% Helper: check_membership(+Ctx, +ObjID, +Item, 
%                          +AttributePath)
% ==========================================================
% Checks if Item is a member of the list at AttributePath.
%
% Item can be:
%   - A literal value (sword, 42, etc.)
%   - An attribute path (parent_id@weapon, etc.)
%
% AttributePath must resolve to a list.
%
% Examples:
%   sword in inventory
%   → Item=sword, Path=inventory
%   → resolve inventory to list
%   → check member(sword, list)
%
%   parent_id@weapon in allowed_weapons
%   → Item=parent_id@weapon, Path=allowed_weapons
%   → resolve Item to weapon on parent
%   → resolve Path to list
%   → check membership

check_membership(Ctx, ObjID, Item, AttributePath) :-
    % Resolve the item (could be path or literal)
    % Note: Item resolution uses resolve_path, which is now
    % strict.
    resolve_condition_value(Ctx, ObjID, Item, ItemValue),
    
    % Resolve the attribute path to get the list
    % Note: AttributePath resolution uses resolve_path,
    % which is now strict.
    resolve_condition_value(
        Ctx, ObjID, AttributePath, ListValue
    ),
    
    % ListValue must be a list
    (   is_list(ListValue)
    ->
        % Check membership
        member(ItemValue, ListValue)
    ;
        % AttributePath didn't resolve to a list
        throw(error(
            type_error(list, ListValue),
            check_membership/4
        ))
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
%           default(@hp, 0) < 0,
%           status = dead
%       ]),
%       not(exists(@invulnerable)),
%       sword in inventory,
%       parent_id@team = ally
%   ])
%
% All of these compose naturally without special syntax.
