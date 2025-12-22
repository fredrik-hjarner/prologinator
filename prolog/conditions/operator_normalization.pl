% =========================================================
% Operator Normalization Module
% =========================================================
% Converts familiar comparison operators to CLP(FD)
% operators for consistent evaluation.
%
% Goal: Allow users to write familiar syntax like < and >
% while maintaining relational/bidirectional properties
% of CLP(FD) internally.
%
% Examples:
%   hp < 0       → hp #< 0
%   parent_id/hp <= 100  → parent_id/hp #=< 100
%   type = enemy → type #= enemy

% ==========================================================
% normalize_comparison(+ComparisonIn, -ComparisonOut)
% ==========================================================
% Converts a comparison operator to its CLP(FD) equivalent.
% If already a CLP(FD) operator, passes through unchanged.

normalize_comparison(Comparison, Normalized) :-
    Comparison =.. [Op, Left, Right],
    (   op_map(Op, CLP_Op)
    ->
        % Operator needs conversion
        Normalized =.. [CLP_Op, Left, Right]
    ;
        % Already in good form (CLP(FD) or pass-through)
        Normalized = Comparison
    ).

% ==========================================================
% Operator Mapping Table
% ==========================================================
% Maps familiar operators to CLP(FD) equivalents.
%
% Standard Prolog → CLP(FD):
%   < (less than)         → #< (clpz less than)
%   > (greater than)      → #> (clpz greater than)
%   =< (less or equal)    → #=< (clpz less or equal)
%   >= (greater or equal) → #>= (clpz greater or equal)
%   = (unification)       → #= (clpz equal)
%   \= (not unifiable)    → #\= (clpz not equal)
%
% Already good (pass-through):
%   #<, #>, #=<, #>=, #=, #\= (CLP(FD) operators)

% Friendly operators → CLP(FD)
op_map(  <,  #< ).
op_map(  >,  #> ).
op_map( =<, #=< ).
op_map( >=, #>= ).
op_map(  =, #= ).
op_map( \=, #\= ).

% CLP(FD) operators are already normalized
op_map( #<,  #< ).
op_map( #>,  #> ).
op_map( #=<, #=< ).
op_map( #>=, #>= ).
op_map( #=,  #= ).
op_map( #\=, #\= ).
