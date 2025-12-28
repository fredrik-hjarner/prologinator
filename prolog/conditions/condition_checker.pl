
check_condition(ObjID, Condition) -->
    check_condition_impl(ObjID, Condition).

check_condition_impl(ObjID, and(Conditions)) -->
    check_and_conditions(Conditions, ObjID).
    
check_and_conditions([], _) --> [].
check_and_conditions([C|Cs], ObjID) -->
    check_condition_impl(ObjID, C),
    check_and_conditions(Cs, ObjID).

check_condition_impl(ObjID, or(Conditions)) -->
    !,
    {member(Condition, Conditions)},
    check_condition_impl(ObjID, Condition).

check_condition_impl(ObjID, not(Condition), Ctx, Ctx) :-
    !,
    \+ check_condition_impl(ObjID, Condition, Ctx, Ctx).

check_condition_impl(ObjID, Item in AttributePath) -->
    !,
    check_membership(ObjID, Item, AttributePath).

check_condition_impl(ObjID, Comparison) -->
    {is_comparison(Comparison)},
    !,
    check_comparison(ObjID, Comparison).

check_condition_impl(ObjID, exists(PathSpec)) -->
    !,
    ( {PathSpec = .(Path)} ->
        {PathToResolve = Path}
    ;
        {throw(
            error(invalid_path_spec(PathSpec), exists/1)
        )}
    ),
    strict_resolve_path(ObjID, PathToResolve, _Value).

check_condition_impl(_ObjID, Condition) -->
    {throw(error(
        unknown_condition(Condition),
        check_condition_impl/3
    ))}.


is_comparison(Term) :-
    compound(Term),
    functor(Term, Op, 2),
    memberchk(Op, [
        <, >, =<, >=, =, \=,
        #<, #>, #=<, #>=, #=, #\=
    ]).


check_comparison(ObjID, Comparison) -->
    {Comparison =.. [_Op, LeftSpec, RightSpec]},
    
    resolve_condition_value(ObjID, LeftSpec, LeftValue),
    resolve_condition_value(ObjID, RightSpec, RightValue),
    
    {ResolvedComparison =.. [_Op, LeftValue, RightValue]},
    
    {normalize_comparison(ResolvedComparison, Normalized)},
    
    {call(Normalized)}.


strip_prefix_at(.(Path), Path) :- !.
strip_prefix_at(Path, Path).

resolve_condition_value(
    ObjID, default(ValueExpr, Fallback), Value
) -->
    !,
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
    
    ( resolve_path(ObjID, PathSpec, Value) ->
        []
    ;
        {Value = Fallback}
    ).

resolve_condition_value(ObjID, PathSpec, Value) -->
    ( {PathSpec = .(Path)} ->
        resolve_path(ObjID, Path, Value)
    ; {atom(PathSpec)} ->
        {throw(
            error(
                missing_at_prefix(PathSpec),
                resolve_condition_value/4
            )
        )}
    ;
        resolve_path(ObjID, PathSpec, Value)
    ).




check_membership(ObjID, Item, AttributePath) -->
    resolve_condition_value(ObjID, Item, ItemValue),
    
    resolve_condition_value(
        ObjID, AttributePath, ListValue
    ),
    
    (   {is_list(ListValue)}
    ->
        {member(ItemValue, ListValue)}
    ;
        {throw(error(
            type_error(list, ListValue),
            check_membership/4
        ))}
    ).

