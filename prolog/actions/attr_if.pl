builtin_action(attr_if(_, _, _)).


execute_action_impl(
    actions_old([
        attr_if(Condition, ThenActions, ElseActions)|Rest
    ]),
    obj_id(ID),
    result(Status, actions_new(ActionsOut))
) -->
    execute_attr_if(
        ID,
        Condition,
        ThenActions,
        ElseActions,
        Rest,
        result(Status, actions_new(ActionsOut))
    ).


execute_attr_if(
    ObjID,
    Condition,
    ThenActions,
    ElseActions,
    Rest,
    result(Status, actions_new(ActionsOut))
) -->
    (   check_condition(ObjID, Condition)
    ->
        tick_object(
            actions_old(ThenActions),
            obj_id(ObjID),
            result(Status, actions_new(ActionAfterTick))
        ),
        {append(ActionAfterTick, Rest, ActionsOut)}
    ;
        tick_object(
            actions_old(ElseActions),
            obj_id(ObjID),
            result(Status, actions_new(ActionAfterTick))
        ),
        {append(ActionAfterTick, Rest, ActionsOut)}
    ).
