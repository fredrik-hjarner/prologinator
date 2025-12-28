builtin_action(load(_)).


execute_action_impl(
    actions_old([load(Path)|Rest]),
    obj_id(_ID),
    result(completed, actions_new(NewActions))
) -->
    execute_load(Path, Rest, NewActions).

execute_load(Path, Rest, NewActions) -->
    {
        atom_chars(PathAtom, Path),
        setup_call_cleanup(
            open(PathAtom, read, Stream),
            read_term(Stream, Actions, []),
            close(Stream)
        ),
        ( is_list(Actions) ->
            append(Actions, Rest, NewActions)
        ;
            throw(error(
                type_error(list, Actions),
                load/1
            ))
        )
    }.


