% load(+Path)
% Mode: load(+Path)
% Description: Loads a file containing a list of actions
%   and prepends them to the action queue
% Yields: false (expands immediately)

execute_action_impl(
    action(load(Path)),
    actions_old([_|Rest]),
    obj_id(_ID),
    result(completed, actions_new(NewActions))
) -->
    execute_load(Path, Rest, NewActions).

execute_load(Path, Rest, NewActions) -->
    {
        % Convert Path (list of chars) to atom for open/3
        atom_chars(PathAtom, Path),
        % Read file as term (expects list of actions)
        setup_call_cleanup(
            open(PathAtom, read, Stream),
            read_term(Stream, Actions, []),
            close(Stream)
        ),
        % Validate it's a list
        ( is_list(Actions) ->
            append(Actions, Rest, NewActions)
        ;
            throw(error(
                type_error(list, Actions),
                load/1
            ))
        )
    }.


