% load action implementation


% load(+Path)
% Mode: load(+Path)
% Description: Loads a file containing a list of actions
%   and prepends them to the action queue
% Yields: false (expands immediately)

execute_action_impl(
    ctx_old(Ctx),
    ctx_new(Ctx),
    action(load(Path)),
    obj_old(ObjIn),
    result(completed, ObjOut)
) :-
    execute_load(Ctx, Path, ObjIn, ObjOut).

% ==========================================================
% execute_load/4
% ==========================================================
execute_load(_Ctx, Path, ObjIn, ObjOut) :-
    obj_acns(ObjIn, [_|Rest]),
    % Convert Path (list of chars) to atom for open/3
    % Path must be a string (list of chars), not an atom
    atom_chars(PathAtom, Path),
    % Read file as term (expects list of actions)
    setup_call_cleanup(
        open(PathAtom, read, Stream),
        read_term(Stream, Actions, []),
        close(Stream)
    ),
    % Validate it's a list
    ( is_list(Actions) ->
        append(Actions, Rest, NewActions),
        obj_acns_obj(ObjIn, NewActions, ObjOut)
    ;
        throw(error(
            type_error(list, Actions),
            load/1
        ))
    ).

