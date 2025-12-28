

empty_ctx(ctx(state(
    frame(0),
    objects([]),
    attrs(EmptyAttrs),
    status(playing),
    next_id(1),
    commands(spawn_cmds([]), fork_cmds([])),
    actionstore(EmptyActionStore)
), input(events([]), held([])))) :-
    empty_assoc(EmptyAttrs),
    empty_assoc(EmptyActionStore).