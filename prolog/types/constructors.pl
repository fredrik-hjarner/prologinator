

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

ctx_with_objs(Objects, Ctx) :-
    empty_ctx(Def),
    ctx_set_objs(Objects, Def, Ctx).

ctx_with_objs_input(Objects, Events, Held, Ctx) :-
    empty_ctx(Def),
    ctx_set_objs(Objects, Def, Ctx1),
    ctx_set_input(input(events(Events), held(Held)), Ctx1,
                  Ctx).

ctx_with_frame_objs_input(Frame, Objects, Events, Held,
                          Ctx) :-
    empty_ctx(Def),
    ctx_set_frame(Frame, Def, Ctx1),
    ctx_set_objs(Objects, Ctx1, Ctx2),
    ctx_set_input(input(events(Events), held(Held)), Ctx2,
                  Ctx).
