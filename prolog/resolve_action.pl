resolve_action(
    MyID,
    wait(Frames),
    wait(Frames)
) -->
    !,
    [].

resolve_action(
    _MyID,
    loop(Actions),
    loop(Actions)
) -->
    !,
    [].

resolve_action(
    _MyID,
    loop(Running, Original),
    loop(Running, Original)
) -->
    !,
    [].

resolve_action(
    _MyID,
    log(Message),
    log(Message)
) -->
    !,
    [].
