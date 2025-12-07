:- module(parallel_test, []).
:- use_module('./parallel').

% ==========================================================
% Tests: parallel
% ==========================================================

test("parallel: backward test - can infer original \
wait_frames values from parallel_running state", (
    % N1 and N2 are unknown - should be inferred
    Action = parallel([wait_frames(N1), wait_frames(N2)]),
    ObjIn = object(
        id(1),
        type(static),
        attrs([]),
        actions([
            parallel([wait_frames(N1), wait_frames(N2)])
        ]),
        collisions([])
    ),
    ObjOut = [object(
        id(1),
        type(static),
        attrs([]),
        actions([
            parallel_running([
                wait_frames(2),
                wait_frames(3)
            ])
        ]),
        collisions([])
    )],
    Commands = [],
    RevHints = [],
    Ctx = ctx(state(
        frame(0),
        [],
        status(playing),
        next_id(1),
        [],
        []
    )),
    execute_action:execute_action(
        ctx_old(Ctx),
        action(Action),
        obj_old(ObjIn),
        obj_new(ObjOut),
        cmds_new(Commands),
        revhints_new(RevHints)
    ),
    N1 = 3,  % Verify that N1 was correctly inferred
    N2 = 4   % Verify that N2 was correctly inferred
)).

