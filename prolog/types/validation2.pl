


object_type_schema(enum([
    static, enemy, proj, player, tower
])).

game_status_schema(enum([playing, won, lost])).

pos_schema(struct(pos, [
    x(integer),
    y(integer)
])).


game_over_schema(struct(game_over, [
    status(enum([won, lost]))
])).

state_change_content_schema(schema(game_over_schema)).


action_schema(union([
    schema(despawn_schema),
    schema(wait_schema),
    schema(move_to_schema),
    schema(spawn_action_schema),
    schema(loop_schema),
    schema(trigger_state_change_schema),
    schema(parallel_all_schema),
    schema(parallel_all_running_schema)
])).

wait_schema(struct(wait, [
    frames(integer)
])).

move_to_schema(struct(move_to, [
    x(integer),
    y(integer),
    frames(integer)
])).

despawn_schema(enum([despawn])).

spawn_action_schema(struct(spawn, [
    type(schema(object_type_schema)),
    pos(schema(pos_schema)),
    actions(list(schema(action_schema)))
])).

loop_schema(struct(loop, [
    actions(list(schema(action_schema)))
])).

trigger_state_change_schema(struct(trigger_state_change, [
    change(schema(state_change_content_schema))
])).

parallel_all_schema(struct(parallel_all, [
    children(list(schema(action_schema)))
])).

parallel_all_running_schema(struct(parallel_all_running, [
    children(list(schema(action_schema)))
])).



command_schema(union([
    schema(spawn_request_schema),
    schema(state_change_schema)
])).

spawn_request_schema(struct(spawn_request, [
    type(schema(object_type_schema)),
    pos(schema(pos_schema)),
    actions(list(schema(action_schema)))
])).

state_change_schema(struct(state_change, [
    change(schema(state_change_content_schema))
])).



id_schema(struct(id, [
    value(integer)
])).

type_schema(struct(type, [
    value(schema(object_type_schema))
])).

attrs_schema(struct(attrs, [
    value(any)
])).

actions_schema(struct(actions, [
    value(list(schema(action_schema)))
])).

object_schema(struct(object, [
    id(schema(id_schema)),
    actions(schema(actions_schema))
])).



frame_schema(struct(frame, [
    value(integer(0, _))
])).

objects_schema(struct(objects, [
    value(list(schema(object_schema)))
])).

status_schema(struct(status, [
    value(schema(game_status_schema))
])).

next_id_schema(struct(next_id, [
    value(integer(1, _))
])).

commands_schema(struct(commands, [
    value(list(schema(command_schema)))
])).

actionstore_schema(struct(actionstore, [
    value(any)  % assoc list - validation handled by assoc
])).

state_schema(struct(state, [
    frame(schema(frame_schema)),
    objects(schema(objects_schema)),
    attrs(schema(attrs_schema)),
    status(schema(status_schema)),
    next_id(schema(next_id_schema)),
    commands(schema(commands_schema)),
    actionstore(schema(actionstore_schema))
])).

context_schema(struct(ctx, [
    state(schema(state_schema))
])).


:- discontiguous(test/2).

test("game_state_validation: valid state passes", (
    empty_assoc(EmptyAttrs0),
    put_assoc(0, EmptyAttrs0, [attr(type, static)],
              EmptyAttrs),
    empty_assoc(EmptyActionStore),
    State = state(
        frame(0),
        objects([object(
            id(0),
            actions([])
        )]),
        attrs(EmptyAttrs),
        status(playing),
        next_id(1),
        commands([]),
        actionstore(EmptyActionStore)
    ),
    validate(State, state_schema)
)).

test("game_state_validation: complex state with \
multiple objects and commands", (
    empty_assoc(EmptyAttrs0),
    put_assoc(0, EmptyAttrs0, [attr(type, tower)],
              Attrs1),
    put_assoc(1, Attrs1, [attr(type, enemy)],
              Attrs2),
    put_assoc(2, Attrs2, [attr(type, proj)],
              EmptyAttrs),
    empty_assoc(EmptyActionStore),
    State = state(
        frame(5),
        objects([
            object(
                id(0),
                actions([wait(3)])
            ),
            object(
                id(1),
                actions([move_to(10, 10, 5)])
            ),
            object(
                id(2),
                actions([])
            )
        ]),
        attrs(EmptyAttrs),
        status(playing),
        next_id(3),
        commands([
            spawn_request(enemy, pos(0, 0), []),
            spawn_request(enemy, pos(0, 0), [])
        ]),
        actionstore(EmptyActionStore)
    ),
    validate(State, state_schema)
)).

test("context_validation: complex context with multiple \
objects and commands", (
    empty_assoc(EmptyAttrs0),
    put_assoc(0, EmptyAttrs0, [attr(type, tower)],
              Attrs1),
    put_assoc(1, Attrs1, [attr(type, enemy)],
              Attrs2),
    put_assoc(2, Attrs2, [attr(type, proj)],
              Attrs3),
    put_assoc(3, Attrs3, [attr(type, static)],
              EmptyAttrs),
    ctx_with_frame_objs_input(42, [
        object(
            id(0),
            actions([
                loop([
                    wait(3),
                    spawn(proj, pos(5, 19), [
                        move_to(5, 0, 20)
                    ])
                ])
            ])
        ),
        object(
            id(1),
            actions([
                move_to(19, 5, 15),
                wait(1)
            ])
        ),
        object(
            id(2),
            actions([move_to(15, 0, 10)])
        ),
        object(
            id(3),
            actions([
                parallel_all([
                    wait(5),
                    spawn(enemy, pos(0, 10), [])
                ])
            ])
        )
    ], [], [], Ctx0),
    ctx_set_attrs(EmptyAttrs, Ctx0, Ctx1),
    ctx_set_nextid(4, Ctx1, Ctx2),
    ctx_set_cmds([
        spawn_request(enemy, pos(0, 0), []),
        spawn_request(proj, pos(10, 10), [
            move_to(10, 0, 10)
        ])
    ], Ctx2, Ctx),
    validate(Ctx, context_schema)
)).

expect_exception(Goal) :-
    catch((Goal, fail), _, true).


test("game_state_validation: invalid status fails", (
    empty_assoc(EmptyActionStore),
    State = state(
        frame(0),
        objects([]),
        attrs(t),
        status(invalid_status),
        next_id(1),
        commands([]),
        actionstore(EmptyActionStore)
    ),
    expect_exception(
        validate(State, state_schema)
    )
)).

test("game_state_validation: non-integer frame fails", (
    empty_assoc(EmptyActionStore),
    State = state(
        frame(not_an_int),
        objects([]),
        attrs(t),
        status(playing),
        next_id(1),
        commands([]),
        actionstore(EmptyActionStore)
    ),
    expect_exception(
        validate(State, state_schema)
    )
)).


test("object_validation: wrong structure (ID not \
wrapped) throws", (
    Obj = object(
        5,
        actions([])
    ),
    expect_exception(
        validate(Obj, object_schema)
    )
)).

test("command_validation: invalid spawn type fails", (
    Cmd = spawn_request(invalid_type, pos(0, 0), []),
    expect_exception(
        validate(Cmd, command_schema)
    )
)).

test("command_validation: invalid pos in spawn fails", (
    Cmd = spawn_request(static, not_a_pos, []),
    expect_exception(
        validate(Cmd, command_schema)
    )
)).

test("pos_validation: non-integer coords fail via action", (
    Action1 = spawn(static, pos(not_int, 10), []),
    expect_exception(
        validate(Action1, action_schema)
    ),
    Action2 = spawn(static, pos(10, not_int), []),
    expect_exception(
        validate(Action2, action_schema)
    )
)).

test("action_validation: invalid wait fails", (
    expect_exception(
        validate(wait(not_int), action_schema)
    )
)).

test("action_validation: invalid move_to fails", (
    Action1 = move_to(not_int,10,5),
    expect_exception(
        validate(Action1, action_schema)
    ),
    Action2 = move_to(10,not_int,5),
    expect_exception(
        validate(Action2, action_schema)
    ),
    Action3 = move_to(10,10,not_int),
    expect_exception(
        validate(Action3, action_schema)
    )
)).

test("action_validation: invalid spawn action fails", (
    Action1 = spawn(invalid_type, pos(0, 0), []),
    expect_exception(
        validate(Action1, action_schema)
    ),
    Action2 = spawn(static, not_a_pos, []),
    expect_exception(
        validate(Action2, action_schema)
    )
)).


test("game_state_validation: wrong arity throws", (
    State = state(
        frame(0),
        objects([]),
        attrs(t),
        status(playing),
        next_id(1)
    ),
    expect_exception(
        validate(State, state_schema)
    )
)).

test("game_state_validation: wrong functor throws", (
    empty_assoc(EmptyActionStore),
    State = not_state(
        frame(0),
        objects([]),
        attrs(t),
        status(playing),
        next_id(1),
        commands([]),
        actionstore(EmptyActionStore)
    ),
    expect_exception(
        validate(State, state_schema)
    )
)).

test("object_validation: wrong arity throws", (
    Obj = object(
        id(0), actions([]), extra_field(123)
    ),
    expect_exception(
        validate(Obj, object_schema)
    )
)).

test("object_validation: wrong functor throws", (
    Obj = not_object(
        id(0),
        actions([])
    ),
    expect_exception(
        validate(Obj, object_schema)
    )
)).

test("spawn_request_validation: wrong arity throws", (
    Cmd = spawn_request(enemy, pos(0, 0)),
    expect_exception(
        validate(Cmd, spawn_request_schema)
    )
)).

test("spawn_request_validation: wrong functor throws", (
    Cmd = not_spawn_request(enemy, pos(0, 0), []),
    expect_exception(
        validate(Cmd, spawn_request_schema)
    )
)).

test("state_change_validation: wrong structure throws", (
    Cmd = not_state_change(game_over(won)),
    expect_exception(
        validate(Cmd, state_change_schema)
    )
)).

test("pos_validation: wrong arity throws", (
    Pos = pos(0),
    expect_exception(
        validate(Pos, pos_schema)
    )
)).

test("pos_validation: wrong functor throws", (
    Pos = not_pos(0, 0),
    expect_exception(
        validate(Pos, pos_schema)
    )
)).

test("action_validation: wrong structure throws", (
    Action = not_an_action(1, 2, 3),
    expect_exception(
        validate(Action, action_schema)
    )
)).

test("action_validation: move_to wrong arity throws", (
    Action = move_to(10, 20),
    expect_exception(
        validate(Action, action_schema)
    )
)).

test("action_validation: spawn wrong arity throws", (
    Action = spawn(enemy, pos(0, 0)),
    expect_exception(
        validate(Action, action_schema)
    )
)).

test("action_validation: despawn", (
    validate(despawn, action_schema)
)).


test("action_validation: spawn with unbound variables \
passes", (
    Action = spawn(Type, Pos, Actions),
    validate(Action, action_schema),
    var(Type),
    var(Pos),
    var(Actions)
)).

test("action_validation: loop with unbound actions \
passes", (
    Action = loop(Actions),
    validate(Action, action_schema),
    var(Actions)
)).

test("action_validation: trigger_state_change with \
unbound change passes", (
    Action = trigger_state_change(Change),
    validate(Action, action_schema),
    var(Change)
)).

test("action_validation: wait with unbound N passes", (
    Action = wait(N),
    validate(Action, action_schema),
    var(N)
)).

test("action_validation: move_to with unbound coordinates \
passes", (
    Action = move_to(X, Y, Frames),
    validate(Action, action_schema),
    var(X),
    var(Y),
    var(Frames)
)).

test("action_validation: spawn with partially bound \
variables passes", (
    Action = spawn(enemy, Pos, Actions),
    validate(Action, action_schema),
    var(Pos),
    var(Actions)
)).
