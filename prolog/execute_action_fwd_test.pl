:- module(execute_action_fwd_test, []).

:- use_module('../build/prologinator').
:- use_module('../prolog/util/test_util').

:- use_module(library(lists), [member/2]).
:- use_module(library(assoc), [
    empty_assoc/1,
    put_assoc/4
]).
:- use_module(library(format)).
% ==========================================================
% Forward Tests (all inputs ground, normal use case)
% ==========================================================

% --------------------------------------------------------
% Tests: move_to
% ----------------------------------------------------------

test("move_to: positive direction, multiple frames \
remaining", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    Action = move_to(10, 20, 3),
    ObjIn = object(
        id(1),
        actions([move_to(10, 20, 3)])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(type, static), attr(x, 0), attr(y, 0)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(Action),
        obj_old(ObjIn),
        result(_, ObjOut),
        Ctx,
        CtxNew
    ),
    ctx_cmds(Commands, CtxNew, CtxNew),
    ctx_attr_val(1/x, NewX, CtxNew, CtxNew),
    ctx_attr_val(1/y, NewY, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ( ObjOut = object(
        id(1),
        actions([move_to(10, 20, 2)|_])
    ) ; err_write("ObjOut mismatch") ),
    (NewX = 3 ; err_write("NewX != 3")),
    (NewY = 6 ; err_write("NewY != 6")),
    (Commands = [] ; err_write("Commands != []"))
)).

test("move_to: negative direction, multiple frames \
remaining", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    Action = move_to(0, 0, 3),
    ObjIn = object(
        id(1),
        actions([move_to(0, 0, 3)])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 10), attr(y, 20)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(Action),
        obj_old(ObjIn),
        result(_, ObjOut),
        Ctx,
        CtxNew
    ),
    ctx_cmds(Commands, CtxNew, CtxNew),
    ctx_attr_val(1/x, NewX, CtxNew, CtxNew),
    ctx_attr_val(1/y, NewY, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ObjOut = object(
        id(1),
        actions([move_to(0, 0, 2)|_])
    ),
    (NewX = 7 ; err_write("NewX != 7")),
    (NewY = 14 ; err_write("NewY != 14")),
    (Commands = [] ; err_write("Commands != []"))
)).

test("move_to: single frame, arrives at target", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    Action = move_to(5, 5, 1),
    ObjIn = object(
        id(1),
        actions([move_to(5, 5, 1)])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(type, static), attr(x, 0), attr(y, 0)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(Action),
        obj_old(ObjIn),
        result(_, ObjOut),
        Ctx,
        CtxNew
    ),
    ctx_cmds(Commands, CtxNew, CtxNew),
    ctx_attr_val(1/x, X, CtxNew, CtxNew),
    ctx_attr_val(1/y, Y, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ObjOut = object(
        id(1),
        actions([])
    ),
    (X = 5 ; err_write("X != 5")),
    (Y = 5 ; err_write("Y != 5")),
    (Commands = [] ; err_write("Commands != []"))
)).

test("move_to: already at target, stays at position and \
continues with remaining frames", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    Action = move_to(10, 20, 3),
    ObjIn = object(
        id(1),
        actions([move_to(10, 20, 3)])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 10), attr(y, 20)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(Action),
        obj_old(ObjIn),
        result(_, ObjOut),
        Ctx,
        CtxNew
    ),
    ctx_cmds(Commands, CtxNew, CtxNew),
    ctx_attr_val(1/x, X, CtxNew, CtxNew),
    ctx_attr_val(1/y, Y, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ObjOut = object(
        id(1),
        actions([move_to(10, 20, 2)|_])
    ),
    (X = 10 ; err_write("X != 10")),
    (Y = 20 ; err_write("Y != 20")),
    (Commands = [] ; err_write("Commands != []"))
)).

test("move_to: negative target coordinates", (
    Action = move_to(-5, -10, 2),
    ObjIn = object(
        id(1),
        actions([move_to(-5, -10, 2)])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(type, static), attr(x, 0), attr(y, 0)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    execute_action(
        action(Action),
        obj_old(ObjIn),
        result(_, ObjOut),
        Ctx,
        CtxNew
    ),
    ctx_cmds(Commands, CtxNew, CtxNew),
    ctx_attr_val(1/x, NewX, CtxNew, CtxNew),
    ctx_attr_val(1/y, NewY, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ObjOut = object(
        id(1),
        actions([move_to(-5, -10, 1)|_])
    ),
    (NewX = -2 ; err_write("NewX != -2")),
    (NewY = -5 ; err_write("NewY != -5")),
    (Commands = [] ; err_write("Commands != []"))
)).

% --------------------------------------------------------
% Tests: trigger_state_change
% --------------------------------------------------------

test("trigger_state_change: forward test - updates status \
to won", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    Action = trigger_state_change(game_over(won)),
    ObjIn = object(
        id(1),
        actions([trigger_state_change(game_over(won))])
    ),
    empty_ctx(CtxIn),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(Action),
        obj_old(ObjIn),
        result(_, ObjOut),
        CtxIn,
        CtxOut
    ),
    ctx_cmds(Commands, CtxOut, CtxOut),
    ctx_status(Status, CtxOut, CtxOut),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ObjOut = object(
        id(1),
        actions([])
    ),
    (Status = won ; err_write("Status != won")),
    (Commands = [] ; err_write("Commands != []"))
)).

test("trigger_state_change: forward test - updates status \
to lost", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    Action = trigger_state_change(game_over(lost)),
    ObjIn = object(
        id(1),
        actions([trigger_state_change(game_over(lost))])
    ),
    empty_ctx(CtxIn),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(Action),
        obj_old(ObjIn),
        result(_, ObjOut),
        CtxIn,
        CtxOut
    ),
    ctx_cmds(Commands, CtxOut, CtxOut),
    ctx_status(Status, CtxOut, CtxOut),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ObjOut = object(
        id(1),
        actions([])
    ),
    (Status = lost ; err_write("Status != lost")),
    (Commands = [] ; err_write("Commands != []"))
)).

test("trigger_state_change: forward test - won cannot \
override lost", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    Action = trigger_state_change(game_over(won)),
    ObjIn = object(
        id(1),
        actions([trigger_state_change(game_over(won))])
    ),
    empty_ctx(CtxTemp),
    ctx_set_status(lost, CtxTemp, CtxIn),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(Action),
        obj_old(ObjIn),
        result(_, ObjOut),
        CtxIn,
        CtxOut
    ),
    ctx_cmds(Commands, CtxOut, CtxOut),
    ctx_status(Status, CtxOut, CtxOut),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ObjOut = object(
        id(1),
        actions([])
    ),
    (Status = lost ; err_write("Status != lost")),
    (Commands = [] ; err_write("Commands != []"))
)).

% ==========================================================
% Tests: noop
% ==========================================================

test("noop: removes self from action queue", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ObjIn = object(
        id(0),
        actions([noop, wait(1)])
    ),
    empty_ctx(Ctx0),
    ctx_set_attr_val(0/type, static, Ctx0, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(noop),
        obj_old(ObjIn),
        result(completed, ObjOut),
        Ctx,
        CtxNew
    ),
    ctx_cmds(Commands, CtxNew, CtxNew),
    ctx_frame(Frame, CtxNew, CtxNew),
    ctx_status(Status, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ObjOut = object(
        id(0),
        actions([wait(1)])
    ),
    (Commands = [] ; err_write("Commands != []")),
    (Frame = 0 ; err_write("Frame != 0")),
    (Status = playing ; err_write("Status != playing"))
)).

% ==========================================================
% Tests: list
% ==========================================================

test("list: executes actions immediately", (
    ObjIn = object(
        id(0),
        actions([
            list([wait(1), move_to(5, 5, 2)]),
            wait(3)
        ])
    ),
    empty_ctx(Ctx0),
    ctx_set_attr_val(0/type, static, Ctx0, Ctx),
    execute_action(
        action(list([wait(1), move_to(5, 5, 2)])),
        obj_old(ObjIn),
        result(Status, ObjOut),
        Ctx,
        CtxNew
    ),
    % list now executes immediately, wait(1) yields
    Status = yielded,
    ObjOut = object(
        id(0),
        actions([list([move_to(5, 5, 2)]), wait(3)])
    ),
    ctx_cmds([], CtxNew, CtxNew),
    ctx_frame(0, CtxNew, CtxNew),
    ctx_status(playing, CtxNew, CtxNew)
)).

test("list: empty list removes itself", (
    ObjIn = object(
        id(0),
        actions([list([]), wait(1)])
    ),
    empty_ctx(Ctx0),
    ctx_set_attr_val(0/type, static, Ctx0, Ctx),
    execute_action(
        action(list([])),
        obj_old(ObjIn),
        result(completed, ObjOut),
        Ctx,
        _
    ),
    ObjOut = object(
        id(0),
        actions([wait(1)])
    )
)).

% ==========================================================
% Tests: despawn
% ==========================================================

test("despawn: despawns object and prevents remaining \
actions from executing", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ObjIn = object(
        id(1),
        actions([
            despawn,
            wait(5),
            move_to(10, 10, 3),
            set_attr(hp, 100)
        ])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(type, static), attr(x, 0), attr(y, 0)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(despawn),
        obj_old(ObjIn),
        result(_, ObjOut),
        Ctx,
        CtxNew
    ),
    ctx_cmds(Commands, CtxNew, CtxNew),
    ctx_frame(Frame, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (ObjOut = [] ; err_write("ObjOut != []")),
    (Commands = [] ; err_write("Commands != []")),
    (Frame = 0 ; err_write("Frame != 0"))
)).

test("despawn: prevents game_over state change from \
executing after despawn", (
    ObjIn = object(
        id(1),
        actions([
            despawn,
            trigger_state_change(game_over(won)),
            wait(5)
        ])
    ),
    empty_ctx(Ctx0),
    ctx_set_attr_val(0/type, static, Ctx0, Ctx),
    execute_action(
        action(despawn),
        obj_old(ObjIn),
        result(_, ObjOut),
        Ctx,
        CtxNew
    ),
    % Object must be despawned (empty list)
    ObjOut = [],
    % Despawn hint must be recorded
    % Status must remain playing (game_over did NOT execute)
    ctx_status(playing, CtxNew, CtxNew),
    % No commands
    ctx_cmds([], CtxNew, CtxNew)
)).

% ==========================================================
% Tests: set_attr
% ==========================================================

test("set_attr: set new attribute", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ObjIn = object(
        id(1),
        actions([set_attr(hp, 100)])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 5), attr(y, 10)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(set_attr(hp, 100)),
        obj_old(ObjIn),
        result(completed, ObjOut),
        Ctx,
        CtxNew
    ),
    ctx_attr_val(1/hp, HP, CtxNew, CtxNew),
    ctx_attr_val(1/x, X, CtxNew, CtxNew),
    ctx_attr_val(1/y, Y, CtxNew, CtxNew),
    ctx_cmds(Commands, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ObjOut = object(
        id(1),
        actions([])
    ),
    (HP = 100 ; err_write("HP != 100")),
    (X = 5 ; err_write("X != 5")),
    (Y = 10 ; err_write("Y != 10")),
    (Commands = [] ; err_write("Commands != []"))
)).

test("set_attr: replace existing attribute", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ObjIn = object(
        id(1),
        actions([set_attr(hp, 50)])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(type, static),
               attr(hp, 100), attr(x, 5),
               attr(y, 10)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(set_attr(hp, 50)),
        obj_old(ObjIn),
        result(completed, ObjOut),
        Ctx,
        CtxNew
    ),
    ctx_attr_val(1/hp, HP, CtxNew, CtxNew),
    ctx_attr_val(1/x, X, CtxNew, CtxNew),
    ctx_attr_val(1/y, Y, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ObjOut = object(
        id(1),
        actions([])
    ),
    (HP = 50 ; err_write("HP != 50")),
    (X = 5 ; err_write("X != 5")),
    (Y = 10 ; err_write("Y != 10"))
)).

% ==========================================================
% Tests: parallel_race
% ==========================================================

test("parallel_race: stops on child completion", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    Obj = object(
        id(0),
        actions([
            parallel_race([
                wait(5),
                noop,
                wait(10)
            ]),
            wait(3)
        ])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(0, EmptyAttrs0, [attr(type, static)],
              EmptyAttrs),
    ctx_with_objs([Obj], Ctx0),
    ctx_set_attrs(EmptyAttrs, Ctx0, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(parallel_race(
            [wait(5), noop, wait(10)]
        )),
        obj_old(Obj),
        result(completed, NewObj),
        Ctx,
        _
    ),
    obj_acns(NewObj, Actions),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (Actions = [wait(3)|_] ; err_write("Actions wrong"))
)).

test("parallel_race: continues if no child \
done", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    Obj = object(
        id(0),
        actions([
            parallel_race([
                wait(5), wait(10)
            ]),
            wait(3)
        ])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(0, EmptyAttrs0, [attr(type, static)],
              EmptyAttrs),
    ctx_with_objs([Obj], Ctx0),
    ctx_set_attrs(EmptyAttrs, Ctx0, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(parallel_race(
            [wait(5), wait(10)]
        )),
        obj_old(Obj),
        result(Status, NewObj),
        Ctx,
        _
    ),
    obj_acns(NewObj, Actions),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (Status = yielded
     ; err_write("Status != yielded")),
    (Actions = [parallel_race([wait(4), wait(9)])|_]
     ;
     err_write("Actions wrong"))
)).

test("parallel_race: despawns parent when child \
despawns", (
    Obj = object(
        id(1),
        actions([
            parallel_race([
                despawn,
                wait(10)
            ]),
            trigger_state_change(game_over(won)),
            wait(5)
        ])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(type, static), attr(x, 0), attr(y, 0)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    execute_action(
        action(parallel_race([despawn, wait(10)])),
        obj_old(Obj),
        result(_, ObjOut),
        Ctx,
        CtxNew
    ),
    ctx_status(Status, CtxNew, CtxNew),
    ctx_cmds(Commands, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (ObjOut = [] ; err_write("ObjOut != []")),
    (Status = playing ; err_write("Status != playing")),
    (Commands = [] ; err_write("Commands != []"))
)).

test("set_attr: multiple sets overwrite, no \
duplicates", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ObjIn = object(
        id(1),
        actions([
            set_attr(hp, 100),
            set_attr(hp, 75),
            set_attr(hp, 50)
        ])
    ),
    empty_ctx(Ctx0),
    ctx_set_attr_val(0/type, static, Ctx0, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(set_attr(hp, 100)),
        obj_old(ObjIn),
        result(completed, Obj1),
        Ctx,
        Ctx1
    ),
    execute_action(
        action(set_attr(hp, 75)),
        obj_old(Obj1),
        result(completed, Obj2),
        Ctx1,
        Ctx2
    ),
    execute_action(
        action(set_attr(hp, 50)),
        obj_old(Obj2),
        result(completed, _ObjOut),
        Ctx2,
        CtxNew
    ),
    ctx_attr_val(1/hp, HP, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (HP = 50 ; err_write("HP != 50"))
)).

% ==========================================================
% Tests: repeat
% ==========================================================

test("repeat: expands actions once and decrements", (
    ObjIn = object(
        id(1),
        actions([
            repeat(3, [noop, set_attr(count, 1)]),
            despawn
        ])
    ),
    empty_ctx(Ctx0),
    ctx_set_attr_val(0/type, static, Ctx0, Ctx),
    execute_action(
        action(repeat(3, [noop, set_attr(count, 1)])),
        obj_old(ObjIn),
        result(completed, ObjOut),
        Ctx,
        CtxNew
    ),
    % Should expand to: [noop, set_attr(count, 1),
    %   repeat(2, [noop, set_attr(count, 1)]), despawn]
    obj_acns(ObjOut, Actions),
    Actions = [
        noop,
        set_attr(count, 1),
        repeat(2, [noop, set_attr(count, 1)]),
        despawn
    ],
    ctx_cmds([], CtxNew, CtxNew)
)).

test("repeat: last repetition doesn't add repeat", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ObjIn = object(
        id(1),
        actions([repeat(1, [noop]), despawn])
    ),
    empty_ctx(Ctx0),
    ctx_set_attr_val(0/type, static, Ctx0, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(repeat(1, [noop])),
        obj_old(ObjIn),
        result(completed, ObjOut),
        Ctx,
        CtxNew
    ),
    obj_acns(ObjOut, Actions),
    ctx_cmds(Commands, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (Actions = [noop, despawn]
     ;
     err_write("Actions wrong")),
    (Commands = [] ; err_write("Commands != []"))
)).

test("repeat: multiple actions in repeat list", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ObjIn = object(
        id(1),
        actions([
            repeat(2, [
                noop,
                set_attr(a, 1),
                set_attr(b, 2)
            ]),
            despawn
        ])
    ),
    empty_ctx(Ctx0),
    ctx_set_attr_val(0/type, static, Ctx0, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(repeat(2, [
            noop,
            set_attr(a, 1),
            set_attr(b, 2)
        ])),
        obj_old(ObjIn),
        result(completed, ObjOut),
        Ctx,
        CtxNew
    ),
    obj_acns(ObjOut, Actions),
    ctx_cmds(Commands, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (Actions = [
        noop,
        set_attr(a, 1),
        set_attr(b, 2),
        repeat(1, [noop, set_attr(a, 1), set_attr(b, 2)]),
        despawn
    ] ; err_write("Actions wrong")),
    (Commands = [] ; err_write("Commands != []"))
)).

% ==========================================================
% Tests: move_delta
% ==========================================================

test("move_delta: single frame moves and completes", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ObjIn = object(
        id(1),
        actions([move_delta(1, 5, -3)])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 10), attr(y, 20)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(move_delta(1, 5, -3)),
        obj_old(ObjIn),
        result(yielded, ObjOut),
        Ctx,
        CtxNew
    ),
    ctx_attr_val(1/x, X, CtxNew, CtxNew),
    ctx_attr_val(1/y, Y, CtxNew, CtxNew),
    ctx_cmds(Commands, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ObjOut = object(
        id(1),
        actions([])
    ),
    (X = 15 ; err_write("X != 15")),
    (Y = 17 ; err_write("Y != 17")),
    (Commands = [] ; err_write("Commands != []"))
)).

test("move_delta: multiple frames continues", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ObjIn = object(
        id(1),
        actions([move_delta(3, 10, 5)])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(type, static), attr(x, 0), attr(y, 0)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(move_delta(3, 10, 5)),
        obj_old(ObjIn),
        result(yielded, ObjOut),
        Ctx,
        CtxNew
    ),
    ctx_attr_val(1/x, X, CtxNew, CtxNew),
    ctx_attr_val(1/y, Y, CtxNew, CtxNew),
    obj_acns(ObjOut, Actions),
    ctx_cmds(Commands, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (X = 10 ; err_write("X != 10")),
    (Y = 5 ; err_write("Y != 5")),
    (Actions = [move_delta(2, 10, 5)]
     ;
     err_write("Actions wrong")),
    (Commands = [] ; err_write("Commands != []"))
)).

test("move_delta: negative deltas work", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ObjIn = object(
        id(1),
        actions([move_delta(2, -10, -5)])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 50), attr(y, 50)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(move_delta(2, -10, -5)),
        obj_old(ObjIn),
        result(yielded, ObjOut),
        Ctx,
        CtxNew
    ),
    ctx_attr_val(1/x, X, CtxNew, CtxNew),
    ctx_attr_val(1/y, Y, CtxNew, CtxNew),
    obj_acns(ObjOut, Actions),
    ctx_cmds(Commands, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (X = 40 ; err_write("X != 40")),
    (Y = 45 ; err_write("Y != 45")),
    (Actions = [move_delta(1, -10, -5)]
     ;
     err_write("Actions wrong")),
    (Commands = [] ; err_write("Commands != []"))
)).

test("move_delta: preserves other attributes", (
    ObjIn = object(
        id(1),
        actions([move_delta(1, 5, -3)])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(x, 10), attr(y, 20),
               attr(hp, 100), attr(speed, 5)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(move_delta(1, 5, -3)),
        obj_old(ObjIn),
        result(yielded, ObjOut),
        Ctx,
        CtxNew
    ),
    ctx_attr_val(1/x, X, CtxNew, CtxNew),
    ctx_attr_val(1/y, Y, CtxNew, CtxNew),
    ctx_attr_val(1/hp, HP, CtxNew, CtxNew),
    ctx_attr_val(1/speed, Speed, CtxNew, CtxNew),
    ctx_cmds(Commands, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ObjOut = object(
        id(1),
        actions([])
    ),
    (X = 15 ; err_write("X != 15")),
    (Y = 17 ; err_write("Y != 17")),
    (HP = 100 ; err_write("HP != 100")),
    (Speed = 5 ; err_write("Speed != 5")),
    (Commands = [] ; err_write("Commands != []"))
)).

% ==========================================================
% Tests: Value Resolution (attr() syntax)
% ==========================================================

test("value_resolution: move_to with attr() references", (
    % Object moves to position stored in its attributes
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(x, 0), attr(y, 0),
               attr(target_x, 100),
               attr(target_y, 200)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    ObjIn = object(
        id(1),
        actions([move_to(attr(target_x),
                         attr(target_y), 5)])
    ),
    execute_action(
        action(move_to(attr(target_x),
                       attr(target_y), 5)),
        obj_old(ObjIn),
        result(_, _ObjOut),
        Ctx,
        CtxNew
    ),
    % Should start moving toward target
    ctx_attr_val(1/x, NewX, CtxNew, CtxNew),
    ctx_attr_val(1/y, NewY, CtxNew, CtxNew),
    NewX > 0,
    NewY > 0
)).

test("value_resolution: set_attr with attr() source", (
    % Copy one attribute to another
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(x, 50), attr(y, 75),
               attr(source_x, 0)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    ObjIn = object(
        id(1),
        actions([set_attr(source_x, attr(x))])
    ),
    execute_action(
        action(set_attr(source_x, attr(x))),
        obj_old(ObjIn),
        result(_, _ObjOut),
        Ctx,
        CtxNew
    ),
    % source_x should now equal x
    ctx_attr_val(1/source_x, 50, CtxNew, CtxNew),
    ctx_attr_val(1/x, 50, CtxNew, CtxNew)
)).

test("value_resolution: path syntax parent_id/target_y", (
    % Object references another object's attribute via path
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(x, 0), attr(y, 0),
               attr(parent_id, 2)],
              Attrs1),
    put_assoc(2, Attrs1,
              [attr(x, 100), attr(y, 200),
               attr(target_y, 250)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    ObjIn = object(
        id(1),
        actions([set_attr(my_target_y,
                          attr(parent_id/target_y))])
    ),
    execute_action(
        action(set_attr(my_target_y,
                        attr(parent_id/target_y))),
        obj_old(ObjIn),
        result(_, _ObjOut),
        Ctx,
        CtxNew
    ),
    % Should copy parent's target_y (250) to my_target_y
    ctx_attr_val(1/my_target_y, 250, CtxNew, CtxNew),
    ctx_attr_val(2/target_y, 250, CtxNew, CtxNew)
)).

test("value_resolution: multi-hop path a/b/c", (
    % Navigate through multiple object IDs
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(x, 0), attr(y, 0),
               attr(first_id, 2)],
              Attrs1),
    put_assoc(2, Attrs1,
              [attr(x, 10), attr(y, 20),
               attr(second_id, 3)],
              Attrs2),
    put_assoc(3, Attrs2,
              [attr(x, 100), attr(y, 200),
               attr(final_value, 999)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    ObjIn = object(
        id(1),
        actions([set_attr(result,
                          attr(first_id/second_id/
                                final_value))])
    ),
    execute_action(
        action(set_attr(result,
                        attr(first_id/second_id/
                              final_value))),
        obj_old(ObjIn),
        result(_, _ObjOut),
        Ctx,
        CtxNew
    ),
    % Should navigate: 1 -> first_id(2) -> second_id(3) ->
    % final_value(999)
    ctx_attr_val(1/result, 999, CtxNew, CtxNew)
)).

test("value_resolution: spawn at attr() position", (
    % Spawn object at position stored in attributes
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(x, 50), attr(y, 100),
               attr(spawn_x, 200),
               attr(spawn_y, 300)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    ObjIn = object(
        id(1),
        actions([spawn(enemy, attr(spawn_x),
                       attr(spawn_y), [])])
    ),
    execute_action(
        action(spawn(enemy, attr(spawn_x),
                     attr(spawn_y), [])),
        obj_old(ObjIn),
        result(_, _ObjOut),
        Ctx,
        CtxNew
    ),
    % Should spawn enemy at (200, 300) - spawn immediately
    % creates object, so check it exists in context
    ctx_objs(Objects, CtxNew, CtxNew),
    member(object(id(_ID), _), Objects),
    ctx_attr_val(_ID/type, enemy, CtxNew, CtxNew),
    ctx_attr_val(_ID/x, 200, CtxNew, CtxNew),
    ctx_attr_val(_ID/y, 300, CtxNew, CtxNew)
)).

test("value_resolution: mixed plain and attr() values", (
    % Mix plain values with attribute references
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(x, 0), attr(y, 0),
               attr(target_x, 100),
               attr(speed, 5)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    ObjIn = object(
        id(1),
        actions([move_to(attr(target_x), 200,
                         attr(speed))])
    ),
    execute_action(
        action(move_to(attr(target_x), 200,
                       attr(speed))),
        obj_old(ObjIn),
        result(_, _ObjOut),
        Ctx,
        CtxNew
    ),
    % Should move toward target (100, 200) with speed=5
    ctx_attr_val(1/x, NewX, CtxNew, CtxNew),
    ctx_attr_val(1/y, NewY, CtxNew, CtxNew),
    NewX > 0,
    NewY > 0
)).

% Note: list() actions are resolved when the list action
% itself is resolved, but testing this is complex because
% list() doesn't execute actions immediately. The other
% tests verify that attr() resolution works correctly for
% all action types.

test("value_resolution: backward compatible plain values", (
    % Plain values still work without attr() wrapper
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(x, 0), attr(y, 0)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    ObjIn = object(
        id(1),
        actions([move_to(100, 200, 5)])
    ),
    execute_action(
        action(move_to(100, 200, 5)),
        obj_old(ObjIn),
        result(_, _ObjOut),
        Ctx,
        CtxNew
    ),
    % Should work exactly as before
    ctx_attr_val(1/x, NewX, CtxNew, CtxNew),
    ctx_attr_val(1/y, NewY, CtxNew, CtxNew),
    NewX > 0,
    NewY > 0
)).
