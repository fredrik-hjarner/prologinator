:- module(execute_action_fwd_test, []).
:- use_module('./execute_action', [execute_action/5]).
:- use_module('./types/accessors').
:- use_module('./types/adv_accessors').
:- use_module('./types/constructors').
:- use_module('./util/util', [err_write/1, err_format/2]).
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
        type(static),
        actions([move_to(10, 20, 3)]),
        collisions([])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 0), attr(y, 0)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(Action),
        obj_old(ObjIn),
        obj_new(ObjOut)
    ),
    ctx_cmds(CtxNew, Commands),
    ctx_revhints(CtxNew, RevHints),
    ctx_attr_val(CtxNew, 1/x, NewX),
    ctx_attr_val(CtxNew, 1/y, NewY),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ( ObjOut = [object(
        id(1),
        type(static),
        actions([move_to(10, 20, 2)|_]),
        collisions([])
    )] ; err_write("ObjOut mismatch") ),
    (NewX = 3 ; err_write("NewX != 3")),
    (NewY = 6 ; err_write("NewY != 6")),
    (Commands = [] ; err_write("Commands != []")),
    (RevHints = [] ; err_write("RevHints != []"))
)).

test("move_to: negative direction, multiple frames \
remaining", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    Action = move_to(0, 0, 3),
    ObjIn = object(
        id(1),
        type(static),
        actions([move_to(0, 0, 3)]),
        collisions([])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 10), attr(y, 20)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(Action),
        obj_old(ObjIn),
        obj_new(ObjOut)
    ),
    ctx_cmds(CtxNew, Commands),
    ctx_revhints(CtxNew, RevHints),
    ctx_attr_val(CtxNew, 1/x, NewX),
    ctx_attr_val(CtxNew, 1/y, NewY),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ObjOut = [object(
        id(1),
        type(static),
        actions([move_to(0, 0, 2)|_]),
        collisions([])
    )],
    (NewX = 7 ; err_write("NewX != 7")),
    (NewY = 14 ; err_write("NewY != 14")),
    (Commands = [] ; err_write("Commands != []")),
    (RevHints = [] ; err_write("RevHints != []"))
)).

test("move_to: single frame, arrives at target", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    Action = move_to(5, 5, 1),
    ObjIn = object(
        id(1),
        type(static),
        actions([move_to(5, 5, 1)]),
        collisions([])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 0), attr(y, 0)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(Action),
        obj_old(ObjIn),
        obj_new(ObjOut)
    ),
    ctx_cmds(CtxNew, Commands),
    ctx_revhints(CtxNew, RevHints),
    ctx_attr_val(CtxNew, 1/x, X),
    ctx_attr_val(CtxNew, 1/y, Y),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ObjOut = [object(
        id(1),
        type(static),
        actions([]),
        collisions([])
    )],
    (X = 5 ; err_write("X != 5")),
    (Y = 5 ; err_write("Y != 5")),
    (Commands = [] ; err_write("Commands != []")),
    (RevHints = [] ; err_write("RevHints != []"))
)).

test("move_to: already at target, stays at position and \
continues with remaining frames", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    Action = move_to(10, 20, 3),
    ObjIn = object(
        id(1),
        type(static),
        actions([move_to(10, 20, 3)]),
        collisions([])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 10), attr(y, 20)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(Action),
        obj_old(ObjIn),
        obj_new(ObjOut)
    ),
    ctx_cmds(CtxNew, Commands),
    ctx_revhints(CtxNew, RevHints),
    ctx_attr_val(CtxNew, 1/x, X),
    ctx_attr_val(CtxNew, 1/y, Y),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ObjOut = [object(
        id(1),
        type(static),
        actions([move_to(10, 20, 2)|_]),
        collisions([])
    )],
    (X = 10 ; err_write("X != 10")),
    (Y = 20 ; err_write("Y != 20")),
    (Commands = [] ; err_write("Commands != []")),
    (RevHints = [] ; err_write("RevHints != []"))
)).

test("move_to: negative target coordinates", (
    Action = move_to(-5, -10, 2),
    ObjIn = object(
        id(1),
        type(static),
        actions([move_to(-5, -10, 2)]),
        collisions([])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 0), attr(y, 0)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(Action),
        obj_old(ObjIn),
        obj_new(ObjOut)
    ),
    ctx_cmds(CtxNew, Commands),
    ctx_revhints(CtxNew, RevHints),
    ctx_attr_val(CtxNew, 1/x, NewX),
    ctx_attr_val(CtxNew, 1/y, NewY),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ObjOut = [object(
        id(1),
        type(static),
        actions([move_to(-5, -10, 1)|_]),
        collisions([])
    )],
    (NewX = -2 ; err_write("NewX != -2")),
    (NewY = -5 ; err_write("NewY != -5")),
    (Commands = [] ; err_write("Commands != []")),
    (RevHints = [] ; err_write("RevHints != []"))
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
        type(static),
        actions([trigger_state_change(game_over(won))]),
        collisions([])
    ),
    empty_ctx(CtxIn),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        ctx_old(CtxIn),
        ctx_new(CtxOut),
        action(Action),
        obj_old(ObjIn),
        obj_new(ObjOut)
    ),
    ctx_cmds(CtxOut, Commands),
    ctx_revhints(CtxOut, RevHints),
    ctx_status(CtxOut, Status),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ObjOut = [object(
        id(1),
        type(static),
        actions([]),
        collisions([])
    )],
    (Status = won ; err_write("Status != won")),
    (Commands = [] ; err_write("Commands != []")),
    (RevHints = [] ; err_write("RevHints != []"))
)).

test("trigger_state_change: forward test - updates status \
to lost", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    Action = trigger_state_change(game_over(lost)),
    ObjIn = object(
        id(1),
        type(static),
        actions([trigger_state_change(game_over(lost))]),
        collisions([])
    ),
    empty_ctx(CtxIn),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        ctx_old(CtxIn),
        ctx_new(CtxOut),
        action(Action),
        obj_old(ObjIn),
        obj_new(ObjOut)
    ),
    ctx_cmds(CtxOut, Commands),
    ctx_revhints(CtxOut, RevHints),
    ctx_status(CtxOut, Status),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ObjOut = [object(
        id(1),
        type(static),
        actions([]),
        collisions([])
    )],
    (Status = lost ; err_write("Status != lost")),
    (Commands = [] ; err_write("Commands != []")),
    (RevHints = [] ; err_write("RevHints != []"))
)).

test("trigger_state_change: forward test - won cannot \
override lost", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    Action = trigger_state_change(game_over(won)),
    ObjIn = object(
        id(1),
        type(static),
        actions([trigger_state_change(game_over(won))]),
        collisions([])
    ),
    empty_ctx(CtxTemp),
    ctx_status_ctx(CtxTemp, lost, CtxIn),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        ctx_old(CtxIn),
        ctx_new(CtxOut),
        action(Action),
        obj_old(ObjIn),
        obj_new(ObjOut)
    ),
    ctx_cmds(CtxOut, Commands),
    ctx_revhints(CtxOut, RevHints),
    ctx_status(CtxOut, Status),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ObjOut = [object(
        id(1),
        type(static),
        actions([]),
        collisions([])
    )],
    (Status = lost ; err_write("Status != lost")),
    (Commands = [] ; err_write("Commands != []")),
    (RevHints = [] ; err_write("RevHints != []"))
)).

% ==========================================================
% Tests: noop
% ==========================================================

test("noop: removes self from action queue", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ObjIn = object(
        id(0), type(static),
        actions([noop, wait(1)]), collisions([])
    ),
    empty_ctx(Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(noop),
        obj_old(ObjIn),
        obj_new([ObjOut])
    ),
    ctx_revhints(CtxNew, RevHints),
    ctx_cmds(CtxNew, Commands),
    ctx_frame(CtxNew, Frame),
    ctx_status(CtxNew, Status),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ObjOut = object(
        id(0), type(static),
        actions([wait(1)]), collisions([])
    ),
    (RevHints = [] ; err_write("RevHints != []")),
    (Commands = [] ; err_write("Commands != []")),
    (Frame = 0 ; err_write("Frame != 0")),
    (Status = playing ; err_write("Status != playing"))
)).

% ==========================================================
% Tests: list
% ==========================================================

test("list: expands actions into queue", (
    ObjIn = object(
        id(0), type(static),
        actions([
            list([wait(1), move_to(5, 5, 2)]),
            wait(3)
        ]),
        collisions([])
    ),
    empty_ctx(Ctx),
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(list([wait(1), move_to(5, 5, 2)])),
        obj_old(ObjIn),
        obj_new([ObjOut])
    ),
    ObjOut = object(
        id(0), type(static),
        actions([wait(1), move_to(5, 5, 2), wait(3)]),
        collisions([])
    ),
    ctx_revhints(CtxNew, []),
    ctx_cmds(CtxNew, []),
    ctx_frame(CtxNew, 0),
    ctx_status(CtxNew, playing)
)).

test("list: empty list removes itself", (
    ObjIn = object(
        id(0), type(static),
        actions([list([]), wait(1)]),
        collisions([])
    ),
    empty_ctx(Ctx),
    execute_action(
        ctx_old(Ctx),
        ctx_new(_),
        action(list([])),
        obj_old(ObjIn),
        obj_new([ObjOut])
    ),
    ObjOut = object(
        id(0), type(static),
        actions([wait(1)]),
        collisions([])
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
        type(static),
        actions([
            despawn,
            wait(5),
            move_to(10, 10, 3),
            set_attr(hp, 100)
        ]),
        collisions([])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 0), attr(y, 0)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(despawn),
        obj_old(ObjIn),
        obj_new(ObjOut)
    ),
    ctx_revhints(CtxNew, RevHints),
    ctx_cmds(CtxNew, Commands),
    ctx_frame(CtxNew, Frame),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (ObjOut = [] ; err_write("ObjOut != []")),
    (RevHints = [despawned(1, [attr(x, 0), attr(y, 0)])] ;
     err_write("RevHints wrong")),
    (Commands = [] ; err_write("Commands != []")),
    (Frame = 0 ; err_write("Frame != 0"))
)).

test("despawn: prevents game_over state change from \
executing after despawn", (
    ObjIn = object(
        id(1),
        type(static),
        actions([
            despawn,
            trigger_state_change(game_over(won)),
            wait(5)
        ]),
        collisions([])
    ),
    empty_ctx(Ctx),
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(despawn),
        obj_old(ObjIn),
        obj_new(ObjOut)
    ),
    % Object must be despawned (empty list)
    ObjOut = [],
    % Despawn hint must be recorded
    ctx_revhints(CtxNew, [despawned(1, [])]),
    % Status must remain playing (game_over did NOT execute)
    ctx_status(CtxNew, playing),
    % No commands
    ctx_cmds(CtxNew, [])
)).

% ==========================================================
% Tests: set_attr
% ==========================================================

test("set_attr: set new attribute", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ObjIn = object(
        id(1), type(enemy),
        actions([set_attr(hp, 100)]), collisions([])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 5), attr(y, 10)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(set_attr(hp, 100)),
        obj_old(ObjIn),
        obj_new([ObjOut])
    ),
    ctx_attr_val(CtxNew, 1/hp, HP),
    ctx_attr_val(CtxNew, 1/x, X),
    ctx_attr_val(CtxNew, 1/y, Y),
    ctx_revhints(CtxNew, RevHints),
    ctx_cmds(CtxNew, Commands),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ObjOut = object(
        id(1), type(enemy),
        actions([]), collisions([])
    ),
    (HP = 100 ; err_write("HP != 100")),
    (X = 5 ; err_write("X != 5")),
    (Y = 10 ; err_write("Y != 10")),
    (RevHints = [] ; err_write("RevHints != []")),
    (Commands = [] ; err_write("Commands != []"))
)).

test("set_attr: replace existing attribute", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ObjIn = object(
        id(1), type(enemy),
        actions([set_attr(hp, 50)]), collisions([])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(hp, 100), attr(x, 5), attr(y, 10)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(set_attr(hp, 50)),
        obj_old(ObjIn),
        obj_new([ObjOut])
    ),
    ctx_attr_val(CtxNew, 1/hp, HP),
    ctx_attr_val(CtxNew, 1/x, X),
    ctx_attr_val(CtxNew, 1/y, Y),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ObjOut = object(
        id(1), type(enemy),
        actions([]), collisions([])
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
        id(0), type(tower),
        actions([
            parallel_race([
                wait(5),
                noop,
                wait(10)
            ]),
            wait(3)
        ]),
        collisions([])
    ),
    empty_ctx(CtxTemp),
    ctx_objs_ctx(CtxTemp, [Obj], Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(_),
        action(parallel_race(
            [wait(5), noop, wait(10)]
        )),
        obj_old(Obj),
        obj_new([NewObj])
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
        id(0), type(tower),
        actions([
            parallel_race([
                wait(5), wait(10)
            ]),
            wait(3)
        ]),
        collisions([])
    ),
    empty_ctx(CtxTemp),
    ctx_objs_ctx(CtxTemp, [Obj], Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(_),
        action(parallel_race(
            [wait(5), wait(10)]
        )),
        obj_old(Obj),
        obj_new([NewObj])
    ),
    obj_acns(NewObj, Actions),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (Actions = [parallel_race_running([wait(4), wait(9)])|_]
     ;
     err_write("Actions wrong"))
)).

test("parallel_race: despawns parent when child \
despawns", (
    Obj = object(
        id(1),
        type(static),
        actions([
            parallel_race([
                despawn,
                wait(10)
            ]),
            trigger_state_change(game_over(won)),
            wait(5)
        ]),
        collisions([])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 0), attr(y, 0)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(parallel_race([despawn, wait(10)])),
        obj_old(Obj),
        obj_new(ObjOut)
    ),
    ctx_revhints(CtxNew, RevHints),
    ctx_status(CtxNew, Status),
    ctx_cmds(CtxNew, Commands),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (ObjOut = [] ; err_write("ObjOut != []")),
    (RevHints = [despawned(1, [attr(x, 0), attr(y, 0)])] ;
     err_write("RevHints wrong")),
    (Status = playing ; err_write("Status != playing")),
    (Commands = [] ; err_write("Commands != []"))
)).

test("set_attr: multiple sets overwrite, no \
duplicates", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ObjIn = object(
        id(1), type(enemy),
        actions([
            set_attr(hp, 100),
            set_attr(hp, 75),
            set_attr(hp, 50)
        ]), collisions([])
    ),
    empty_ctx(Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(Ctx1),
        action(set_attr(hp, 100)),
        obj_old(ObjIn),
        obj_new([Obj1])
    ),
    execute_action(
        ctx_old(Ctx1),
        ctx_new(Ctx2),
        action(set_attr(hp, 75)),
        obj_old(Obj1),
        obj_new([Obj2])
    ),
    execute_action(
        ctx_old(Ctx2),
        ctx_new(CtxNew),
        action(set_attr(hp, 50)),
        obj_old(Obj2),
        obj_new([_ObjOut])
    ),
    ctx_attr_val(CtxNew, 1/hp, HP),
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
        type(static),
        actions([
            repeat(3, [noop, set_attr(count, 1)]),
            despawn
        ]),
        collisions([])
    ),
    empty_ctx(Ctx),
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(repeat(3, [noop, set_attr(count, 1)])),
        obj_old(ObjIn),
        obj_new([ObjOut])
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
    ctx_revhints(CtxNew, []),
    ctx_cmds(CtxNew, [])
)).

test("repeat: last repetition doesn't add repeat", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ObjIn = object(
        id(1),
        type(static),
        actions([repeat(1, [noop]), despawn]),
        collisions([])
    ),
    empty_ctx(Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(repeat(1, [noop])),
        obj_old(ObjIn),
        obj_new([ObjOut])
    ),
    obj_acns(ObjOut, Actions),
    ctx_revhints(CtxNew, RevHints),
    ctx_cmds(CtxNew, Commands),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (Actions = [noop, despawn]
     ;
     err_write("Actions wrong")),
    (RevHints = [] ; err_write("RevHints != []")),
    (Commands = [] ; err_write("Commands != []"))
)).

test("repeat: multiple actions in repeat list", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ObjIn = object(
        id(1),
        type(static),
        actions([
            repeat(2, [
                noop,
                set_attr(a, 1),
                set_attr(b, 2)
            ]),
            despawn
        ]),
        collisions([])
    ),
    empty_ctx(Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(repeat(2, [
            noop,
            set_attr(a, 1),
            set_attr(b, 2)
        ])),
        obj_old(ObjIn),
        obj_new([ObjOut])
    ),
    obj_acns(ObjOut, Actions),
    ctx_revhints(CtxNew, RevHints),
    ctx_cmds(CtxNew, Commands),
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
    (RevHints = [] ; err_write("RevHints != []")),
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
        type(static),
        actions([move_delta(1, 5, -3)]),
        collisions([])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 10), attr(y, 20)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(move_delta(1, 5, -3)),
        obj_old(ObjIn),
        obj_new([ObjOut])
    ),
    ctx_attr_val(CtxNew, 1/x, X),
    ctx_attr_val(CtxNew, 1/y, Y),
    ctx_revhints(CtxNew, RevHints),
    ctx_cmds(CtxNew, Commands),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ObjOut = object(
        id(1),
        type(static),
        actions([]),
        collisions([])
    ),
    (X = 15 ; err_write("X != 15")),
    (Y = 17 ; err_write("Y != 17")),
    (RevHints = [] ; err_write("RevHints != []")),
    (Commands = [] ; err_write("Commands != []"))
)).

test("move_delta: multiple frames continues", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ObjIn = object(
        id(1),
        type(static),
        actions([move_delta(3, 10, 5)]),
        collisions([])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 0), attr(y, 0)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(move_delta(3, 10, 5)),
        obj_old(ObjIn),
        obj_new([ObjOut])
    ),
    ctx_attr_val(CtxNew, 1/x, X),
    ctx_attr_val(CtxNew, 1/y, Y),
    obj_acns(ObjOut, Actions),
    ctx_revhints(CtxNew, RevHints),
    ctx_cmds(CtxNew, Commands),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (X = 10 ; err_write("X != 10")),
    (Y = 5 ; err_write("Y != 5")),
    (Actions = [move_delta(2, 10, 5)]
     ;
     err_write("Actions wrong")),
    (RevHints = [] ; err_write("RevHints != []")),
    (Commands = [] ; err_write("Commands != []"))
)).

test("move_delta: negative deltas work", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ObjIn = object(
        id(1),
        type(static),
        actions([move_delta(2, -10, -5)]),
        collisions([])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 50), attr(y, 50)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(move_delta(2, -10, -5)),
        obj_old(ObjIn),
        obj_new([ObjOut])
    ),
    ctx_attr_val(CtxNew, 1/x, X),
    ctx_attr_val(CtxNew, 1/y, Y),
    obj_acns(ObjOut, Actions),
    ctx_revhints(CtxNew, RevHints),
    ctx_cmds(CtxNew, Commands),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (X = 40 ; err_write("X != 40")),
    (Y = 45 ; err_write("Y != 45")),
    (Actions = [move_delta(1, -10, -5)]
     ;
     err_write("Actions wrong")),
    (RevHints = [] ; err_write("RevHints != []")),
    (Commands = [] ; err_write("Commands != []"))
)).

test("move_delta: preserves other attributes", (
    ObjIn = object(
        id(1),
        type(static),
        actions([move_delta(1, 5, -3)]),
        collisions([])
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
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(move_delta(1, 5, -3)),
        obj_old(ObjIn),
        obj_new([ObjOut])
    ),
    ctx_attr_val(CtxNew, 1/x, X),
    ctx_attr_val(CtxNew, 1/y, Y),
    ctx_attr_val(CtxNew, 1/hp, HP),
    ctx_attr_val(CtxNew, 1/speed, Speed),
    ctx_revhints(CtxNew, RevHints),
    ctx_cmds(CtxNew, Commands),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ObjOut = object(
        id(1),
        type(static),
        actions([]),
        collisions([])
    ),
    (X = 15 ; err_write("X != 15")),
    (Y = 17 ; err_write("Y != 17")),
    (HP = 100 ; err_write("HP != 100")),
    (Speed = 5 ; err_write("Speed != 5")),
    (RevHints = [] ; err_write("RevHints != []")),
    (Commands = [] ; err_write("Commands != []"))
)).

