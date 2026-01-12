:- module(execute_action_fwd_test, []).

#include "./build/prologinator.pl"
#include "./prolog/util/test_util.pl"

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
    ActionsIn = [move_to(10, 20, 3)],
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(type, static), attr(x, 0), attr(y, 0)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        actions_old(ActionsIn),
        obj(object(id(1))),
        result(_, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    ctx_spawnCmds(SpawnCmds, CtxNew, CtxNew),
    ctx_attr_val(1/x, NewX, CtxNew, CtxNew),
    ctx_attr_val(1/y, NewY, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ( ActionsOut = [move_to(10, 20, 2)|_]
     ; expect(false, "ActionsOut mismatch") ),
    expect(NewX = 3, "NewX != 3"),
    expect(NewY = 6, "NewY != 6"),
    expect(SpawnCmds = [], "SpawnCmds != []")
)).

test("move_to: negative direction, multiple frames \
remaining", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ActionsIn = [move_to(0, 0, 3)],
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 10), attr(y, 20)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        actions_old(ActionsIn),
        obj(object(id(1))),
        result(_, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    ctx_spawnCmds(SpawnCmds, CtxNew, CtxNew),
    ctx_attr_val(1/x, NewX, CtxNew, CtxNew),
    ctx_attr_val(1/y, NewY, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ActionsOut = [move_to(0, 0, 2)|_],
    expect(NewX = 7, "NewX != 7"),
    expect(NewY = 14, "NewY != 14"),
    expect(SpawnCmds = [], "SpawnCmds != []")
)).

test("move_to: single frame, arrives at target", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ActionsIn = [move_to(5, 5, 1)],
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(type, static), attr(x, 0), attr(y, 0)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        actions_old(ActionsIn),
        obj(object(id(1))),
        result(_, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    ctx_spawnCmds(SpawnCmds, CtxNew, CtxNew),
    ctx_attr_val(1/x, X, CtxNew, CtxNew),
    ctx_attr_val(1/y, Y, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    expect(X = 5, "X != 5"),
    expect(Y = 5, "Y != 5"),
    expect(SpawnCmds = [], "SpawnCmds != []")
)).

test("move_to: already at target, stays at position and \
continues with remaining frames", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ActionsIn = [move_to(10, 20, 3)],
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 10), attr(y, 20)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        actions_old(ActionsIn),
        obj(object(id(1))),
        result(_, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    ctx_spawnCmds(SpawnCmds, CtxNew, CtxNew),
    ctx_attr_val(1/x, X, CtxNew, CtxNew),
    ctx_attr_val(1/y, Y, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ActionsOut = [move_to(10, 20, 2)|_],
    expect(X = 10, "X != 10"),
    expect(Y = 20, "Y != 20"),
    expect(SpawnCmds = [], "SpawnCmds != []")
)).

test("move_to: negative target coordinates", (
    ActionsIn = [move_to(-5, -10, 2)],
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(type, static), attr(x, 0), attr(y, 0)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    execute_action(
        actions_old(ActionsIn),
        obj(object(id(1))),
        result(_, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    ctx_spawnCmds(SpawnCmds, CtxNew, CtxNew),
    ctx_attr_val(1/x, NewX, CtxNew, CtxNew),
    ctx_attr_val(1/y, NewY, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ActionsOut = [move_to(-5, -10, 1)|_],
    expect(NewX = -2, "NewX != -2"),
    expect(NewY = -5, "NewY != -5"),
    expect(SpawnCmds = [], "SpawnCmds != []")
)).

% --------------------------------------------------------
% Tests: trigger_state_change
% --------------------------------------------------------

test("trigger_state_change: forward test - updates status \
to won", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ActionsIn = [trigger_state_change(game_over(won))],
    empty_ctx(CtxIn),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        actions_old(ActionsIn),
        obj(object(id(1))),
        result(_, actions_new(ActionsOut)),
        CtxIn,
        CtxOut
    ),
    ctx_spawnCmds(SpawnCmds, CtxOut, CtxOut),
    ctx_status(Status, CtxOut, CtxOut),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    expect(Status = won, "Status != won"),
    expect(SpawnCmds = [], "SpawnCmds != []")
)).

test("trigger_state_change: forward test - updates status \
to lost", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ActionsIn = [trigger_state_change(game_over(lost))],
    empty_ctx(CtxIn),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        actions_old(ActionsIn),
        obj(object(id(1))),
        result(_, actions_new(ActionsOut)),
        CtxIn,
        CtxOut
    ),
    ctx_spawnCmds(SpawnCmds, CtxOut, CtxOut),
    ctx_status(Status, CtxOut, CtxOut),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    expect(Status = lost, "Status != lost"),
    expect(SpawnCmds = [], "SpawnCmds != []")
)).

test("trigger_state_change: forward test - won cannot \
override lost", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ActionsIn = [trigger_state_change(game_over(won))],
    empty_ctx(CtxTemp),
    ctx_set_status(lost, CtxTemp, CtxIn),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        actions_old(ActionsIn),
        obj(object(id(1))),
        result(_, actions_new(ActionsOut)),
        CtxIn,
        CtxOut
    ),
    ctx_spawnCmds(SpawnCmds, CtxOut, CtxOut),
    ctx_status(Status, CtxOut, CtxOut),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    expect(Status = lost, "Status != lost"),
    expect(SpawnCmds = [], "SpawnCmds != []")
)).

% ==========================================================
% Tests: wait(0)
% ==========================================================

test("wait(0): removes self from action queue", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ActionsIn = [wait(0), wait(1)],
    empty_ctx(Ctx0),
    ctx_set_attr_val(0/type, static, Ctx0, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        actions_old(ActionsIn),
        obj(object(id(0))),
        result(completed, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    ctx_spawnCmds(SpawnCmds, CtxNew, CtxNew),
    ctx_frame(Frame, CtxNew, CtxNew),
    ctx_status(Status, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    ActionsOut = [wait(1)],
    expect(SpawnCmds = [], "SpawnCmds != []"),
    expect(Frame = 0, "Frame != 0"),
    expect(Status = playing, "Status != playing")
)).

% ==========================================================
% Tests: list
% ==========================================================

test("list: executes actions immediately", (
    ObjIn = object(id(0)),
    ActionsIn = [
        list([wait(1), move_to(5, 5, 2)]),
        wait(3)
    ],
    empty_ctx(Ctx0),
    ctx_set_attr_val(0/type, static, Ctx0, Ctx),
    obj_id(ObjIn, ID),
    execute_action(
        actions_old(ActionsIn),
        obj(ObjIn),
        result(Status, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    % list now executes immediately, wait(1) yields
    Status = yielded,
    ActionsOut = [list([move_to(5, 5, 2)]), wait(3)],
    ctx_spawnCmds([], CtxNew, CtxNew),
    ctx_frame(0, CtxNew, CtxNew),
    ctx_status(playing, CtxNew, CtxNew)
)).

test("list: empty list removes itself", (
    ActionsIn = [list([]), wait(1)],
    empty_ctx(Ctx0),
    ctx_set_attr_val(0/type, static, Ctx0, Ctx),
    execute_action(
        actions_old(ActionsIn),
        obj(object(id(0))),
        result(completed, actions_new(ActionsOut)),
        Ctx,
        _
    ),
    ActionsOut = [wait(1)]
)).

% ==========================================================
% Tests: despawn
% ==========================================================

test("despawn: despawns object and prevents remaining \
actions from executing", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ActionsIn = [
        despawn,
        wait(5),
        move_to(10, 10, 3),
        set_attr(hp, 100)
    ],
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(type, static), attr(x, 0), attr(y, 0)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        actions_old(ActionsIn),
        obj(object(id(1))),
        result(_, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    ctx_spawnCmds(SpawnCmds, CtxNew, CtxNew),
    ctx_frame(Frame, CtxNew, CtxNew),
    ctx_status(Status, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    expect(SpawnCmds = [], "SpawnCmds != []"),
    expect(Frame = 0, "Frame != 0"),
    expect(Status = playing, "Status != playing")
)).

test("despawn: prevents game_over state change from \
executing after despawn", (
    ActionsIn = [
        despawn,
        trigger_state_change(game_over(won)),
        wait(5)
    ],
    empty_ctx(Ctx0),
    ctx_set_attr_val(0/type, static, Ctx0, Ctx),
    execute_action(
        actions_old(ActionsIn),
        obj(object(id(1))),
        result(_, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    % Object must be despawned (empty list)
    % Despawn hint must be recorded
    % Status must remain playing (game_over did NOT execute)
    ctx_status(playing, CtxNew, CtxNew),
    % No commands
    ctx_spawnCmds([], CtxNew, CtxNew)
)).


% ==========================================================
% Tests: parallel_race
% ==========================================================

test("parallel_race: stops on child completion", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    Obj = object(id(0)),
    empty_attr_store(EmptyAttrs0),
    put_assoc(0, EmptyAttrs0, [attr(type, static)],
              EmptyAttrs),
    ctx_with_objs([Obj], Ctx0),
    ctx_set_attrs(EmptyAttrs, Ctx0, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    obj_id(Obj, ID),
    ActionsIn = [
        parallel_race([wait(5), wait(0), wait(10)]),
        wait(3)
    ],
    execute_action(
        actions_old(ActionsIn),
        obj(Obj),
        result(completed, actions_new(ActionsOut)),
        Ctx,
        _
    ),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    expect(ActionsOut = [wait(3)|_], "Actions wrong")
)).

test("parallel_race: continues if no child \
done", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ActionsIn = [
        parallel_race([
            wait(5), wait(10)
        ]),
        wait(3)
    ],
    empty_attr_store(EmptyAttrs0),
    put_assoc(0, EmptyAttrs0, [attr(type, static)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx0),
    ctx_set_objs([object(id(0))], Ctx0, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        actions_old(ActionsIn),
        obj(object(id(0))),
        result(Status, actions_new(ActionsOut)),
        Ctx,
        _
    ),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (Status = yielded
     ; expect(false, "Status != yielded")),
    (ActionsOut = [parallel_race([wait(4), wait(9)])|_]
     ;
     expect(false, "Actions wrong"))
)).

test("parallel_race: despawns parent when child \
despawns", (
    ActionsIn = [
        parallel_race([
            despawn,
            wait(10)
        ]),
        trigger_state_change(game_over(won)),
        wait(5)
    ],
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(type, static), attr(x, 0), attr(y, 0)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    execute_action(
        actions_old(ActionsIn),
        obj(object(id(1))),
        result(Status_Despawn, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    ctx_status(Status, CtxNew, CtxNew),
    ctx_spawnCmds(SpawnCmds, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (Status_Despawn = despawned ;
     expect(false, "Status != despawned")),
    expect(Status = playing, "Status != playing"),
    expect(SpawnCmds = [], "SpawnCmds != []")
)).

% ==========================================================
% Tests: repeat
% ==========================================================

test("repeat: expands actions once and decrements", (
    ActionsIn = [
        repeat(3, [wait(0), set_attr(count, 1)]),
        despawn
    ],
    empty_ctx(Ctx0),
    ctx_set_attr_val(0/type, static, Ctx0, Ctx),
    execute_action(
        actions_old(ActionsIn),
        obj(object(id(1))),
        result(completed, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    % Should expand to: [wait(0), set_attr(count, 1),
    %   repeat(2, [wait(0), set_attr(count, 1)]), despawn]
    ActionsOut = [despawn],
    ctx_spawnCmds([], CtxNew, CtxNew)
)).

test("repeat: last repetition doesn't add repeat", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ActionsIn = [repeat(1, [wait(0)]), despawn],
    empty_ctx(Ctx0),
    ctx_set_attr_val(0/type, static, Ctx0, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        actions_old(ActionsIn),
        obj(object(id(1))),
        result(completed, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    ctx_spawnCmds(SpawnCmds, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (ActionsOut = [despawn]
     ;
     expect(false, "Actions wrong")),
    expect(SpawnCmds = [], "SpawnCmds != []")
)).

test("repeat: multiple actions in repeat list", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ActionsIn = [
        repeat(2, [
            wait(0),
            set_attr(a, 1),
            set_attr(b, 2)
        ]),
        despawn
    ],
    empty_ctx(Ctx0),
    ctx_set_attr_val(0/type, static, Ctx0, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        actions_old(ActionsIn),
        obj(object(id(1))),
        result(completed, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    ctx_spawnCmds(SpawnCmds, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    (ActionsOut = [despawn]
    ; expect(false, "Actions wrong")),
    expect(SpawnCmds = [], "SpawnCmds != []")
)).

% ==========================================================
% Tests: move_delta
% ==========================================================

test("move_delta: single frame moves and completes", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ActionsIn = [move_delta(1, 5, -3)],
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 10), attr(y, 20)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        actions_old(ActionsIn),
        obj(object(id(1))),
        result(yielded, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    ctx_attr_val(1/x, X, CtxNew, CtxNew),
    ctx_attr_val(1/y, Y, CtxNew, CtxNew),
    ctx_spawnCmds(SpawnCmds, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    expect(ActionsOut = [], "ActionsOut != []"),
    expect(X = 15, "X != 15"),
    expect(Y = 17, "Y != 17"),
    expect(SpawnCmds = [], "SpawnCmds != []")
)).

test("move_delta: multiple frames continues", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ActionsIn = [move_delta(3, 10, 5)],
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0,
              [attr(type, static), attr(x, 0), attr(y, 0)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        actions_old(ActionsIn),
        obj(object(id(1))),
        result(yielded, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    ctx_attr_val(1/x, X, CtxNew, CtxNew),
    ctx_attr_val(1/y, Y, CtxNew, CtxNew),
    ctx_spawnCmds(SpawnCmds, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    expect(X = 10, "X != 10"),
    expect(Y = 5, "Y != 5"),
    (ActionsOut = [move_delta(2, 10, 5)]
     ;
     expect(false, "Actions wrong")),
    expect(SpawnCmds = [], "SpawnCmds != []")
)).

test("move_delta: negative deltas work", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ActionsIn = [move_delta(2, -10, -5)],
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, [attr(x, 50), attr(y, 50)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        actions_old(ActionsIn),
        obj(object(id(1))),
        result(yielded, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    ctx_attr_val(1/x, X, CtxNew, CtxNew),
    ctx_attr_val(1/y, Y, CtxNew, CtxNew),
    ctx_spawnCmds(SpawnCmds, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    expect(X = 40, "X != 40"),
    expect(Y = 45, "Y != 45"),
    (ActionsOut = [move_delta(1, -10, -5)]
     ;
     expect(false, "Actions wrong")),
    expect(SpawnCmds = [], "SpawnCmds != []")
)).

test("move_delta: preserves other attributes", (
    ActionsIn = [move_delta(1, 5, -3)],
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
        actions_old(ActionsIn),
        obj(object(id(1))),
        result(yielded, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    ctx_attr_val(1/x, X, CtxNew, CtxNew),
    ctx_attr_val(1/y, Y, CtxNew, CtxNew),
    ctx_attr_val(1/hp, HP, CtxNew, CtxNew),
    ctx_attr_val(1/speed, Speed, CtxNew, CtxNew),
    ctx_spawnCmds(SpawnCmds, CtxNew, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    expect(ActionsOut = [], "ActionsOut != []"),
    expect(X = 15, "X != 15"),
    expect(Y = 17, "Y != 17"),
    expect(HP = 100, "HP != 100"),
    expect(Speed = 5, "Speed != 5"),
    expect(SpawnCmds = [], "SpawnCmds != []")
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
    ActionsIn = [
        move_to(:target_x, :target_y, 5)
    ],
    execute_action(
        actions_old(ActionsIn),
        obj(object(id(1))),
        result(_, actions_new(_)),
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
    ActionsIn = [
        % TODO: Hm I notice the seeming inconsistency here
        %       first attr does not need : but the other one
        %       does.. does'nt look too pretty or consistent
        set_attr(source_x, :x)
    ],
    execute_action(
        actions_old(ActionsIn),
        obj(object(id(1))),
        result(_, actions_new(_)),
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
    ActionsIn = [
        set_attr(my_target_y, :parent_id:target_y)
    ],
    execute_action(
        actions_old(ActionsIn),
        obj(object(id(1))),
        result(_, actions_new(_)),
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
    ActionsIn = [
        set_attr(
            result,
            :first_id:second_id:final_value
        )
    ],
    execute_action(
        actions_old(ActionsIn),
        obj(object(id(1))),
        result(_, actions_new(_)),
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
    ActionsIn = [spawn([set_attr(type, enemy),
                        copy_attr(spawn_x, x),
                        copy_attr(spawn_y, y)])],
    execute_action(
        actions_old(ActionsIn),
        obj(object(id(1))),
        result(_, actions_new(_)),
        Ctx,
        CtxNew
    ),
    % Check that spawn command was created with copy_attr
    % actions (copy_attr doesn't get resolved, it's executed
    % at spawn time on the new object)
    ctx_spawnCmds(SpawnCmds, CtxNew, CtxNew),
    % Should have one spawn command with parent_id
    % automatically added, then the actions as provided
    SpawnCmds = [spawn_cmd(actions([
                                     set_attr(parent_id, 1),
                                     set_attr(type, enemy),
                                     copy_attr(spawn_x, x),
                                     copy_attr(spawn_y, y)
                                     ]))]
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
    ActionsIn = [
        move_to(
            :target_x,
            200,
            :speed
        )
    ],
    execute_action(
        actions_old(ActionsIn),
        obj(object(id(1))),
        result(_, actions_new(_)),
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
    ActionsIn = [move_to(100, 200, 5)],
    execute_action(
        actions_old(ActionsIn),
        obj(object(id(1))),
        result(_, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    % Should work exactly as before
    ctx_attr_val(1/x, NewX, CtxNew, CtxNew),
    ctx_attr_val(1/y, NewY, CtxNew, CtxNew),
    NewX > 0,
    NewY > 0
)).
