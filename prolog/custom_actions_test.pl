:- module(custom_actions_test, []).
:- use_module('./execute_action', [
    execute_action/5,
    user_action/2
]).
:- use_module('./types/accessors').
:- use_module('./types/constructors', [
    ctx_with_attrs/2,
    empty_attr_store/1
]).
:- use_module('./engine', [tick/2]).
:- use_module(library(lists), [member/2]).
:- use_module(library(assoc), [
    empty_assoc/1,
    put_assoc/4
]).
% NOTE: YOu dont need to import expect because its goal
% expansion stuff
:- use_module('./util/util', [err_write/1]).
:- use_module(library(format)).
:- use_module(library(lists), [length/2]).

% ==========================================================
% Tests: Custom Actions (define_action and user-defined
% actions)
% ==========================================================

% --------------------------------------------------------
% Test: define_action stores the definition
% --------------------------------------------------------

test("define_action: stores action definition", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    % Clear any existing definitions
    retractall(execute_action:user_action(_, _)),
    
    ObjIn = object(
        id(0),
        type(static),
        actions([
            define_action(
                zigzag(Amplitude, Times),
                repeat(Times, [
                    move_delta(Amplitude, 0, 10),
                    move_delta(-Amplitude, 0, 10)
                ])
            )
        ]),
        collisions([])
    ),
    empty_attr_store(EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx0),
    ctx_objs_ctx(Ctx0, [], Ctx1),
    ctx_nextid_ctx(Ctx1, 1, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(define_action(
            zigzag(Amplitude, Times),
            repeat(Times, [
                move_delta(Amplitude, 0, 10),
                move_delta(-Amplitude, 0, 10)
            ])
        )),
        obj_old(ObjIn),
        result(completed, ObjOut)
    ),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    % Action should be removed from queue
    expect(obj_acns(ObjOut, [])),
    % Definition should be stored
    expect(execute_action:user_action(zigzag(_, _), _)),
    expect(ctx_cmds(CtxNew, []))
)).

% ----------------------------------------------------------
% Test: user-defined action expands and executes
% ----------------------------------------------------------

test("custom_action: zigzag expands and executes", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    % Clear any existing definitions
    retractall(execute_action:user_action(_, _)),
    
    % Define the action
    assertz(execute_action:user_action(
        zigzag(Amplitude, Times),
        repeat(Times, [
            move_delta(Amplitude, 0, 10),
            move_delta(-Amplitude, 0, 10)
        ])
    )),
    
    ObjIn = object(
        id(0),
        type(static),
        actions([zigzag(30, 2)]),
        collisions([])
    ),
    empty_attr_store(EmptyAttrs0),
    put_assoc(0, EmptyAttrs0, [attr(x, 100), attr(y, 100)],
              Attrs),
    ctx_with_attrs(Attrs, Ctx0),
    ctx_objs_ctx(Ctx0, [], Ctx1),
    ctx_nextid_ctx(Ctx1, 1, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(zigzag(30, 2)),
        obj_old(ObjIn),
        result(completed, ObjOut)
    ),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    % Should expand to repeat(2, [...])
    % After one execution, should have repeat(1, [...])
    % remaining
    obj_acns(ObjOut, Actions),
    % The expanded action should be in the queue
    expect((
        member(repeat(1, [move_delta(30, 0, 10),
            move_delta(-30, 0, 10)]), Actions)
        ; member(move_delta(30, 0, 10), Actions)
            % Or already expanded further
    )),
    expect(ctx_cmds(CtxNew, []))
)).

% --------------------------------------------------------
% Test: complete workflow - define and use in same object
% --------------------------------------------------------

test("custom_action: define and use in same action list", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    % Clear any existing definitions
    retractall(execute_action:user_action(_, _)),
    
    ObjIn = object(
        id(0),
        type(static),
        actions([
            % Define zigzag
            define_action(
                zigzag(Amp, N),
                repeat(N, [
                    move_delta(Amp, 0, 5),
                    move_delta(-Amp, 0, 5)
                ])
            ),
            wait(1),
            % Use it
            zigzag(20, 1),
            despawn
        ]),
        collisions([])
    ),
    empty_attr_store(EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, CtxTemp),
    ctx_objs_ctx(CtxTemp, [ObjIn], CtxTemp2),
    ctx_nextid_ctx(CtxTemp2, 1, Ctx0),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    % First tick: define_action executes
    tick(ctx_in(Ctx0), ctx_out(Ctx1)),
    expect(ctx_objs(Ctx1, [Obj1])),
    obj_acns(Obj1, Actions1),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    % define_action should be gone, zigzag should be there
    expect(\+ member(define_action(_, _), Actions1)),
    expect(member(zigzag(20, 1), Actions1)),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    % Second tick: zigzag expands and executes
    tick(ctx_in(Ctx1), ctx_out(Ctx2)),
    expect(ctx_objs(Ctx2, [Obj2])),
    obj_acns(Obj2, Actions2),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    % zigzag should be expanded to move_delta
    %   (repeat(1, ...) expands immediately)
    expect(\+ member(zigzag(_, _), Actions2)),
    expect(member(move_delta(_, _, _), Actions2))
)).

% --------------------------------------------------------
% Test: shoot_burst custom action
% --------------------------------------------------------

test("custom_action: shoot_burst defines and executes", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    % Clear any existing definitions
    retractall(execute_action:user_action(_, _)),
    
    ObjIn = object(
        id(0),
        type(static),
        actions([
            define_action(
                shoot_burst(Count),
                repeat(Count, [
                    spawn(proj, 100, 100, [
                        move_delta(0, -5, 20)
                    ]),
                    wait(3)
                ])
            ),
            wait(1),
            shoot_burst(2)
        ]),
        collisions([])
    ),
    empty_attr_store(EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, CtxTemp),
    ctx_objs_ctx(CtxTemp, [ObjIn], CtxTemp2),
    ctx_nextid_ctx(CtxTemp2, 1, Ctx0),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    % First tick: define_action executes
    tick(ctx_in(Ctx0), ctx_out(Ctx1)),
    expect(
        ctx_objs(Ctx1, [Obj1])
    ),
    obj_acns(Obj1, Actions1),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    % define_action should be gone
    expect(\+ member(define_action(_, _), Actions1)),
    expect(member(shoot_burst(2), Actions1)),
    
    % Second tick: shoot_burst expands
    tick(ctx_in(Ctx1), ctx_out(Ctx2)),
    ctx_objs(Ctx2, Objects2),
    % Should have spawned a projectile
    length(Objects2, NumObjects),
    % Original object + at least one projectile
    expect(NumObjects >= 2),
    % Find the original object
    expect(member(Obj2, Objects2)),
    expect(obj_id(Obj2, 0)),
    obj_acns(Obj2, Actions2),
    % shoot_burst should be expanded
    expect(\+ member(shoot_burst(_), Actions2))
)).

% --------------------------------------------------------
% Test: multiple custom actions
% --------------------------------------------------------

test("custom_action: multiple definitions work", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    % Clear any existing definitions
    retractall(execute_action:user_action(_, _)),
    
    ObjIn = object(
        id(0),
        type(static),
        actions([
            % frame 1
            define_action(
                move_up(Dist),
                move_delta(0, -Dist, 10)
            ),
            wait(1),
            % frame 2
            define_action(
                move_down(Dist),
                move_delta(0, Dist, 10)
            ),
            wait(1),
            % frame 3
            move_up(5),
            move_down(3)
        ]),
        collisions([])
    ),
    empty_attr_store(EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, CtxTemp),
    ctx_objs_ctx(CtxTemp, [ObjIn], CtxTemp2),
    ctx_nextid_ctx(CtxTemp2, 1, Ctx0),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    % First tick: first define_action
    tick(ctx_in(Ctx0), ctx_out(Ctx1)),
    expect(
        ctx_objs(Ctx1, [Obj1])
    ),
    obj_acns(Obj1, Actions1),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    expect(member(define_action(move_down(_), _),
        Actions1)),
    expect(\+ member(define_action(move_up(_), _),
        Actions1)),
    
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    % Second tick: second define_action
    tick(ctx_in(Ctx1), ctx_out(Ctx2)),
    expect(
        ctx_objs(Ctx2, [Obj2])
    ),
    obj_acns(Obj2, Actions2),

    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    expect(\+ member(define_action(_, _), Actions2)),
    expect(member(move_up(5), Actions2)),
    expect(member(move_down(3), Actions2)),
    
    % Third tick: move_up expands
    tick(ctx_in(Ctx2), ctx_out(Ctx3)),
    expect(
        ctx_objs(Ctx3, [Obj3])
    ),
    obj_acns(Obj3, Actions3),
    expect([] = Actions3)
)).

% --------------------------------------------------------
% Test: custom action with parameters
% --------------------------------------------------------

test("custom_action: parameters are correctly \
substituted", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    % Clear any existing definitions
    retractall(execute_action:user_action(_, _)),
    
    % Define a custom action with multiple parameters
    assertz(execute_action:user_action(
        move_pattern(X1, Y1, X2, Y2, Frames),
        list([
            move_to(X1, Y1, Frames),
            move_to(X2, Y2, Frames)
        ])
    )),
    
    ObjIn = object(
        id(0),
        type(static),
        actions([move_pattern(10, 10, 20, 20, 5)]),
        collisions([])
    ),
    empty_attr_store(EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx0),
    ctx_objs_ctx(Ctx0, [], Ctx1),
    ctx_nextid_ctx(Ctx1, 1, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(move_pattern(10, 10, 20, 20, 5)),
        obj_old(ObjIn),
        result(completed, ObjOut)
    ),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    % Should expand to list([move_to(10, 10, 5),
    %   move_to(20, 20, 5)])
    obj_acns(ObjOut, Actions),
    expect(
        (
            member(list([move_to(10, 10, 5),
                move_to(20, 20, 5)]), Actions)
            ; member(move_to(10, 10, 5), Actions)
                % Or already expanded
        )
    ),
    expect(ctx_cmds(CtxNew, []))
)).

