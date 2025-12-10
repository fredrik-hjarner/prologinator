:- module(custom_actions_test, []).
:- use_module('./execute_action', [execute_action/5]).
:- use_module('./types/accessors').
:- use_module('./engine', [tick/2]).
:- use_module(library(lists), [member/2]).
:- use_module(library(assoc), [
    empty_assoc/1,
    put_assoc/4
]).

% ==========================================================
% Tests: Custom Actions (define_action and user-defined
% actions)
% ==========================================================

% --------------------------------------------------------
% Test: define_action stores the definition
% --------------------------------------------------------

test("define_action: stores action definition", (
    % Clear any existing definitions
    retractall(user_action(_, _)),
    
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
    empty_assoc(EmptyAttrs),
    Ctx = ctx(state(
        frame(0),
        objects([]),
        attrs(EmptyAttrs),
        status(playing),
        next_id(1),
        commands([])
    )),
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
        obj_new([ObjOut])
    ),
    % Action should be removed from queue
    obj_acns(ObjOut, []),
    % Definition should be stored
    user_action(zigzag(_, _), _),
    ctx_cmds(CtxNew, [])
)).

% ----------------------------------------------------------
% Test: user-defined action expands and executes
% ----------------------------------------------------------

test("custom_action: zigzag expands and executes", (
    % Clear any existing definitions
    retractall(user_action(_, _)),
    
    % Define the action
    assertz(user_action(
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
    empty_assoc(EmptyAttrs0),
    put_assoc(0, EmptyAttrs0, [attr(x, 100), attr(y, 100)],
              EmptyAttrs),
    Ctx = ctx(state(
        frame(0),
        objects([]),
        attrs(EmptyAttrs),
        status(playing),
        next_id(1),
        commands([])
    )),
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(zigzag(30, 2)),
        obj_old(ObjIn),
        obj_new([ObjOut])
    ),
    % Should expand to repeat(2, [...])
    % After one execution, should have repeat(1, [...])
    % remaining
    obj_acns(ObjOut, Actions),
    % The expanded action should be in the queue
    ( member(repeat(1, [move_delta(30, 0, 10), move_delta(-30, 0, 10)]), Actions)
    ; member(move_delta(30, 0, 10), Actions)  % Or already expanded further
    ),
    ctx_cmds(CtxNew, [])
)).

% --------------------------------------------------------
% Test: complete workflow - define and use in same object
% --------------------------------------------------------

test("custom_action: define and use in same action list", (
    % Clear any existing definitions
    retractall(user_action(_, _)),
    
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
            % Use it
            zigzag(20, 1),
            despawn
        ]),
        collisions([])
    ),
    Ctx0 = ctx(state(
        frame(0),
        objects([]),
        status(playing),
        next_id(1),
        commands([])
    )),
    
    % First tick: define_action executes
    tick(ctx_in(Ctx0), ctx_out(Ctx1)),
    ctx_objs(Ctx1, [Obj1]),
    obj_acns(Obj1, Actions1),
    % define_action should be gone, zigzag should be there
    \+ member(define_action(_, _), Actions1),
    member(zigzag(20, 1), Actions1),
    
    % Second tick: zigzag expands and executes
    tick(ctx_in(Ctx1), ctx_out(Ctx2)),
    ctx_objs(Ctx2, [Obj2]),
    obj_acns(Obj2, Actions2),
    % zigzag should be expanded (either to repeat or to move_delta)
    \+ member(zigzag(_, _), Actions2),
    % Should have either repeat or move_delta in queue
    ( member(repeat(_, _), Actions2)
    ; member(move_delta(_, _, _), Actions2)
    )
)).

% --------------------------------------------------------
% Test: shoot_burst custom action
% --------------------------------------------------------

test("custom_action: shoot_burst defines and executes", (
    % Clear any existing definitions
    retractall(user_action(_, _)),
    
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
            shoot_burst(2)
        ]),
        collisions([])
    ),
    Ctx0 = ctx(state(
        frame(0),
        objects([]),
        status(playing),
        next_id(1),
        commands([])
    )),
    
    % First tick: define_action executes
    tick(ctx_in(Ctx0), ctx_out(Ctx1)),
    ctx_objs(Ctx1, [Obj1]),
    obj_acns(Obj1, Actions1),
    % define_action should be gone
    \+ member(define_action(_, _), Actions1),
    member(shoot_burst(2), Actions1),
    
    % Second tick: shoot_burst expands
    tick(ctx_in(Ctx1), ctx_out(Ctx2)),
    ctx_objs(Ctx2, Objects2),
    % Should have spawned a projectile
    length(Objects2, NumObjects),
    NumObjects >= 2,  % Original object + at least one projectile
    % Find the original object
    member(Obj2, Objects2),
    obj_id(Obj2, 0),
    obj_acns(Obj2, Actions2),
    % shoot_burst should be expanded
    \+ member(shoot_burst(_), Actions2)
)).

% --------------------------------------------------------
% Test: multiple custom actions
% --------------------------------------------------------

test("custom_action: multiple definitions work", (
    % Clear any existing definitions
    retractall(user_action(_, _)),
    
    ObjIn = object(
        id(0),
        type(static),
        actions([
            define_action(move_up(Dist), move_delta(0, -Dist, 10)),
            define_action(move_down(Dist), move_delta(0, Dist, 10)),
            move_up(5),
            move_down(3)
        ]),
        collisions([])
    ),
    Ctx0 = ctx(state(
        frame(0),
        objects([]),
        status(playing),
        next_id(1),
        commands([])
    )),
    
    % First tick: first define_action
    tick(ctx_in(Ctx0), ctx_out(Ctx1)),
    ctx_objs(Ctx1, [Obj1]),
    obj_acns(Obj1, Actions1),
    member(define_action(move_down(_), _), Actions1),
    \+ member(define_action(move_up(_), _), Actions1),
    
    % Second tick: second define_action
    tick(ctx_in(Ctx1), ctx_out(Ctx2)),
    ctx_objs(Ctx2, [Obj2]),
    obj_acns(Obj2, Actions2),
    \+ member(define_action(_, _), Actions2),
    member(move_up(5), Actions2),
    member(move_down(3), Actions2),
    
    % Third tick: move_up expands
    tick(ctx_in(Ctx2), ctx_out(Ctx3)),
    ctx_objs(Ctx3, [Obj3]),
    obj_acns(Obj3, Actions3),
    \+ member(move_up(_), Actions3),
    member(move_delta(0, -5, 10), Actions3),
    member(move_down(3), Actions3)
)).

% --------------------------------------------------------
% Test: custom action with parameters
% --------------------------------------------------------

test("custom_action: parameters are correctly substituted", (
    % Clear any existing definitions
    retractall(user_action(_, _)),
    
    % Define a custom action with multiple parameters
    assertz(user_action(
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
    empty_assoc(EmptyAttrs),
    Ctx = ctx(state(
        frame(0),
        objects([]),
        attrs(EmptyAttrs),
        status(playing),
        next_id(1),
        commands([])
    )),
    execute_action(
        ctx_old(Ctx),
        ctx_new(CtxNew),
        action(move_pattern(10, 10, 20, 20, 5)),
        obj_old(ObjIn),
        obj_new([ObjOut])
    ),
    % Should expand to list([move_to(10, 10, 5), move_to(20, 20, 5)])
    obj_acns(ObjOut, Actions),
    ( member(list([move_to(10, 10, 5), move_to(20, 20, 5)]), Actions)
    ; member(move_to(10, 10, 5), Actions)  % Or already expanded
    ),
    ctx_cmds(CtxNew, [])
)).

