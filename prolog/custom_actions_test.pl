:- module(custom_actions_test, []).

:- use_module('../build/prologinator').
:- use_module('../prolog/util/test_util').

:- use_module(library(lists), [member/2]).
:- use_module(library(assoc), [
    empty_assoc/1,
    put_assoc/4,
    gen_assoc/3
]).
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
    retractall(prologinator:user_action(_, _)),
    
    ActionsIn = [
        define_action(
            zigzag(Amplitude, Times),
            repeat(Times, [
                move_delta(Amplitude, 0, 10),
                move_delta(-Amplitude, 0, 10)
            ])
        )
    ],
    empty_attr_store(EmptyAttrs0),
    put_assoc(0, EmptyAttrs0, [attr(type, static)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx0),
    ctx_set_objs([], Ctx0, Ctx1),
    ctx_set_nextid(1, Ctx1, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(define_action(
            zigzag(Amplitude, Times),
            repeat(Times, [
                move_delta(Amplitude, 0, 10),
                move_delta(-Amplitude, 0, 10)
            ])
        )),
        actions_old(ActionsIn),
        obj_id(0),
        result(completed, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    % Action should be removed from queue
    expect(ActionsOut = []),
    % Definition should be stored
    expect(prologinator:user_action(zigzag(_, _), _)),
    expect(ctx_spawnCmds([], CtxNew, CtxNew))
)).

% ----------------------------------------------------------
% Test: user-defined action expands and executes
% ----------------------------------------------------------

test("custom_action: zigzag expands and executes", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    % Clear any existing definitions
    retractall(prologinator:user_action(_, _)),
    
    % Define the action
    assertz(prologinator:user_action(
        zigzag(Amplitude, Times),
        repeat(Times, [
            move_delta(Amplitude, 0, 10),
            move_delta(-Amplitude, 0, 10)
        ])
    )),
    
    ActionsIn = [zigzag(30, 2)],
    empty_attr_store(EmptyAttrs0),
    put_assoc(0, EmptyAttrs0,
              [attr(type, static),
               attr(x, 100), attr(y, 100)],
              Attrs),
    ctx_with_attrs(Attrs, Ctx0),
    ctx_set_objs([], Ctx0, Ctx1),
    ctx_set_nextid(1, Ctx1, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(zigzag(30, 2)),
        actions_old(ActionsIn),
        obj_id(0),
        result(completed, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    % Should expand to repeat(2, [...])
    % After one execution, should have repeat(1, [...])
    % remaining
    % The expanded action should be in the queue
    expect((
        member(repeat(1, [move_delta(30, 0, 10),
            move_delta(-30, 0, 10)]), ActionsOut)
        ; member(move_delta(30, 0, 10), ActionsOut)
            % Or already expanded further
    )),
    expect(ctx_spawnCmds([], CtxNew, CtxNew))
)).

% --------------------------------------------------------
% Test: complete workflow - define and use in same object
% --------------------------------------------------------

test("custom_action: define and use in same action list", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    % Clear any existing definitions
    retractall(prologinator:user_action(_, _)),
    
    ObjIn = object(id(0)),
    ActionsIn = [
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
    ],
    empty_attr_store(EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, CtxTemp),
    ctx_set_objs([ObjIn], CtxTemp, CtxTemp2),
    ctx_set_nextid(1, CtxTemp2, Ctx0),
    % Add actions to actionstore
    empty_assoc(EmptyActionStore),
    put_assoc(0, EmptyActionStore, [ActionsIn],
              ActionStore0),
    ctx_set_actionstore(ActionStore0, Ctx0,
                        Ctx0WithActions),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    % First tick: define_action executes
    tick(Ctx0WithActions, Ctx1),
    ctx_objs(Objects1, Ctx1, Ctx1),
    expect(length(Objects1, 1), 'Should have one object'),
    ctx_actionstore(ActionStore1, Ctx1, Ctx1),
    ( gen_assoc(0, ActionStore1, Streams1) ->
        ( Streams1 = [Actions1] ->
            % define_action should be gone, zigzag should
            % be there
            expect(\+ member(define_action(_, _),
                             Actions1)),
            expect(member(zigzag(20, 1),
                          Actions1))
        ;
            throw(error('Expected single stream, got',
                        Streams1))
        )
    ;
        throw(error('Object 0 not in actionstore'))
    ),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    % Second tick: zigzag expands and executes
    tick(Ctx1, Ctx2),
    ctx_objs(Objects2, Ctx2, Ctx2),
    expect(length(Objects2, 1), 'Should have one object'),
    ctx_actionstore(ActionStore2, Ctx2, Ctx2),
    gen_assoc(0, ActionStore2, [Actions2]),
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
    retractall(prologinator:user_action(_, _)),
    
    ObjIn = object(id(0)),
    ActionsIn = [
        define_action(
            shoot_burst(Count),
            repeat(Count, [
                spawn([set_attr(type, proj),
                       set_attr(x, 100),
                       set_attr(y, 100),
                       move_delta(0, -5, 20)]),
                wait(3)
            ])
        ),
        wait(1),
        shoot_burst(2)
    ],
    empty_attr_store(EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, CtxTemp),
    ctx_set_objs([ObjIn], CtxTemp, CtxTemp2),
    ctx_set_nextid(1, CtxTemp2, Ctx0),
    % Add actions to actionstore
    empty_assoc(EmptyActionStore),
    put_assoc(0, EmptyActionStore, [ActionsIn],
              ActionStore0),
    ctx_set_actionstore(ActionStore0, Ctx0,
                        Ctx0WithActions),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    % First tick: define_action executes
    write('First tick: define_action executes'), nl,
    tick(Ctx0WithActions, Ctx1),
    write('After first tick'), nl,
    expect(
        ctx_objs(Objects1, Ctx1, Ctx1),
        'Objects should be updated'
    ),
    expect(length(Objects1, 1), 'Should have one object'),
    expect(
        ctx_actionstore(ActionStore1, Ctx1, Ctx1),
        'Actionstore should be updated'
    ),
    expect(
        gen_assoc(0, ActionStore1, [Actions1]),
        'Object 0 should have actions in actionstore'
    ),
    % define_action should be gone
    expect(\+ member(define_action(_, _), Actions1)),
    expect(member(shoot_burst(2), Actions1)),
    
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    % Second tick: shoot_burst expands
    write('Second tick: shoot_burst expands'), nl,
    tick(Ctx1, Ctx2),
    write('After second tick'), nl,
    ctx_objs(Objects2, Ctx2, Ctx2),
    % Should have spawned a projectile
    length(Objects2, NumObjects),
    % Original object + at least one projectile
    expect(NumObjects >= 2),
    % Find the original object (ID 0)
    member(Obj2, Objects2),
    obj_id(Obj2, 0),
    expect(
        ctx_actionstore(ActionStore2, Ctx2, Ctx2),
        'Actionstore should be updated'
    ),
    expect(
        gen_assoc(0, ActionStore2, [Actions2]),
        'Object 0 should have actions in actionstore'
    ),
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
    retractall(prologinator:user_action(_, _)),
    
    ObjIn = object(id(0)),
    ActionsIn = [
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
    ],
    empty_attr_store(EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, CtxTemp),
    ctx_set_objs([ObjIn], CtxTemp, CtxTemp2),
    ctx_set_nextid(1, CtxTemp2, Ctx0),
    % Add actions to actionstore
    empty_assoc(EmptyActionStore),
    put_assoc(0, EmptyActionStore, [ActionsIn],
              ActionStore0),
    ctx_set_actionstore(ActionStore0, Ctx0,
                        Ctx0WithActions),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    % First tick: first define_action
    tick(Ctx0WithActions, Ctx1),
    ctx_objs(Objects1, Ctx1, Ctx1),
    expect(length(Objects1, 1), 'Should have one object'),
    ctx_actionstore(ActionStore1, Ctx1, Ctx1),
    gen_assoc(0, ActionStore1, [Actions1]),
    expect(member(define_action(move_down(_), _),
        Actions1)),
    expect(\+ member(define_action(move_up(_), _),
        Actions1)),
    
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    % Second tick: second define_action
    tick(Ctx1, Ctx2),
    ctx_objs(Objects2, Ctx2, Ctx2),
    expect(length(Objects2, 1), 'Should have one object'),
    ctx_actionstore(ActionStore2, Ctx2, Ctx2),
    gen_assoc(0, ActionStore2, [Actions2]),

    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    expect(\+ member(define_action(_, _), Actions2)),
    expect(member(move_up(5), Actions2)),
    expect(member(move_down(3), Actions2)),
    
    % Third tick: move_up expands and executes (completes
    % instantly since Frames=0), then move_down expands and
    % executes (also completes instantly)
    tick(Ctx2, Ctx3),
    ctx_objs(Objects3, Ctx3, Ctx3),
    expect(length(Objects3, 1), 'Should have one object'),
    ctx_actionstore(ActionStore3, Ctx3, Ctx3),
    ( gen_assoc(0, ActionStore3, Streams3) ->
        ( Streams3 = [] ->
            % All streams completed and were removed
            expect(true)
        ; Streams3 = [Actions3] ->
            % Both move_up and move_down complete instantly
            % (Frames=0), so actions should be empty
            expect([] = Actions3)
        ;
            throw(error('Unexpected streams', Streams3))
        )
    ;
        % No actionstore entry means all actions completed
        expect(true)
    )
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
    retractall(prologinator:user_action(_, _)),
    
    % Define a custom action with multiple parameters
    assertz(prologinator:user_action(
        move_pattern(X1, Y1, X2, Y2, Frames),
        list([
            move_to(X1, Y1, Frames),
            move_to(X2, Y2, Frames)
        ])
    )),
    
    ActionsIn = [move_pattern(10, 10, 20, 20, 5)],
    empty_attr_store(EmptyAttrs0),
    put_assoc(0, EmptyAttrs0,
              [attr(type, static), attr(x, 0), attr(y, 0)],
              EmptyAttrs),
    ctx_with_attrs(EmptyAttrs, Ctx0),
    ctx_set_objs([], Ctx0, Ctx1),
    ctx_set_nextid(1, Ctx1, Ctx),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        action(move_pattern(10, 10, 20, 20, 5)),
        actions_old(ActionsIn),
        obj_id(0),
        result(Status, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    % Should expand to list([move_to(...), move_to(...)])
    % The list action now executes immediately, so move_to
    % will have started (yielded after 1 frame)
    expect(
        Status = yielded
    ),
    expect(
        member(list([move_to(10, 10, _),
            move_to(20, 20, 5)]), ActionsOut)
    ),
    expect(ctx_spawnCmds([], CtxNew, CtxNew))
)).

