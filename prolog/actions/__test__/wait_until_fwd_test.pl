:- module(wait_until_fwd_test, []).

#include "./build/prologinator.pl"
#include "./prolog/util/test_util.pl"

% ==========================================================
% Tests: wait_until Action (Forward)
% ==========================================================

test("wait_until: completes when path exists", (
    % --------------------------------------------------
    % Arrange
    % --------------------------------------------------
    ActionsIn = [
        wait_until(exists(:collision_id)),
        wait(0)
    ],
    empty_attr_store(EmptyAttrs0),
    put_assoc(1, EmptyAttrs0, 
              [attr(type, static), attr(x, 0), attr(y, 0), 
               attr(collision_id, 5)], 
              Attrs),
    ctx_with_attrs(Attrs, Ctx),
    % --------------------------------------------------
    % Act
    % --------------------------------------------------
    execute_action(
        actions_old(ActionsIn),
        obj_id(1),
        result(Status, actions_new(ActionsOut)),
        Ctx,
        CtxNew
    ),
    % --------------------------------------------------
    % Assert
    % --------------------------------------------------
    expect(Status = completed, 'Status != completed'),
    expect(ActionsOut = [wait(0)],
        'Actions != [wait(0)]'),
    expect(ctx_spawnCmds([], CtxNew, CtxNew),
           'SpawnCmds != []')
)).

% TODO: Not should expect exception when path does not exist
% test("wait_until: yields when path does not exist", (
%     % --------------------------------------------------
%     % Arrange
%     % --------------------------------------------------
%     ActionsIn = [
%         wait_until(exists(collision_id)),
%         wait(0)
%     ],
%     empty_attr_store(EmptyAttrs0),
%     put_assoc(1, EmptyAttrs0, 
%               [attr(type, static), attr(x, 0), attr(y,0)],
%               Attrs),
%     ctx_with_attrs(Attrs, Ctx),
%     % --------------------------------------------------
%     % Act
%     % --------------------------------------------------
%     execute_action(
%         actions_old(ActionsIn),
%         obj_id(1),
%         result(Status, actions_new(ActionsOut)),
%         Ctx,
%         _CtxNew
%     ),
%     % --------------------------------------------------
%     % Assert
%     % --------------------------------------------------
%     expect(Status = yielded, 'Status != yielded'),
%     expect(
%         ActionsOut = [
%             wait_until(exists(collision_id)), wait(0)
%         ],
%         'Action not preserved'
%     )
% )).

test("wait_until: works with nested paths", (
    % --------------------------------------------------
    % Arrange
    % --------------------------------------------------
    ActionsIn = [
        wait_until(exists(:collision_id:collisionType)),
        wait(0)
    ],
    empty_attr_store(EmptyAttrs0),
    % Object 1 has collision_id pointing to object 5
    put_assoc(1, EmptyAttrs0, 
              [attr(type, static), attr(x, 0), attr(y, 0), 
               attr(collision_id, 5)], 
              Attrs1),
    % Object 5 has collisionType
    put_assoc(5, Attrs1, 
              [attr(x, 10), attr(y, 10), 
               attr(collisionType, enemy)], 
              Attrs),
    ctx_with_attrs(Attrs, Ctx),
    % --------------------------------------------------
    % Act
    % --------------------------------------------------
    execute_action(
        actions_old(ActionsIn),
        obj_id(1),
        result(Status, actions_new(ActionsOut)),
        Ctx,
        _CtxNew
    ),
    % --------------------------------------------------
    % Assert
    % --------------------------------------------------
    expect(Status = completed, 'Status != completed'),
    expect(ActionsOut = [wait(0)],
        'Actions != [wait(0)]')
)).

test("wait_until: yields when nested path does not exist", (
    % --------------------------------------------------
    % Arrange
    % --------------------------------------------------
    ActionsIn = [
        wait_until(exists(
            :collision_id:collisionType
        )),
        wait(0)
    ],
    empty_attr_store(EmptyAttrs0),
    % Object 1 has collision_id pointing to object 5
    put_assoc(1, EmptyAttrs0, 
              [attr(type, static), attr(x, 0), attr(y, 0), 
               attr(collision_id, 5)], 
              Attrs1),
    % Object 5 exists but has no collisionType
    put_assoc(5, Attrs1, 
              [attr(x, 10), attr(y, 10)], 
              Attrs),
    ctx_with_attrs(Attrs, Ctx),
    % --------------------------------------------------
    % Act
    % --------------------------------------------------
    execute_action(
        actions_old(ActionsIn),
        obj_id(1),
        result(Status, actions_new(ActionsOut)),
        Ctx,
        _CtxNew
    ),
    % --------------------------------------------------
    % Assert
    % --------------------------------------------------
    write_term(Status, [quoted(true)]), nl,
    write_term(ActionsOut, [quoted(true)]), nl,
    expect(Status = yielded, 'Status != yielded'),
    expect(ActionsOut = [
            wait_until(exists(:collision_id:collisionType)),
            wait(0)
        ],
        'Action not preserved'
    )
)).

test("wait_until: yields when intermediate path does not \
exist", (
    % --------------------------------------------------
    % Arrange
    % --------------------------------------------------
    ActionsIn = [
        wait_until(exists(:collision_id:collisionType)),
        wait(0)
    ],
    empty_attr_store(EmptyAttrs0),
    % Object 1 has no collision_id at all
    put_assoc(1, EmptyAttrs0, 
              [attr(type, static), attr(x, 0), attr(y, 0)], 
              Attrs),
    ctx_with_attrs(Attrs, Ctx),
    % --------------------------------------------------
    % Act
    % --------------------------------------------------
    execute_action(
        actions_old(ActionsIn),
        obj_id(1),
        result(Status, actions_new(ActionsOut)),
        Ctx,
        _CtxNew
    ),
    % --------------------------------------------------
    % Assert
    % --------------------------------------------------
    expect(Status = yielded, 'Status != yielded'),
    expect(ActionsOut = [wait_until(exists(
                             :collision_id:collisionType)),
                         wait(0)],
           'Action not preserved')
)).
