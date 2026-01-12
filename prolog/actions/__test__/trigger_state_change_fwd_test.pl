:- module(trigger_state_change_fwd_test, []).

#include "./build/prologinator.pl"
#include "./prolog/util/test_util.pl"

% ==========================================================
% Forward Tests (all inputs ground, normal use case)
% ==========================================================

% --------------------------------------------------------
% Tests: trigger_state_change
% --------------------------------------------------------

test("trigger_state_change: forward test - updates status \
to won", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ActionsIn = [trigger_state_change(game_over(won))],
    empty_ctx(CtxTemp),
    ctx_set_attr_val(1/type, static, CtxTemp, CtxIn),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        actions_old(ActionsIn),
        obj(object(id(1))),
        result(completed, actions_new(ActionsOut)),
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
    empty_ctx(CtxTemp),
    ctx_set_attr_val(1/type, static, CtxTemp, CtxIn),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        actions_old(ActionsIn),
        obj(object(id(1))),
        result(completed, actions_new(ActionsOut)),
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
    empty_ctx(CtxTemp0),
    ctx_set_attr_val(1/type, static, CtxTemp0, CtxTemp),
    ctx_set_status(lost, CtxTemp, CtxIn),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    execute_action(
        actions_old(ActionsIn),
        obj(object(id(1))),
        result(completed, actions_new(ActionsOut)),
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

