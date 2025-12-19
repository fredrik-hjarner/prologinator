:- module(fork_fwd_test, []).

:- use_module('../../../build/prologinator').
:- use_module('../../../prolog/util/test_util').
:- use_module('../../../prolog/test_utils/\
test_constructors').

:- use_module(library(lists), [member/2, length/2]).
:- use_module(library(assoc), [gen_assoc/3]).

% ==========================================================
% Forward Tests (all inputs ground, normal use case)
% ==========================================================

% ==========================================================
% Tests: fork
% ==========================================================

test("fork: adds new stream to actionstore and processes \
in same frame", (
    % ------------------------------------------------------
    % Arrange
    % ------------------------------------------------------
    ctx_with_obj_with_actions(
        [fork([wait(1), noop]), wait(2)],
        Ctx
    ),
    % ------------------------------------------------------
    % Act
    % ------------------------------------------------------
    tick(Ctx, CtxNew),
    % ------------------------------------------------------
    % Assert
    % ------------------------------------------------------
    % Fork command should be processed and new stream added
    ctx_actionstore(ActionStoreNew, CtxNew, CtxNew),
    gen_assoc(0, ActionStoreNew, Streams),
    % After fork completes and wait(2) yields:
    % - Original stream becomes [wait(1)] (wait(2) yielded)
    % - Fork command adds new stream [wait(1), noop]
    % The forked stream should be processed in the
    % same frame, so after processing [wait(1),
    % noop], it becomes [noop] (wait(1) yielded)
    % Should have 2 streams: [wait(1)] and [noop]
    length(Streams, NumStreams),
    % Check that fork command was cleared
    ctx_forkCmds([], CtxNew, CtxNew),
    % Check stream count - should be 2
    (NumStreams = 2 ->
        true
    ;
        format(string(ErrorMsg),
               "Expected 2 streams, got: ~w Streams: ~w",
               [NumStreams, Streams]),
        expect(false, ErrorMsg)
    ),
    % Check that both streams exist
    expect(member([wait(1)], Streams),
           "Stream [wait(1)] not found"),
    expect(member([noop], Streams),
           "Forked stream [noop] not found"),
    % Fork commands should be cleared
    ctx_forkCmds([], CtxNew, CtxNew),
    % Frame should increment
    ctx_frame(1, CtxNew, CtxNew),
    ctx_status(playing, CtxNew, CtxNew)
)).

