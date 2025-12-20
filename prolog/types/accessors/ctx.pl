% Context Accessors Module
% Provides getter and setter predicates for context data
% structures
%
% This module centralizes all field access patterns to
% ensure consistent access to nested structures and make
% refactoring easier.

% Old convention (deprecated):
% For getters the functors start with the "thing" we get
% from, then comes _ followed by all "stuff" we want to get.
% The fields come in the order in which they occur on the
% "thing"! So try to keep that!
% For setters it's the same, except we add _ followed by the
% "thing" in the end.
% Rationale for covention:
% The functor immediately tells you what to put where.

% New convention (what we're refactoring towards):
% The setters should have CtxIn and CtxOut as last arguments
% so that they can be better used in DCGs.
% The new naming convention is to start the predicate name
% with the entity then followed by all the fields to be set
% on it such as ctx_set_frame, ctx_set_objs_attrs.
% Single fields setters don't use dcg, but bulk setters do
% use dcg.

% ==========================================================
%
%   Context Accessors
%
% ==========================================================

% ==========================================================
% Context Getters
% ==========================================================

% ----------------------------------------------------------
% Single Field Getters
% ----------------------------------------------------------

% getter ctx_frame/3 for dcg use
ctx_frame(F, Ctx, Ctx) :-
    Ctx = ctx(state(frame(F), _, _, _, _, _, _), _).

ctx_objs(O, Ctx, Ctx) :-
    Ctx = ctx(state(_, objects(O), _, _, _, _, _), _).

ctx_attrs(A, Ctx, Ctx) :-
    Ctx = ctx(state(_, _, attrs(A), _, _, _, _), _).

ctx_state(S, Ctx, Ctx) :-
    Ctx = ctx(S, _).

ctx_cmds(commands(SC, FC), Ctx, Ctx) :-
    Ctx = ctx(state(_, _, _, _, _,
                    commands(SC, FC), _), _).

ctx_spawnCmds(SC, Ctx, Ctx) :-
    Ctx = ctx(state(_, _, _, _, _,
                    commands(spawn_cmds(SC),
                             fork_cmds(_)), _), _).

ctx_forkCmds(FC, Ctx, Ctx) :-
    Ctx = ctx(state(_, _, _, _, _,
                    commands(spawn_cmds(_),
                             fork_cmds(FC)), _), _).

ctx_status(S, Ctx, Ctx) :-
    Ctx = ctx(state(_, _, _, status(S), _, _, _), _).

ctx_nextid(N, Ctx, Ctx) :-
    Ctx = ctx(state(_, _, _, _, next_id(N), _, _), _).

ctx_actionstore(AS, Ctx, Ctx) :-
    Ctx = ctx(state(_, _, _, _, _, _,
                    actionstore(AS)), _).

ctx_input(I, Ctx, Ctx) :-
    Ctx = ctx(_, I).

ctx_events(E, Ctx, Ctx) :-
    Ctx = ctx(_, input(events(E), _)).

ctx_held(H, Ctx, Ctx) :-
    Ctx = ctx(_, input(_, held(H))).

% ----------------------------------------------------------
% Bulk Getters
% ----------------------------------------------------------

ctx_objs_cmds(Objs, Cmds) -->
    ctx_objs(Objs),
    ctx_cmds(Cmds).

ctx_objs_attrs(Objs, Attrs) -->
    ctx_objs(Objs),
    ctx_attrs(Attrs).

ctx_status_cmds(Status, Cmds) -->
    ctx_status(Status),
    ctx_cmds(Cmds).

ctx_objs_nextid_cmds(Objs, NextID, Cmds) -->
    ctx_objs(Objs),
    ctx_nextid(NextID),
    ctx_cmds(Cmds).

% ==========================================================
% Context Setters
% ==========================================================

% ----------------------------------------------------------
% Single Field Setters
% ----------------------------------------------------------

ctx_set_frame(F, ctx(state(_, O, A, S, N, C, AS), I),
              ctx(state(frame(F), O, A, S, N, C, AS), I)).

ctx_set_objs(O, ctx(state(F, _, A, S, N, C, AS), I),
             ctx(state(F, objects(O), A, S, N, C, AS), I)).

ctx_set_attrs(A, ctx(state(F, O, _, S, N, C, AS), I),
              ctx(state(F, O, attrs(A), S, N, C, AS), I)).

ctx_set_state(S, ctx(_, I), ctx(S, I)).

ctx_set_cmds(commands(SC, FC),
             ctx(state(F, O, A, S, N, _, AS), I),
             ctx(state(F, O, A, S, N,
                       commands(SC, FC), AS), I)).

ctx_set_spawnCmds(SC,
                  ctx(state(F, O, A, S, N,
                            commands(spawn_cmds(_),
                                     fork_cmds(FC)), AS),
                      I),
                  ctx(state(F, O, A, S, N,
                            commands(spawn_cmds(SC),
                                     fork_cmds(FC)), AS),
                      I)).

ctx_set_forkCmds(FC,
                 ctx(state(F, O, A, S, N,
                           commands(spawn_cmds(SC),
                                    fork_cmds(_)), AS),
                     I),
                 ctx(state(F, O, A, S, N,
                           commands(spawn_cmds(SC),
                                    fork_cmds(FC)), AS),
                     I)).

ctx_set_status(S, ctx(state(F, O, A, _, N, C, AS), I),
               ctx(state(F, O, A, status(S), N, C, AS),
                   I)).

ctx_set_nextid(N, ctx(state(F, O, A, S, _, C, AS), I),
               ctx(state(F, O, A, S, next_id(N), C, AS),
                   I)).

ctx_set_actionstore(AS, ctx(state(F, O, A, S, N, C, _),
                            I),
                    ctx(state(F, O, A, S, N, C,
                              actionstore(AS)), I)).

ctx_set_input(I, ctx(S, _), ctx(S, I)).

% ----------------------------------------------------------
% Bulk Setters (CtxOld, CtxNew as hidden last arguments)
% ----------------------------------------------------------

ctx_set_objs_cmds(Objs, Cmds) -->
    ctx_set_objs(Objs),
    ctx_set_cmds(Cmds).

ctx_set_objs_attrs(Objs, Attrs) -->
    ctx_set_objs(Objs),
    ctx_set_attrs(Attrs).

ctx_set_status_cmds(Status, Cmds) -->
    ctx_set_status(Status),
    ctx_set_cmds(Cmds).

ctx_set_nextid_cmds(NextID, Cmds) -->
    ctx_set_nextid(NextID),
    ctx_set_cmds(Cmds).

ctx_set_objs_nextid_cmds(Objs, NextID, Cmds) -->
    ctx_set_objs(Objs),
    ctx_set_nextid(NextID),
    ctx_set_cmds(Cmds).
