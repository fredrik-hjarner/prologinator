% Action Execution Module
% Handles execution of all game actions

:- module(execute_action, [
    execute_action/6
]).

:- use_module(library(clpz)).
:- use_module(library(lists), [
    append/2,
    append/3,
    select/3,
    member/2,
    findall/3
]).
:- use_module('./types/validation', [action_validation/1]).
:- use_module('./types/accessors', [
    obj_acns/2,
    obj_acns_obj/3,
    obj_attrs/2,
    obj_attrs_acns_obj/4
]).
% :- use_module('xod/xod', [validate/2]).
% :- use_module('./types/validation2').

% ==========================================================
% execute_action/5
% ==========================================================

% Forward execution (normal game)
% :- pred execute_action(+Action, +ObjIn, -ObjOut,
%     -Commands, -RevHints) 
%     :: (action(Action), game_object(ObjIn)) 
%     => (action(Action), game_object(ObjOut),
%     list(command, Commands), list(rev_hint, RevHints))
%     + is_det.

% Reverse execution (undo/replay)
% :- pred execute_action(-Action, -ObjIn, +ObjOut,
%     +Commands, +RevHints) + is_det.

% Action inference - which must the action have been?
% :- pred execute_action(-Action, +ObjIn, +ObjOut,
%     +Commands, +RevHints).

% Validation (consistency check)
% :- pred execute_action(+Action, +ObjIn, +ObjOut,
%     +Commands, +RevHints).

% Wrapper: validates action then delegates to implementation
execute_action(
    ctx_old(Ctx),
    action(Action),
    obj_old(ObjIn),
    obj_new(ObjOut),
    cmds_new(Commands),
    revhints_new(RevHints)
) :-
    action_validation(Action),
    % validate(Action, action_schema),
    execute_action_impl(
        ctx_old(Ctx),
        action(Action),
        obj_old(ObjIn),
        obj_new(ObjOut),
        cmds_new(Commands),
        revhints_new(RevHints)
    ).

% ==========================================================
% execute_action_impl/5
% ==========================================================
% Internal implementation (no validation)
% ----------------------------------------------------------
% Basic Actions: wait_frames
% ----------------------------------------------------------

execute_action_impl(
    ctx_old(_Ctx),
    action(wait_frames(N)),
    obj_old(ObjIn),
    obj_new([ObjOut]),
    cmds_new([]),
    revhints_new([])
) :-
    obj_acns(ObjIn, [_|Rest]),
    ( N #> 1 ->
        N1 #= N - 1,
        NewActions = [wait_frames(N1)|Rest]
    ;
        % N = 1, wait is done
        NewActions = Rest
    ),
    obj_acns_obj(ObjIn, NewActions, ObjOut).

% ----------------------------------------------------------
% Basic Actions: move_to
% ----------------------------------------------------------

execute_action_impl(
    ctx_old(_Ctx),
    action(move_to(TargetX, TargetY, Frames)),
    obj_old(object(
        id(ID),
        type(Type),
        attrs(Attrs),
        actions([_|Rest]),
        Colls
    )),
    obj_new([object(
        id(ID),
        type(Type),
        attrs(NewAttrs),
        actions(NewActions),
        Colls
    )]),
    cmds_new([]),
    revhints_new([])
) :-
    select(pos(CurrentX, CurrentY), Attrs, RestAttrs),
    % Compute step using integer division
    DX #= (TargetX - CurrentX) // Frames,
    DY #= (TargetY - CurrentY) // Frames,
    NewX #= CurrentX + DX,
    NewY #= CurrentY + DY,
    % Label at the boundary where we need ground values for
    % game objects
    (ground(TargetX), ground(TargetY), ground(CurrentX),
     ground(CurrentY), ground(Frames) ->
        labeling([], [NewX, NewY])
    ;
        true
    ),
    NewAttrs = [pos(NewX, NewY)|RestAttrs],
    ( Frames #> 1 ->
        Frames1 #= Frames - 1,
        NewActions = [move_to(TargetX, TargetY, Frames1)|
            Rest]
    ;
        NewActions = Rest  % Arrived
    ).

% ----------------------------------------------------------
% Basic Actions: despawn
% ----------------------------------------------------------

execute_action_impl(
    ctx_old(_Ctx),
    action(despawn),
    obj_old(object(
        id(ID),
        type(_Type),
        attrs(Attrs),
        actions(_),
        collisions(_Colls)
    )),
    obj_new([]),
    cmds_new([]),
    revhints_new([despawned(ID, Attrs)])
) :- !.

% ----------------------------------------------------------
% Compound Actions: spawn (from Addendum 3 - FINAL version)
% ----------------------------------------------------------

execute_action_impl(
    ctx_old(_Ctx),
    action(spawn(Type, Pos, Actions)),
    obj_old(ObjIn),
    obj_new([ObjOut]),
    cmds_new([spawn_request(Type, Pos, Actions)]),
    revhints_new([])
) :-
    obj_acns(ObjIn, [_|Rest]),
    obj_acns_obj(ObjIn, Rest, ObjOut).

% ----------------------------------------------------------
% Compound Actions: loop
% ----------------------------------------------------------

execute_action_impl(
    ctx_old(_Ctx),
    action(loop(Actions)),
    obj_old(ObjIn),
    obj_new([ObjOut]),
    cmds_new([]),
    revhints_new([])
) :-
    obj_acns(ObjIn, [_|Rest]),
    append(Actions, [loop(Actions)], Expanded),
    append(Expanded, Rest, NewActions),
    obj_acns_obj(ObjIn, NewActions, ObjOut).

% ----------------------------------------------------------
% Compound Actions: trigger_state_change
% ----------------------------------------------------------

execute_action_impl(
    ctx_old(_Ctx),
    action(trigger_state_change(Change)),
    obj_old(ObjIn),
    obj_new([ObjOut]),
    cmds_new([state_change(Change)]),
    revhints_new([])
) :-
    obj_acns(ObjIn, [_|Rest]),
    obj_acns_obj(ObjIn, Rest, ObjOut).

% ----------------------------------------------------------
% Compound Actions: parallel
% ----------------------------------------------------------

execute_action_impl(
    ctx_old(Ctx),
    action(parallel(ChildActions)),
    obj_old(ObjIn),
    obj_new(MaybeObjectOut),
    cmds_new(AllCommands),
    revhints_new(AllRevHints)
) :-
    ObjIn = object(
        id(ID), type(Type), _,
        actions([_|Rest]), collisions(Colls)
    ),
    tick_parallel_children(
        ctx_old(Ctx),
        ChildActions, % list of actions that run in parallel
        ObjIn,
        ObjFinal,
        ChildResults,
        AllCommands,
        AllRevHints
    ),
    obj_attrs(ObjFinal, AttrsOut),
    ( member([], ChildResults) ->
        % Any child despawned, so despawn the whole parallel
        MaybeObjectOut = []
    ;
        % All children are alive. Filter out "Done" ones.
        % Look inside objects returned in ChildResults.
        findall( % pick out
            Action,
            (
                member([ChildObj], ChildResults),
                ChildObj = object(
                    _, _, _, actions([Action|_]), _
                )
            ),
            RemainingActions
        ),
        ( RemainingActions = [] ->
            % All children finished naturally.
            obj_attrs_acns_obj(
                ObjIn, AttrsOut, Rest, NewObj
            ),
            MaybeObjectOut = [NewObj]
        ;
            % Some children are still running.
            obj_attrs_acns_obj(
                ObjIn,
                AttrsOut,
                [parallel_running(RemainingActions)|Rest],
                NewObj
            ),
            MaybeObjectOut = [NewObj]
        )
    ).

execute_action_impl(
    ctx_old(Ctx),
    action(parallel_running(Children)),
    obj_old(Obj),
    obj_new(NewObj),
    cmds_new(Commands),
    revhints_new(RevHints)
) :-
    execute_action(
        ctx_old(Ctx),
        action(parallel(Children)),
        obj_old(Obj),
        obj_new(NewObj),
        cmds_new(Commands),
        revhints_new(RevHints)
    ).

% ==========================================================
% tick_parallel_children/6
% ==========================================================

% :- pred tick_parallel_children(
%     ?Children, % actions_list
%     ?ObjIn, % game_object
%     ?ObjFinal, % game_object with final attrs
%     ?UpdatedChildren, % list of result lists
%     ?AllCommands,
%     ?AllRevHints).

tick_parallel_children(
    ctx_old(_Ctx), [], ObjIn, ObjIn, [], [], []
).

tick_parallel_children(
    ctx_old(Ctx),
    [Child|RestChildren],
    ObjIn,
    ObjFinal,
    [ChildResult|RestResults],
    AllCommands,
    AllRevHints
) :-
    ObjIn = object(
        id(ID), type(Type), attrs(AttrsIn),
        actions(_), collisions(Colls)
    ),

    % 1. Construct input object for this child action
    ChildObjIn = object(
        id(ID), type(Type), attrs(AttrsIn),
        actions([Child]), collisions(Colls)
    ),

    % 2. Execute the action
    execute_action(
        ctx_old(Ctx),
        action(Child),
        obj_old(ChildObjIn),
        obj_new(ResultList),
        cmds_new(C1),
        revhints_new(R1)
    ),

    % 3. Determine attributes for the NEXT child.
    %    If this child killed the object, we pass the OLD
    %    attributes to the next sibling so it can at least
    %    attempt to run (e.g., to generate its own
    %    commands/hints).
    ( ResultList = [object(_, _, attrs(AttrsNext), _, _)] ->
        true
    ;
        ResultList = [],
        AttrsNext = AttrsIn
    ),

    % 4. Build object for next child
    NextObjIn = object(
        id(ID), type(Type), attrs(AttrsNext),
        actions(_), collisions(Colls)
    ),

    % 5. Recurse
    tick_parallel_children(
        ctx_old(Ctx),
        RestChildren,
        NextObjIn,
        ObjFinal,
        RestResults,
        C2,
        R2
    ),
    
    % 6. Build output
    ChildResult = ResultList,
    append(C1, C2, AllCommands),
    append(R1, R2, AllRevHints).
