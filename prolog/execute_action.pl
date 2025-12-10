% Action Execution Module
% Handles execution of all game actions

:- module(execute_action, [
    execute_action/5, user_action/2
]).

:- use_module(library(clpz)).
:- use_module(library(lists), [
    append/2,
    append/3,
    select/3,
    member/2,
    findall/3
]).
:- use_module(library(assoc), [
    gen_assoc/3,
    del_assoc/4
]).
:- use_module(library(format)).
:- use_module(library(charsio), [atom_chars/2]).
:- use_module(library(iso_ext)).
:- use_module('./types/validation', [action_validation/1]).
:- use_module('./types/accessors').
:- use_module('./types/adv_accessors').
:- use_module('./resolve_action', [resolve_action/4]).
:- use_module('./builtin_actions', [builtin_action/1]).
:- use_module('./input_helpers', [
    key_down/2,
    key_up/2,
    key_held/2
]).
:- use_module('./util/util', [
    select_many/3,
    is_list/1
]).

% :- use_module('xod/xod', [validate/2]).
% :- use_module('./types/validation2').

% execute_action_impl/5 clauses are intentionally separated
% by other code
:- discontiguous(execute_action_impl/5).

% ==========================================================
% Custom Actions: Runtime Expansion
% ==========================================================

% Dynamic predicate to store user-defined action definitions
:- dynamic(user_action/2).

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

% ==========================================================
% Top-level execute_action (with resolution)
% ==========================================================
execute_action(
    ctx_old(CtxOld),
    ctx_new(CtxNew),
    action(Action),
    obj_old(ObjIn),
    obj_new(ObjOut)
) :-
    % 1. Resolve value specs in action
    obj_id(ObjIn, MyID),
    resolve_action(CtxOld, MyID, Action, ResolvedAction),
    
    % 2. Delegate to existing logic (renamed)
    execute_action_resolved(
        ctx_old(CtxOld),
        ctx_new(CtxNew),
        action(ResolvedAction),
        obj_old(ObjIn),
        obj_new(ObjOut)
    ).

% ==========================================================
% RENAMED: execute_action → execute_action_resolved
% ==========================================================
% Wrapper: validates action then delegates to implementation
% Now threads Context directly to accumulate side effects.
% Also handles user-defined actions via runtime expansion.
execute_action_resolved(
    ctx_old(CtxOld),
    ctx_new(CtxNew),
    action(Action),
    obj_old(ObjIn),
    obj_new(ObjOut)
) :-
    % TODO: action_validation nowadays it almost useless
    %       since custom actions allow anything.
    %       so maybe I should validate builtins separately?
    action_validation(Action),
    % validate(Action, action_schema),
    ( builtin_action(Action) ->
        % It's a built-in action - execute normally
        execute_action_impl(
            ctx_old(CtxOld),
            ctx_new(CtxNew),
            action(Action),
            obj_old(ObjIn),
            obj_new(ObjOut)
        )
    ; user_action(Action, Body) -> % absolute prolog voodoo!
        % It's a user-defined action!
        % Action unifies with Template, binding variables
        % in Body automatically
        % Body now has the correct bindings, use it directly
        % Note: Body may contain attr() specs, so recurse
        % through top-level execute_action for resolution
        execute_action(
            ctx_old(CtxOld),
            ctx_new(CtxNew),
            action(Body),
            obj_old(ObjIn),
            obj_new(ObjOut)
        )
    ;
        % Unknown action
        throw(unknown_action(Action))
    ).

% ==========================================================
% execute_action_impl/5
% ==========================================================
% Internal implementation (no validation)
% ----------------------------------------------------------
% Basic Actions: wait
% ----------------------------------------------------------

execute_action_impl(
    ctx_old(Ctx),
    ctx_new(Ctx), % Context unchanged
    action(wait(N)),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    obj_acns(ObjIn, [_|Rest]),
    wait_continue(N, Rest, NewActions),
    obj_acns_obj(ObjIn, NewActions, ObjOut).

% Helper: Continue or finish wait action based on remaining
% frames
wait_continue(N, Rest, [wait(N1)|Rest]) :-
    N #> 1,
    N1 #= N - 1.
wait_continue(N, Rest, Rest) :- N #=< 1.

% ----------------------------------------------------------
% Basic Actions: move_to
% ----------------------------------------------------------

execute_action_impl(
    ctx_old(Ctx),
    ctx_new(CtxOut), % Context may change due to attrs
    action(move_to(TargetX, TargetY, Frames)),
    obj_old(object(
        id(ID),
        type(Type),
        actions([_|Rest]),
        Colls
    )),
    obj_new([object(
        id(ID),
        type(Type),
        actions(NewActions),
        Colls
    )])
) :-
    % Get current position from attribute store
    % Fails if object doesn't have x/y attributes
    ctx_attr_val(Ctx, ID/x, CurrX),
    ctx_attr_val(Ctx, ID/y, CurrY),
    % Compute step using integer division
    DX #= (TargetX - CurrX) // Frames,
    DY #= (TargetY - CurrY) // Frames,
    NewX #= CurrX + DX,
    NewY #= CurrY + DY,
    % Label at the boundary where we need ground values for
    % game objects
    (ground(TargetX), ground(TargetY), ground(CurrX),
     ground(CurrY), ground(Frames) ->
        labeling([], [NewX, NewY])
    ;
        true
    ),
    % Update position in attribute store
    ctx_attr_val_ctx(Ctx, ID/x, NewX, Ctx1),
    ctx_attr_val_ctx(Ctx1, ID/y, NewY, CtxOut),
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
    ctx_old(CtxIn),
    ctx_new(CtxOut),
    action(despawn),
    obj_old(object(id(ID), _, _, _)),
    obj_new([])
) :-
    % Remove object's attributes from store
    ctx_attrs(CtxIn, AttrStore),
    ( gen_assoc(ID, AttrStore, _Attrs) ->
        del_assoc(ID, AttrStore, _, NewAttrStore)
    ;
        NewAttrStore = AttrStore
    ),
    ctx_attrs_ctx(CtxIn, NewAttrStore, CtxOut).

% ----------------------------------------------------------
% Basic Actions: noop
% ----------------------------------------------------------

execute_action_impl(
    ctx_old(Ctx),
    ctx_new(Ctx),
    action(noop),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    obj_acns(ObjIn, [_|Rest]),
    obj_acns_obj(ObjIn, Rest, ObjOut).

% ----------------------------------------------------------
% Basic Actions: define_action
% ----------------------------------------------------------

% Defines a custom action macro at runtime. Stores the
% Signature->Body mapping in user_action/2. When Signature
% is called later, execute_action/5 will expand it with
% parameter substitution and execute the Body.
execute_action_impl(
    ctx_old(Ctx),
    ctx_new(Ctx),  % Context unchanged
    action(define_action(Signature, Body)),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    % Store the macro definition
    assertz(user_action(Signature, Body)),
    % Remove this action from queue
    obj_acns(ObjIn, [_|Rest]),
    obj_acns_obj(ObjIn, Rest, ObjOut).

% ----------------------------------------------------------
% Basic Actions: set_attr
% ----------------------------------------------------------

% set_attr/2 - Set attribute on self
execute_action_impl(
    ctx_old(Ctx),
    ctx_new(CtxOut),
    action(set_attr(Key, Value)),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    obj_id(ObjIn, MyID),
    obj_acns(ObjIn, [_|Rest]),
    obj_acns_obj(ObjIn, Rest, ObjOut),
    ctx_attr_val_ctx(Ctx, MyID/Key, Value, CtxOut).

% set_attr/3 - Set attribute on specific object
execute_action_impl(
    ctx_old(Ctx),
    ctx_new(CtxOut),
    action(set_attr(TargetID, Key, Value)),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    obj_acns(ObjIn, [_|Rest]),
    obj_acns_obj(ObjIn, Rest, ObjOut),
    ctx_attr_val_ctx(Ctx, TargetID/Key, Value, CtxOut).

% ----------------------------------------------------------
% Basic Actions: incr
% ----------------------------------------------------------

% incr/2 - Increment attribute on self
execute_action_impl(
    ctx_old(Ctx),
    ctx_new(CtxOut),
    action(incr(Key, Amount)),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    obj_id(ObjIn, MyID),
    obj_acns(ObjIn, [_|Rest]),
    obj_acns_obj(ObjIn, Rest, ObjOut),
    ( ctx_attr_val(Ctx, MyID/Key, CurrentValue) ->
        NewValue #= CurrentValue + Amount
    ;
        NewValue = Amount
    ),
    ctx_attr_val_ctx(Ctx, MyID/Key, NewValue, CtxOut).

% incr/3 - Increment attribute on specific object
execute_action_impl(
    ctx_old(Ctx),
    ctx_new(CtxOut),
    action(incr(TargetID, Key, Amount)),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    obj_acns(ObjIn, [_|Rest]),
    obj_acns_obj(ObjIn, Rest, ObjOut),
    ( ctx_attr_val(Ctx, TargetID/Key,
                   CurrentValue) ->
        NewValue #= CurrentValue + Amount
    ;
        NewValue = Amount
    ),
    ctx_attr_val_ctx(Ctx, TargetID/Key, NewValue,
                     CtxOut).

% ----------------------------------------------------------
% Basic Actions: decr
% ----------------------------------------------------------

% decr/2 - Decrement attribute on self
execute_action_impl(
    ctx_old(Ctx),
    ctx_new(CtxOut),
    action(decr(Key, Amount)),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    obj_id(ObjIn, MyID),
    obj_acns(ObjIn, [_|Rest]),
    obj_acns_obj(ObjIn, Rest, ObjOut),
    ( ctx_attr_val(Ctx, MyID/Key, CurrentValue) ->
        NewValue #= CurrentValue - Amount
    ;
        NewValue #= 0 - Amount
    ),
    ctx_attr_val_ctx(Ctx, MyID/Key, NewValue, CtxOut).

% decr/3 - Decrement attribute on specific object
execute_action_impl(
    ctx_old(Ctx),
    ctx_new(CtxOut),
    action(decr(TargetID, Key, Amount)),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    obj_acns(ObjIn, [_|Rest]),
    obj_acns_obj(ObjIn, Rest, ObjOut),
    ( ctx_attr_val(Ctx, TargetID/Key,
                   CurrentValue) ->
        NewValue #= CurrentValue - Amount
    ;
        NewValue #= 0 - Amount
    ),
    ctx_attr_val_ctx(Ctx, TargetID/Key, NewValue,
                     CtxOut).

% ----------------------------------------------------------
% Basic Actions: log
% ----------------------------------------------------------

% Confirmed that it works! Don't worry!
execute_action_impl(
    ctx_old(Ctx),
    ctx_new(Ctx), % Context unchanged
    action(log(Msg)),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    obj_acns(ObjIn, [_|Rest]),
    format("~s~n", [Msg]),
    obj_acns_obj(ObjIn, Rest, ObjOut).

% ----------------------------------------------------------
% Compound Actions: spawn
% ----------------------------------------------------------

% IMMEDIATELY spawns the object into the context
execute_action_impl(
    ctx_old(CtxIn),
    ctx_new(CtxOut),
    action(spawn(Type, X, Y, Actions)),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    obj_acns(ObjIn, [_|Rest]),
    obj_acns_obj(ObjIn, Rest, ObjOut),
    
    % 1. Generate ID
    ctx_nextid(CtxIn, ID),
    NextID #= ID + 1,
    ctx_nextid_ctx(CtxIn, NextID, CtxTemp1),
    
    % 2. Create Object (no attributes - stored separately)
    NewObj = object(
        id(ID), type(Type),
        actions(Actions), collisions([])
    ),
    
    % 3. Initialize attributes in store
    ctx_attr_val_ctx(CtxTemp1, ID/x, X, CtxTemp2a),
    ctx_attr_val_ctx(CtxTemp2a, ID/y, Y, CtxTemp2),
    
    % 4. Append to Context
    % Since ID is increasing, appending to end keeps the
    % list sorted.
    % Note: append/3 is O(N), but for typical game sizes
    % (<1000 objects) this is acceptable. For larger scales,
    % consider difference lists or reverse-order storage.
    ctx_objs(CtxTemp2, CurrentSpawns),
    append(CurrentSpawns, [NewObj], NewSpawns),
    ctx_objs_ctx(CtxTemp2, NewSpawns, CtxOut).

% ----------------------------------------------------------
% Compound Actions: loop
% ----------------------------------------------------------

execute_action_impl(
    ctx_old(Ctx),
    ctx_new(Ctx),
    action(loop(Actions)),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    obj_acns(ObjIn, [_|Rest]),
    append(Actions, [loop(Actions)], Expanded),
    append(Expanded, Rest, NewActions),
    obj_acns_obj(ObjIn, NewActions, ObjOut).

% ----------------------------------------------------------
% Compound Actions: list
% ----------------------------------------------------------

execute_action_impl(
    ctx_old(Ctx),
    ctx_new(Ctx),
    action(list(Actions)),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    obj_acns(ObjIn, [_|Rest]),
    append(Actions, Rest, NewActions),
    obj_acns_obj(ObjIn, NewActions, ObjOut).

% ----------------------------------------------------------
% Compound Actions: repeat
% ----------------------------------------------------------

% repeat(+Times, +Actions)
% Mode: repeat(+Times, +Actions)
% Description: Execute action list N times, then
%   continue
% Yields: false (expands immediately)

execute_action_impl(
    ctx_old(Ctx),
    ctx_new(Ctx),
    action(repeat(Times, Acts)),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    obj_acns(ObjIn, [_|Rest]),
    Times #> 0,
    Times1 #= Times - 1,
    % Build next action(s)
    ( Times1 #> 0 ->
        % More reps: add another repeat
        NextRepeat = [repeat(Times1, Acts)]
    ;
        % Last rep: no more repeat
        NextRepeat = []
    ),
    % Queue: Actions + NextRepeat + Rest
    append(Acts, NextRepeat, Tail),
    append(Tail, Rest, NewActions),
    obj_acns_obj(ObjIn, NewActions, ObjOut).

% ----------------------------------------------------------
% Compound Actions: load
% ----------------------------------------------------------

% load(+Path)
% Mode: load(+Path)
% Description: Loads a file containing a list of actions
%   and prepends them to the action queue
% Yields: false (expands immediately)

execute_action_impl(
    ctx_old(Ctx),
    ctx_new(Ctx),
    action(load(Path)),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    obj_acns(ObjIn, [_|Rest]),
    % Convert Path (list of chars) to atom for open/3
    % Path must be a string (list of chars), not an atom
    atom_chars(PathAtom, Path),
    % Read file as term (expects list of actions)
    setup_call_cleanup(
        open(PathAtom, read, Stream),
        read_term(Stream, Actions, []),
        close(Stream)
    ),
    % Validate it's a list
    ( is_list(Actions) ->
        append(Actions, Rest, NewActions),
        obj_acns_obj(ObjIn, NewActions, ObjOut)
    ;
        throw(error(
            type_error(list, Actions),
            load/1
        ))
    ).

% ----------------------------------------------------------
% Basic Actions: move_delta
% ----------------------------------------------------------

% move_delta(+Frames, +DX, +DY)
% Mode: move_delta(+Frames, +DX, +DY)
% Description: Relative movement: move by (DX, DY)
%   each frame for Frames frames
% Yields: true when Frames > 0

execute_action_impl(
    ctx_old(Ctx),
    ctx_new(CtxOut),
    action(move_delta(Frames, DX, DY)),
    obj_old(object(
        id(ID),
        type(Type),
        actions([_|Rest]),
        Colls
    )),
    obj_new([object(
        id(ID),
        type(Type),
        actions(NewActions),
        Colls
    )])
) :-
    % Get current position from attribute store
    ( ctx_attr_val(Ctx, ID/x, CurrX),
      ctx_attr_val(Ctx, ID/y, CurrY) ->
        true
    ;
        % Default to 0,0 if not set
        CurrX = 0,
        CurrY = 0
    ),
    % Apply delta
    NewX #= CurrX + DX,
    NewY #= CurrY + DY,
    % Label for grounding
    ( ground(CurrX), ground(CurrY), ground(DX),
      ground(DY) ->
        labeling([], [NewX, NewY])
    ;
        true
    ),
    % Update position in attribute store
    ctx_attr_val_ctx(Ctx, ID/x, NewX, Ctx1),
    ctx_attr_val_ctx(Ctx1, ID/y, NewY, CtxOut),
    % Continue or arrive
    ( Frames #> 1 ->
        Frames1 #= Frames - 1,
        NewActions = [move_delta(Frames1, DX, DY)|Rest]
    ;
        NewActions = Rest
    ).

% ----------------------------------------------------------
% Input Actions: wait_key_down
% ----------------------------------------------------------

% wait_key_down(+KeyCode)
% Mode: wait_key_down(+KeyCode)
% Description: Waits until specified key is pressed
%   (detects 'down' event for the key)
% Yields: false (checks each frame)

execute_action_impl(
    ctx_old(Ctx),
    ctx_new(Ctx),  % Context unchanged
    action(wait_key_down(KeyCode)),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    obj_acns(ObjIn, [_|Rest]),
    
    % Check if key was pressed THIS frame
    ( key_down(Ctx, KeyCode) ->
        % Key pressed: action complete
        obj_acns_obj(ObjIn, Rest, ObjOut)
    ;
        % Key not pressed: keep waiting
        obj_acns_obj(
          ObjIn,
          [wait_key_down(KeyCode)|Rest],
          ObjOut
        )
    ).

% ----------------------------------------------------------
% Input Actions: wait_key_up
% ----------------------------------------------------------

% wait_key_up(+KeyCode)
% Mode: wait_key_up(+KeyCode)
% Description: Waits until specified key is released
%   (detects 'up' event for the key)
% Yields: false (checks each frame)

execute_action_impl(
    ctx_old(Ctx),
    ctx_new(Ctx),  % Context unchanged
    action(wait_key_up(KeyCode)),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    obj_acns(ObjIn, [_|Rest]),
    
    % Check if key released THIS frame
    ( key_up(Ctx, KeyCode) ->
        % Key released: action complete
        obj_acns_obj(ObjIn, Rest, ObjOut)
    ;
        % Key still pressed: keep waiting
        obj_acns_obj(
          ObjIn,
          [wait_key_up(KeyCode)|Rest],
          ObjOut
        )
    ).

% ----------------------------------------------------------
% Input Actions: wait_key_held
% ----------------------------------------------------------

% wait_key_held(+KeyCode)
% Mode: wait_key_held(+KeyCode)
% Description: Yields every frame while key is held
%   Use in loop: loop([wait_key_held(39), move(...)])
% Yields: true when key is held, false otherwise

execute_action_impl(
    ctx_old(Ctx),
    ctx_new(Ctx),  % Context unchanged
    action(wait_key_held(KeyCode)),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    obj_acns(ObjIn, [_|Rest]),
    
    ( key_held(Ctx, KeyCode) ->
        % Key held: yield (action complete)
        obj_acns_obj(ObjIn, Rest, ObjOut)
    ;
        % Key not held: keep waiting
        obj_acns_obj(
          ObjIn,
          [wait_key_held(KeyCode)|Rest],
          ObjOut
        )
    ).

% ----------------------------------------------------------
% Compound Actions: trigger_state_change
% ----------------------------------------------------------

% IMMEDIATELY updates status in context
execute_action_impl(
    ctx_old(CtxIn),
    ctx_new(CtxOut),
    action(trigger_state_change(Change)),
    obj_old(ObjIn),
    obj_new([ObjOut])
) :-
    obj_acns(ObjIn, [_|Rest]),
    obj_acns_obj(ObjIn, Rest, ObjOut),
    
    ctx_status(CtxIn, CurrentStatus),
    update_status(Change, CurrentStatus, NewStatus),
    ctx_status_ctx(CtxIn, NewStatus, CtxOut).

update_status(game_over(lost), _, lost).
update_status(game_over(won), lost, lost).
update_status(game_over(won), _, won).
% update_status(_, S, S). % fallback

% ----------------------------------------------------------
% Compound Actions: parallel
% ----------------------------------------------------------

execute_action_impl(
    ctx_old(CtxOld),
    ctx_new(CtxNew),
    action(parallel_all(ChildActions)),
    obj_old(ObjIn),
    obj_new(MaybeObjectOut)
) :-
    obj_acns(ObjIn, [_|Rest]),
    tick_all(
        CtxOld,
        CtxNew,
        ChildActions,
        ObjIn,
        _ObjFinal,
        ChildResults
    ),
    ( member([], ChildResults) ->
        MaybeObjectOut = []
    ;
        filter_running_actions(
            ChildResults, RemainingActions
        ),
        ( RemainingActions = [] ->
            obj_acns_obj(ObjIn, Rest, NewObj),
            MaybeObjectOut = [NewObj]
        ;
            obj_acns_obj(
                ObjIn,
                [
                    parallel_all_running(RemainingActions)
                    |Rest
                ],
                NewObj
            ),
            MaybeObjectOut = [NewObj]
        )
    ).

execute_action_impl(
    ctx_old(CtxOld),
    ctx_new(CtxNew),
    action(parallel_all_running(Children)),
    obj_old(Obj),
    obj_new(NewObj)
) :-
    execute_action(
        ctx_old(CtxOld),
        ctx_new(CtxNew),
        action(parallel_all(Children)),
        obj_old(Obj),
        obj_new(NewObj)
    ).

% ----------------------------------------------------------
% Compound Actions: parallel_race
% ----------------------------------------------------------

% parallel_race(+ChildActions)
% Mode: parallel_race(+ChildActions)
% Description: Execute all children in
%   parallel. Stop when ANY child
%   finishes (despawns or action
%   completes).
% Yields: false

execute_action_impl(
    ctx_old(CtxOld),
    ctx_new(CtxNew),
    action(parallel_race(ChildActions)),
    obj_old(ObjIn),
    obj_new(MaybeObjectOut)
) :-
    obj_acns(ObjIn, [_|Rest]),
    % Execute children until one finishes or despawns.
    % Status indicates the outcome:
    %   - done(FinalObj): A child finished/despawned.
    %     Stop immediately, use FinalObj's attrs.
    %   - continue(RunningKids, FinalObj): No child done
    %     yet. Continue racing with RunningKids next tick.
    tick_race(CtxOld, CtxNew, ChildActions, ObjIn, Status),
    ( Status = done(FinalObj, Despawned) ->
        ( Despawned = true ->
            % Child despawned: parent must also despawn
            MaybeObjectOut = []
        ;
            % Child finished normally: proceed with parent
            %   actions (attributes already in store by ID)
            obj_acns_obj(ObjIn, Rest, NewObj),
            MaybeObjectOut = [NewObj]
        )
    ; Status = continue(RunningKids, FinalObj) ->
        % Race continues: wrap remaining children in
        %   parallel_race_running for next tick
        % (attributes already in store by ID)
        obj_acns_obj(
            ObjIn,
            [parallel_race_running(RunningKids)|Rest],
            NewObj
        ),
        MaybeObjectOut = [NewObj]
    ).

% Handle parallel_race_running triggered
%   from race
%
% parallel_race_running(+ChildActions)
% Mode: parallel_race_running(+ChildActions)
% Description: Continue racing when
%   children didn't all finish

execute_action_impl(
    ctx_old(CtxOld),
    ctx_new(CtxNew),
    action(parallel_race_running(Children)),
    obj_old(Obj),
    obj_new(NewObj)
) :-
    execute_action_impl(
        ctx_old(CtxOld),
        ctx_new(CtxNew),
        action(parallel_race(Children)),
        obj_old(Obj),
        obj_new(NewObj)
    ).

% ==========================================================
% Helpers for Parallel Execution
% ==========================================================

% tick_all(+CIn, -COut, +Actions, +ObjIn,
%   -ObjFinal, -Results)
% Executes ALL children sequentially, continuing even
%   if one despawns or finishes. All children get their
%   turn to execute in this tick.
%
% Unlike tick_race, this does NOT stop early. Every
%   child is executed regardless of what happens to
%   previous children.

tick_all(C, C, [], Obj, Obj, []).
tick_all(
    CIn, COut, [Act|Acts], ObjIn, ObjFinal, [Res|Results]
) :-
    run_child(CIn, CTemp, Act, ObjIn, Res),
    update_obj_attrs(ObjIn, Res, NextObj),
    tick_all(CTemp, COut, Acts, NextObj, ObjFinal, Results).

% tick_race(+CIn, -COut, +Actions, +ObjIn, -Status)
% Executes children sequentially until one finishes
%   or despawns, then stops immediately.
%
% Status indicates the outcome:
%   - done(FinalObj, Despawned): A child finished
%     (actions empty) or despawned (result is []).
%     Execution stops here.
%     FinalObj has attrs updated from the winning child.
%     Despawned is true if child despawned,
%     false if finished.
%   - continue(RunningKids, FinalObj): No child finished
%     yet.
%     RunningKids is the list of actions still running.
%     FinalObj has attrs from all children processed so far.
%
% This implements "fail-fast" behavior: as soon as any
%   child completes, remaining children are NOT executed
%   in this tick.

tick_race(C, C, [], Obj, continue([], Obj)).
tick_race(CIn, COut, [Act|Acts], ObjIn, Status) :-
    run_child(CIn, CTemp, Act, ObjIn, Res),
    ( is_child_done(Res) ->
        % Winner found: stop immediately, don't process
        %   remaining children
        COut = CTemp,
        update_obj_attrs(ObjIn, Res, FinalObj),
        ( Res = [] ->
            Despawned = true
        ;
            Despawned = false
        ),
        Status = done(FinalObj, Despawned)
    ;
        % This child still running: continue with next
        %   sibling
        update_obj_attrs(ObjIn, Res, NextObj),
        tick_race(CTemp, COut, Acts, NextObj, RecStatus),
        ( RecStatus = continue(RestKids, FinalObj) ->
            % Race still going: collect this child's action
            extract_action(Res, NewAct),
            Status = continue([NewAct|RestKids], FinalObj)
        ;
            % A later child finished: propagate the done
            %   status up
            Status = RecStatus
        )
    ).

% run_child(+CIn, -COut, +Act, +Obj, -Res)
run_child(CIn, COut, Act, Obj, Res) :-
    obj_id(Obj, ID),
    obj_type(Obj, T),
    obj_collisions(Obj, C),
    % Create child object (attrs in centralized store)
    Child = object(
        id(ID),
        type(T),
        actions([Act]),
        collisions(C)
    ),
    execute_action(
        ctx_old(CIn),
        ctx_new(COut),
        action(Act),
        obj_old(Child),
        obj_new(Res)
    ).

% update_obj_attrs(+Obj, +ResultList, -NewObj)
% Updates object based on child result.
%   - If result is an object, use it
%   - If result is [] (despawned), keep old obj
%   Attributes are in centralized store, so no need to copy
%   them.

update_obj_attrs(
    Obj, [object(_, _, _, _)], Obj
).
update_obj_attrs(Obj, [], Obj).

% is_child_done(+ResultList)
% True if ResultList indicates the child is done:
%   - [] means child despawned
%   - [object(..., actions([]), ...)] means child
%     finished (no remaining actions)

is_child_done([]).
is_child_done([object(_, _, actions([]), _)]).

% extract_action(+ResultList, -Action)
% Extracts the next action from a running child's
%   result. The child is still running if it has
%   actions remaining.

extract_action([object(_, _, actions([A|_]), _)], A).

% filter_running_actions(+Results, -Actions)
% Recursively filters child results to extract actions
%   from children that are still running.
% Finished children (actions([])) and despawned
%   children ([]) are skipped.

filter_running_actions([], []).
filter_running_actions([Res|Rs], Acc) :-
    ( Res = [object(_, _, actions([A|_]), _)] ->
        % Child still running: include its action
        Acc = [A|RestAcc],
        filter_running_actions(Rs, RestAcc)
    ;
        % Child done or despawned: skip it
        filter_running_actions(Rs, Acc)
    ).

