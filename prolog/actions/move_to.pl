% move_to action implementation

:- module(execute_action_move_to, []).

:- use_module(library(clpz)).
:- use_module('../types/accessors').
:- use_module('../types/adv_accessors').

:- multifile(execute_action:execute_action_impl/5).
:- discontiguous(execute_action:execute_action_impl/5).

execute_action:execute_action_impl(
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
    execute_move_to(
        Ctx,
        CtxOut,
        TargetX,
        TargetY,
        Frames,
        ID,
        Rest,
        NewActions
    ).

% ==========================================================
% execute_move_to/8
% ==========================================================
execute_move_to(
    Ctx,
    CtxOut,
    TargetX,
    TargetY,
    Frames,
    ID,
    Rest,
    NewActions
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

