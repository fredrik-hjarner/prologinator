% move_delta action implementation

:- module(execute_action_move_delta, []).

:- use_module(library(clpz)).
:- use_module('../types/accessors').
:- use_module('../types/adv_accessors').

:- multifile(execute_action:execute_action_impl/5).
:- discontiguous(execute_action:execute_action_impl/5).

% move_delta(+Frames, +DX, +DY)
% Mode: move_delta(+Frames, +DX, +DY)
% Description: Relative movement: move by (DX, DY)
%   each frame for Frames frames
% Yields: true when Frames > 0

execute_action:execute_action_impl(
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
    execute_move_delta(
        Ctx, CtxOut, Frames, DX, DY, ID, Rest, NewActions
    ).

% ==========================================================
% execute_move_delta/9
% ==========================================================
execute_move_delta(
    Ctx, CtxOut, Frames, DX, DY, ID, Rest, NewActions
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

