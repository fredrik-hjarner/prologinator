% Test Context Constructors
% Helper functions for creating test contexts with
% objects and actions

:- module(test_constructors, [ctx_with_obj_with_actions/2]).

:- use_module('../../build/prologinator').
:- use_module(library(assoc), [empty_assoc/1, put_assoc/4]).

% ==========================================================
% ctx_with_obj_with_actions/2
% ==========================================================
% Creates a context with object ID 0, type=static attribute,
% and actions in the actionstore.
% 
% Usage:
%   ctx_with_obj_with_actions([fork([wait(1)]),
%                               wait(2)], Ctx)
ctx_with_obj_with_actions(Actions, Ctx) :-
    empty_ctx(Ctx0),
    ctx_set_attr_val(0/type, static, Ctx0, Ctx1),
    ctx_set_objs([object(id(0))], Ctx1, Ctx2),
    % Set up actionstore entry for object 0
    % Actionstore maps ObjID -> list of streams
    % (lists of actions)
    empty_assoc(EmptyActionStore0),
    put_assoc(0, EmptyActionStore0, [Actions],
              ActionStore1),
    ctx_set_actionstore(ActionStore1, Ctx2, Ctx).

