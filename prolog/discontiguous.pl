% ==========================================================
% Discontiguous Declarations
% ==========================================================
% All discontiguous predicate declarations for the
% monolithic build
% This file must be included BEFORE any clauses are defined

:- discontiguous(builtin_action/1).
% :- dynamic(builtin_action/1).

:- discontiguous(check_condition_impl/4).

:- dynamic(execute_action_impl/5).
:- discontiguous(execute_action_impl/5).

