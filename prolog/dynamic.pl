% ==========================================================
% Dynamic Declarations
% ==========================================================
% All dynamic predicate declarations for the monolithic
% build
% This file must be included BEFORE any clauses are defined

:- dynamic(execute_action_impl/5). % I'm sad this is needed.
:- dynamic(user_action/2).

