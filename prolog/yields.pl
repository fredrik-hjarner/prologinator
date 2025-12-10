% Yields Module
% Determines whether actions yield (take time) or complete
% immediately

:- module(yields, [yields/2]).

:- use_module(library(clpz)).
:- use_module('./execute_action', [user_action/2]).

% ==========================================================
% Yielding Actions (Reified)
% ==========================================================
% Bidirectional: works forward (check if action yields) and
% backward (generate yielding actions)
% Modes: yields(?Action, ?T) - can be called with Action
% bound or unbound, T is true or false
% :- pred yields(?Action, ?T) # "Reified: T=true if action
% yields, T=false otherwise. Can generate yielding actions
% when Action is unbound and T=true.".

% yields tells whether an action "yields" or not.
% This is how it works now: AFTER an action has been
% executed, we check if it yields. If it does yield then
% we DONT execute the next action. However it it does NOT
% yield then we execute the next action etc until EITHER
% the game object cease to exist or we come to a yielding
% action.
% Reified version: always takes a truth value T.
% Fully relational: pattern matching + CLP constraints.

% wait(N) yields when N > 0
yields(wait(N), true) :- N #> 0.
yields(wait(N), false) :- N #=< 0.

% move_to(_, _, Frames) yields when Frames > 0
yields(move_to(_, _, Frames), true) :- Frames #> 0.
yields(move_to(_, _, Frames), false) :- Frames #=< 0.

% parallel_all_running(_) always yields
yields(parallel_all_running(_), true).
% parallel_race_running(_) always yields
yields(parallel_race_running(_), true).

% Non-yielding actions: despawn, spawn, loop,
% trigger_state_change, parallel_all, noop, list, set_attr,
% repeat, define_action
yields(despawn, false).
yields(spawn(_, _, _, _), false).
yields(loop(_), false).
yields(trigger_state_change(_), false).
yields(parallel_all(_), false).
% parallel_race(_) always yields false
%   (will be unwrapped immediately if
%   any child finishes)
yields(parallel_race(_), false).
yields(noop, false).
yields(list(_), false).
yields(set_attr(_, _), false).
% repeat(Times, Actions) does NOT yield
%   (expands immediately into action list)
yields(repeat(_, _), false).
% define_action(_, _) does NOT yield
%   (expands immediately, stores definition)
yields(define_action(_, _), false).
% load(_) does NOT yield (expands immediately)
yields(load(_), false).
% log(_) does NOT yield (expands immediately)
yields(log(_), false).

% move_delta(Frames, _, _) yields when Frames > 0
yields(move_delta(Frames, _, _), true) :-
    Frames #> 0.
yields(move_delta(Frames, _, _), false) :-
    Frames #=< 0.

% Input actions
yields(wait_key_down(_), true).
yields(wait_key_up(_), true).
yields(wait_key_held(_), true).

% Custom actions (user-defined via define_action)
% do NOT yield - they always expand immediately
yields(Action, false) :-
    user_action(Template, _),
    Action = Template.

% If we reach here, action matched nothing above
% Safe default: don't yield
% TODO: Look into if this can be removed. Might mask errors.
yields(_, false).

