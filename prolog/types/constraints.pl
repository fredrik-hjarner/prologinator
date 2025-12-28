

bounded_list_of(Goal, List, MaxLen) :-
( ground(List)
-> % Fast path: List is already ground (validation mode)
   length(List, Len),
   Len =< MaxLen,
   maplist(Goal, List)
;  % Constraint path: List is unbound (generation mode)
   between(0, MaxLen, Len),
   length(List, Len),
   maplist(Goal, List)
).

bounded_list_of_depth(Goal, List, MaxLen, DepthLeft) :-
    between(0, MaxLen, Len),
    length(List, Len),
    maplist_with_depth(Goal, List, DepthLeft).

maplist_with_depth(_, [], _).
maplist_with_depth(Goal, [H|T], DepthLeft) :-
    call(Goal, H, DepthLeft),
    maplist_with_depth(Goal, T, DepthLeft).

last([X], X).
last([_|T], X) :-
    last(T, X).


state_constraint(
  state(
      frame(Frame),
      objects(Objects),
      attrs(_Attrs),
      status(Status),
      next_id(NextID),
      commands(Commands),
      actionstore(_ActionStore)
  )
) :-
    Frame #>= 0,
    bounded_list_of(object_constraint, Objects, 200),
    
    maplist(obj_id, Objects, IDs),
    ( ground(IDs)
    ->
        is_ascending(IDs)        % Simple check, no CLP(FD)
    ;
        chain(#<, IDs)            % CLP(FD) for generation
    ),
    
    (IDs = [] -> MaxID = -1 ; last(IDs, MaxID)),
    NextID #> MaxID,
    
    game_status_constraint(Status),
    bounded_list_of(command_constraint, Commands, 100).


game_status_constraint(playing).
game_status_constraint(won).
game_status_constraint(lost).


object_constraint(
  object(
      id(ID),
      actions(Actions)
  )
) :-
    ID #>= 0,
    ID #=< 1000,
    bounded_list_of(action_constraint, Actions, 100).


object_type_constraint(static).
object_type_constraint(enemy).
object_type_constraint(proj).
object_type_constraint(player).
object_type_constraint(tower).  % Used in game.pl


attribute_constraint(_).  % Accept any attribute

attr_constraint(A) :- attribute_constraint(A).


action_constraint(A) :- action_constraint(A, 10).

action_constraint(wait(N), _) :- 
    N #>= 0.

action_constraint(move_to(X, Y, Frames), _) :-
    X in -10..200,
    Y in -10..200,
    Frames #> 0.

action_constraint(despawn, _).

action_constraint(spawn(Acts), DepthLeft) :-
    DepthLeft #> 0,
    DepthLeft1 #= DepthLeft - 1,
    bounded_list_of_depth(
        action_constraint, Acts, 100, DepthLeft1
    ).

action_constraint(loop(Acts), DepthLeft) :-
    DepthLeft #> 0,
    DepthLeft1 #= DepthLeft - 1,
    bounded_list_of_depth(
        action_constraint, Acts, 30, DepthLeft1
    ).

action_constraint(trigger_state_change(Change), _) :-
    state_change_constraint(Change).

action_constraint(parallel_all(Children), DepthLeft) :-
    DepthLeft #> 0,
    DepthLeft1 #= DepthLeft - 1,
    bounded_list_of_depth(
        action_constraint, Children, 100, DepthLeft1
    ).

action_constraint(parallel_race(Children), DepthLeft) :-
    DepthLeft #> 0,
    DepthLeft1 #= DepthLeft - 1,
    bounded_list_of_depth(
        action_constraint, Children, 100, DepthLeft1
    ).

action_constraint(
    parallel_all_running(Children), DepthLeft
) :-
    DepthLeft #> 0,
    DepthLeft1 #= DepthLeft - 1,
    bounded_list_of_depth(
        action_constraint, Children, 100, DepthLeft1
    ).


pos_constraint(_).  % Accept any position structure


command_constraint(spawn_request(Type, _X, _Y, Acts)) :-
    object_type_constraint(Type),
    bounded_list_of(action_constraint, Acts, 100).

command_constraint(state_change(Change)) :-
    state_change_constraint(Change).


state_change_constraint(game_over(won)).
state_change_constraint(game_over(lost)).


collision_constraint(_).


is_ascending([]).
is_ascending([_]).
is_ascending([A, B|Rest]) :-
    A < B,                    % Simple comparison, not #
    is_ascending([B|Rest]).