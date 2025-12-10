% Collision Detection Module
% Handles collision detection between game objects

:- module(collisions, [detect_collisions/4]).

:- use_module(library(lists), [
    findall/3, member/2, list_to_set/2, append/3
]).
:- use_module(library(assoc), [gen_assoc/3]).
:- use_module('./types/accessors').

% ==========================================================
% Collision Detection (simplified - grid-based)
% ==========================================================
% -list(game_object), -list(hint)).

detect_collisions(State, Objects, NewObjects, RevHints) :-
    state_attrs(State, AttrStore),
    findall(
        collision(ID1, ID2),
        (
            member(Obj1, Objects),
            obj_id(Obj1, ID1),
            member(Obj2, Objects),
            obj_id(Obj2, ID2),
            ID1 @< ID2,
            gen_assoc(ID1, AttrStore, A1),
            gen_assoc(ID2, AttrStore, A2),
            member(attr(x, X1), A1),
            member(attr(y, Y1), A1),
            member(attr(x, X2), A2),
            member(attr(y, Y2), A2),
            collides_at(X1, Y1, X2, Y2)
        ),
        Collisions
    ),
    handle_collisions(
        State, Objects, Collisions, NewObjects, RevHints
    ).

collides_at(X, Y, X, Y).

handle_collisions(
    State,
    Objects,
    Collisions,
    NewObjects,
    RevHints
) :-
    findall(
        (ProjID, EnemyID),
        (
            member(collision(ProjID, EnemyID), Collisions),
            member(Obj1, Objects),
            obj_id_type(Obj1, ProjID, proj),
            member(Obj2, Objects),
            obj_id_type(Obj2, EnemyID, enemy)
        ;
            member(collision(EnemyID, ProjID), Collisions),
            member(Obj1, Objects),
            obj_id_type(Obj1, EnemyID, enemy),
            member(Obj2, Objects),
            obj_id_type(Obj2, ProjID, proj)
        ),
        ToRemove
    ),
    findall(ID, member((ID, _), ToRemove), P1),
    findall(ID, member((_, ID), ToRemove), P2),
    append(P1, P2, AllIDs),
    list_to_set(AllIDs, UniqueIDs),
    % TODO: I dont like that what happens at collision is
    % hard-coded like this.
    %       should be dynamic.
    remove_with_rev_hints(
        State, Objects, UniqueIDs, NewObjects, RevHints
    ).

% :- pred remove_with_rev_hints(+Objects,
% +IDsOfObjectsToRemove, -NewObjects, -RevHints).

remove_with_rev_hints(_State, [], _, [], []).
remove_with_rev_hints(
    State,
    [Obj|Rest],
    ToRemove,
    NewObjs,
    RevHints
) :-
    obj_id(Obj, ID),
    ( member(ID, ToRemove) ->
        state_attrs(State, AttrStore),
        ( gen_assoc(ID, AttrStore, Attrs) ->
            true
        ;
            Attrs = []
        ),
        RevHints = [despawned(ID, Attrs)|RestRevHints],
        remove_with_rev_hints(
            State, Rest, ToRemove, NewObjs, RestRevHints
        )
    ;
        NewObjs = [Obj|RestObjs],
        remove_with_rev_hints(
            State, Rest, ToRemove, RestObjs, RevHints
        )
    ).
