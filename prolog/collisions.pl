% Collision Detection Module
% Handles collision detection between game objects

:- module(collisions, [detect_collisions/3]).

:- use_module(library(lists), [
    findall/3, member/2, list_to_set/2, append/3
]).
:- use_module('./types/accessors', [
    obj_id_attrs/3,
    obj_id_type/3
]).

% ==========================================================
% Collision Detection (simplified - grid-based)
% ==========================================================
% -list(game_object), -list(hint)).

detect_collisions(Objects, NewObjects, RevHints) :-
    findall(
        collision(ID1, ID2),
        (
            member(Obj1, Objects),
            obj_id_attrs(Obj1, ID1, A1),
            member(Obj2, Objects),
            obj_id_attrs(Obj2, ID2, A2),
            ID1 @< ID2,
            member(pos(X1, Y1), A1),
            member(pos(X2, Y2), A2),
            collides_at(X1, Y1, X2, Y2)
        ),
        Collisions
    ),
    handle_collisions(
        Objects, Collisions, NewObjects, RevHints
    ).

collides_at(X, Y, X, Y).

handle_collisions(
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
        Objects, UniqueIDs, NewObjects, RevHints
    ).

% :- pred remove_with_rev_hints(+Objects,
% +IDsOfObjectsToRemove, -NewObjects, -RevHints).

remove_with_rev_hints([], _, [], []).
remove_with_rev_hints(
    [object(id(ID), _, attrs(AttrList), _, _)|Rest],
    ToRemove,
    NewObjs,
    [despawned(ID, AttrList)|RestRevHints]
) :-
    member(ID, ToRemove),
    !,
    remove_with_rev_hints(
        Rest, ToRemove, NewObjs, RestRevHints
    ).
remove_with_rev_hints(
    [Obj|Rest], ToRemove, [Obj|NewObjs], RevHints
) :-
    remove_with_rev_hints(
        Rest, ToRemove, NewObjs, RevHints
    ).

