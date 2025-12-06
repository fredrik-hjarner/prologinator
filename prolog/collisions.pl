% Collision Detection Module
% Handles collision detection between game objects

:- module(collisions, [detect_collisions/3]).

:- use_module(library(lists), [
    findall/3, member/2, list_to_set/2, append/3
]).

% ==========================================================
% Collision Detection (simplified - grid-based)
% ==========================================================
% -list(game_object), -list(hint)).

detect_collisions(Objects, NewObjects, RevHints) :-
    findall(
        collision(ID1, ID2),
        (
            member(
                object(
                    id(ID1), _, attrs(A1), _, collisions(_)
                ),
                Objects
            ),
            member(
                object(
                    id(ID2), _, attrs(A2), _, collisions(_)
                ),
                Objects
            ),
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
            member(collision(ID1, ID2), Collisions),
            member(
                object(
                    id(ID1), type(proj), _, _, collisions(_)
                ),
                Objects
            ),
            member(
                object(
                    id(ID2),
                    type(enemy),
                    _,
                    _,
                    collisions(_)
                ),
                Objects
            ),
            ProjID = ID1, EnemyID = ID2
        ;
            member(collision(ID1, ID2), Collisions),
            member(
                object(
                    id(ID1),
                    type(enemy),
                    _,
                    _,
                    collisions(_)
                ),
                Objects
            ),
            member(
                object(
                    id(ID2),
                    type(proj),
                    _,
                    _,
                    collisions(_)
                ),
                Objects
            ),
            ProjID = ID2, EnemyID = ID1
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
    [object(
        id(ID), _, attrs(AttrList), _, collisions(_)
    )|Rest],
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

