

replace_macros(Term, Term) :-
    var(Term), !.

replace_macros(Term, Replaced) :-
    compound(Term),
    functor(Term, def, 1),
    arg(1, Term, Name),
    atom(Name),
    macro_val(Name, Value),
    !,
    Replaced = Value.

replace_macros([H|T], [H2|T2]) :-
    !,
    replace_macros(H, H2),
    replace_macros(T, T2).

replace_macros(TermIn, TermOut) :-
    compound(TermIn),
    !,
    TermIn =.. [Functor | ArgsIn],
    maplist(replace_macros, ArgsIn, ArgsOut),
    TermOut =.. [Functor | ArgsOut].

replace_macros(Term, Term).


user:term_expansion(Term, []) :-
    nonvar(Term),
    Term = define(Name, Value),
    atom(Name),
    !,
    (retractall(macro_val(Name, _)); true),
    assertz(macro_val(Name, Value)).

user:term_expansion(TermIn, TermOut) :-
    nonvar(TermIn),
    \+ (TermIn = define(_, _)),
    \+ (TermIn = (:- _)),
    \+ (functor(TermIn, ':-', _)),
    \+ (functor(TermIn, term_expansion, _)),
    \+ (functor(TermIn, replace_macros, _)),
    replace_macros(TermIn, TermOut),
    TermIn \== TermOut.


run_tests :-
    findall(
        T,
        (
            test(T, G),
            ( G ->
                format("PASS: ~s~n", [T])
            ;
                format("FAIL: ~s~n", [T])
            )
        ),
        _
    ).

set_def(K, V) :-
    ( retractall(macro_val(K, _))
    ; true
    ),
    assertz(macro_val(K, V)).
clr_def(K)    :- retractall(macro_val(K, _)).

test("basic replacement", (
    set_def(x, 10),
    replace_macros(def(x), R),
    R == 10,
    clr_def(x)
)).

test("nested compound replacement", (
    set_def(width, 100),
    replace_macros(rect(def(width), 200), R),
    R == rect(100, 200),
    clr_def(width)
)).

test("list replacement", (
    set_def(a, 1),
    set_def(b, 2),
    replace_macros([def(a), def(b), 3], R),
    R == [1, 2, 3],
    clr_def(a), clr_def(b)
)).

test("deep nesting", (
    set_def(deep, success),
    replace_macros(a(b(c([d, def(deep)]))), R),
    R == a(b(c([d, success]))),
    clr_def(deep)
)).

test("undefined macro stays as-is", (
    replace_macros(def(undefined_thing), R),
    R == def(undefined_thing)
)).

test("complex value injection", (
    set_def(pos, point(10, 20)),
    replace_macros(player(def(pos)), R),
    R == player(point(10, 20)),
    clr_def(pos)
)).

