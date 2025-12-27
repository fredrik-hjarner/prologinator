% Execute with SWI-Prolog!
:- use_module(library(lists)).

% 1. CONFIG: Operators
:- op(700, xfx, #=).
:- op(700, xfx, #>).
:- op(700, xfx, #<).
:- op(700, xfx, #>=).
:- op(700, xfx, #=<).
:- op(700, xfx, #\=).
:- op(700, xfx, in).
:- op(700, xfx, ins).
:- op(450, xfx, ..).

% 2. CONFIG: Blocklist
ignore_predicate(format_).
ignore_predicate(throw_error).
ignore_predicate(is_list).

% 3. MAIN
main :-
    current_prolog_flag(argv, Files),
    (   Files == []
    ->  format(user_error, 
               "Usage: swipl -g main~n", []),
        halt(1)
    ;   true
    ),
    maplist(read_file_safe, Files, Results),
    maplist(get_definitions, Results, 
            DefLists),
    append(DefLists, AllDefs),
    maplist(get_edges(AllDefs), Results, 
            EdgeLists),
    append(EdgeLists, AllEdges),
    write_dot(AllEdges),
    halt.

% 4. READER
read_file_safe(File, Terms) :-
    catch(
        setup_call_cleanup(
            open(File, read, In),
            read_stream(In, Terms),
            close(In)
        ),
        _,
        (format(
            user_error,
            "Skipping ~w because syntax errors~n", 
            [File]
        ),
         Terms = [])
    ).

read_stream(In, Terms) :-
    read_term(In, Term, []),
    (   Term == end_of_file 
    ->  Terms = []
    ;   Terms = [Term|Rest], 
        read_stream(In, Rest)
    ).

% 5. EXTRACTORS (name only)
get_definitions(Terms, Defs) :-
    findall(Name, 
        (member(T, Terms), term_name(T, Name)),
        Defs).

term_name((:- _), _) :- !, fail.
term_name((Head :- _), Name) :- 
    !, head_name(Head, Name).
term_name((Head --> _), Name) :- 
    !, head_name(Head, Name).
term_name(Head, Name) :- 
    head_name(Head, Name).

head_name(_:Head, Name) :- 
    !, functor(Head, Name, _).
head_name(Head, Name) :- 
    functor(Head, Name, _).

get_edges(KnownDefs, Terms, Edges) :-
    findall(Caller-Callee,
        (   member(T, Terms),
            term_name(T, Caller),
            term_body(T, Body),
            body_call(Body, Callee),
            member(Callee, KnownDefs),
            Caller \= Callee,
            \+ ignore_predicate(Callee)
        ),
        Edges).

term_body((_ :- Body), Body).
term_body((_ --> Body), Body).
term_body(_, true).

% 6. WALKER
body_call(Var, _) :- 
    var(Var), !, fail.
body_call(_:G, Name) :- 
    !, body_call(G, Name).
body_call((A, B), C) :- 
    !, (body_call(A, C) ; body_call(B, C)).
body_call((A; B), C) :- 
    !, (body_call(A, C) ; body_call(B, C)).
body_call((A->B), C) :- 
    !, (body_call(A, C) ; body_call(B, C)).
body_call(\+ A, C) :- 
    !, body_call(A, C).
body_call(call(A), C) :- 
    !, body_call(A, C).
body_call(Goal, Name) :- 
    compound(Goal), 
    functor(Goal, Name, _).
body_call(Goal, Name) :- 
    compound(Goal),
    arg(_, Goal, Arg),
    body_call(Arg, Name).
body_call(Goal, Name) :- 
    atom(Goal), 
    Goal \= true, 
    Goal \= [],
    Name = Goal.

% 7. WRITER
write_dot(Edges) :-
    format("digraph G {~n", []),
    format("  rankdir=LR;~n", []),
    format("  node [shape=box];~n", []),
    sort(Edges, Unique),
    maplist(print_edge, Unique),
    format("}~n", []).

print_edge(Caller-Callee) :-
    format("  \"~w\" -> \"~w\";~n", 
           [Caller, Callee]).