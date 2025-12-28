

normalize_comparison(Comparison, Normalized) :-
    Comparison =.. [Op, Left, Right],
    (   op_map(Op, CLP_Op)
    ->
        Normalized =.. [CLP_Op, Left, Right]
    ;
        Normalized = Comparison
    ).


op_map(  <,  #< ).
op_map(  >,  #> ).
op_map( =<, #=< ).
op_map( >=, #>= ).
op_map(  =, #= ).
op_map( \=, #\= ).

op_map( #<,  #< ).
op_map( #>,  #> ).
op_map( #=<, #=< ).
op_map( #>=, #>= ).
op_map( #=,  #= ).
op_map( #\=, #\= ).
