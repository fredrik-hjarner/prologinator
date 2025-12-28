main :-
    expect(
        test_action_sequence(
            start_attrs([]),
            actions([
                log("before loop"),
                loop([
                    log("in loop"),
                    wait(1)
                ])
            ]),
            ticks(1),
            end_attrs([])
        ),
        '== FAILURE =='
    ),
    write('== SUCCESS =='), nl,
    halt.
