
expect(Goal, Message) :- (
    Goal ->
        true
    ;
        write(user_output, 'ERROR: Assertion failed: '),
        write(user_output, Message),
        nl,
        halt(1)
).

expect(Goal) :- (
    Goal ->
        true
    ;
        write(user_output, 'ERROR: Assertion failed: '),
        write_term(user_output, Goal,
            [quoted(true), max_depth(3)]),
        nl,
        halt(1)
).
