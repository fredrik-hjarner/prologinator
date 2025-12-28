[
    log("before loop"),
    loop([ % crash here.
        log("in loop"),
        wait(1)
    ])
].
