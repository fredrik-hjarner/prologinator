

key_down(KeyCode) -->
    ctx_events(Events),
    {member(event(key(KeyCode), down), Events)}.

key_up(KeyCode) -->
    ctx_events(Events),
    {member(event(key(KeyCode), up), Events)}.


key_held(KeyCode) -->
    ctx_held(Keys),
    {member(KeyCode, Keys)}.

