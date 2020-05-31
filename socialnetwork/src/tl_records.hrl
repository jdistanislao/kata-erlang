-author("japs").

-record(msg, {
    from,
    content,
    mentions = [],
    timestamp = erlang:monotonic_time()
}).

-record(tl_state, {
    user,
    token,
    messages = [],
    private_messages = [],
    subscriptions = []
}).
