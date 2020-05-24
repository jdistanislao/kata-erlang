-module(socialnetwork_tests).
-author("japs").

-include_lib("eunit/include/eunit.hrl").

start_tl(Users) ->
    StartTimelines = fun(User, Refs) ->
        {ok, Ref} = timeline:start(User),
        [Ref | Refs]
        end,
    Refs = lists:foldl(StartTimelines, [], Users),
    lists:reverse(Refs).

stop_tl(Timelines) ->
    lists:foreach(fun({Tl, _}) -> ?assertMatch(ok, gen_server:stop(Tl)) end, Timelines).

alice_can_create_her_timeline_test() ->
    Refs = start_tl([alice]),
    {Pid, Token} = lists:last(Refs),
    ?assert(is_pid(Pid)),
    ?assert(is_reference(Token)),
    stop_tl(Refs).

alice_can_view_her_timeline_test() ->
    Refs = start_tl([alice]),
    {ok, Messages} = timeline:get_messages(alice),
    ?assertMatch([], Messages),
    stop_tl(Refs).

alice_can_post_messages_to_her_personal_timeline_test() ->
    Refs = start_tl([alice]),
    {_, Token} = lists:last(Refs),
    PostResponse = timeline:post(alice, Token, "first"),
    {ok, Messages} = timeline:get_messages(alice),
    ?assertMatch(ok, PostResponse),
    ?assertMatch(["first"], Messages),
    stop_tl(Refs).

messages_are_shown_in_reverse_time_order_test() ->
    Refs = start_tl([alice]),
    {_, Token} = lists:last(Refs),
    timeline:post(alice, Token, "first"),
    timeline:post(alice, Token, "second"),
    {ok, Messages} = timeline:get_messages(alice),
    ?assertMatch(["second","first"], Messages),
    stop_tl(Refs).

alice_could_not_post_messages_to_bob_timeline_test() ->
    Refs = start_tl([alice, bob]),
    [{_, AliceToken}, _] = Refs,
    timeline:post(bob, AliceToken, "not allowed"),
    timeline:post(alice, AliceToken, "first"),
    {ok, BobMessages} = timeline:get_messages(bob),
    {ok, AliceMessages} = timeline:get_messages(alice),
    ?assertMatch([], BobMessages),
    ?assertMatch(["first"], AliceMessages),
    stop_tl(Refs).

%%Useless :)
alice_can_view_bob_timeline_test() ->
    ?assert(true).

charlie_can_subscribe_to_alice_timeline_test() ->
    Refs = start_tl([alice, charlie, bob]),
    [{_, AliceToken}, {_, CharlieToken}, {_, BobToken}] = Refs,
    timeline:post(bob, BobToken, "first B"),
    timeline:post(alice, AliceToken, "first A"),
    timeline:post(charlie, CharlieToken, "first C"),
    timeline:subscribe(charlie, CharlieToken, [alice, bob]),
    {ok, Messages} = timeline:get_messages(charlie),
    ?assertMatch(["first C", "first A", "first B"], Messages),
    stop_tl(Refs).

prevent_multiple_subscribe_to_same_timeline_test() ->
    Refs = start_tl([alice, charlie]),
    [{_, AliceToken}, {_, CharlieToken}] = Refs,
    timeline:post(alice, AliceToken, "first A"),
    timeline:post(charlie, CharlieToken, "first C"),
    timeline:subscribe(charlie, CharlieToken, [alice, alice]),
    timeline:subscribe(charlie, CharlieToken, [alice]),
    {ok, Messages} = timeline:get_messages(charlie),
    ?assertMatch(["first C", "first A"], Messages),
    stop_tl(Refs).


