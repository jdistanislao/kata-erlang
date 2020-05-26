-module(timeline_tests).
-author("japs").

-include_lib("eunit/include/eunit.hrl").

-define(USERS, [alice, bob, charlie]).

start_tl(_) ->
    StartTimelines = fun(User, Refs) ->
        {ok, Ref} = timeline:start(User),
        [Ref | Refs]
        end,
    Refs = lists:foldl(StartTimelines, [], ?USERS),
    lists:reverse(Refs).

stop_tl(Timelines) ->
    lists:foreach(fun({Tl, _}) -> ?assertMatch(ok, gen_server:stop(Tl)) end, Timelines).

alice_can_create_her_timeline_test() ->
    Refs = start_tl([alice]),
    [{Pid, Token}, _, _] = Refs,
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
    [{_, Token}, _, _] = Refs,
    PostResponse = timeline:post(alice, Token, "first"),
    {ok, Messages} = timeline:get_messages(alice),
    ?assertMatch(ok, PostResponse),
    ?assertMatch(["first"], Messages),
    stop_tl(Refs).

messages_are_shown_in_reverse_time_order_test() ->
    Refs = start_tl([alice]),
    [{_, Token}, _, _] = Refs,
    timeline:post(alice, Token, "first"),
    timeline:post(alice, Token, "second"),
    {ok, Messages} = timeline:get_messages(alice),
    ?assertMatch(["second","first"], Messages),
    stop_tl(Refs).

alice_could_not_post_messages_to_bob_timeline_test() ->
    Refs = start_tl([alice, bob]),
    [{_, AliceToken}, _, _] = Refs,
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
    [{_, AliceToken}, {_, BobToken}, {_, CharlieToken}] = Refs,
    timeline:post(bob, BobToken, "first B"),
    timeline:post(alice, AliceToken, "first A"),
    timeline:post(charlie, CharlieToken, "first C"),
    timeline:subscribe(charlie, CharlieToken, [alice, bob]),
    {ok, Messages} = timeline:get_messages(charlie),
    ?assertMatch(["first C", "first A", "first B"], Messages),
    stop_tl(Refs).

prevent_multiple_subscribe_to_same_timeline_test() ->
    Refs = start_tl([alice, charlie]),
    [{_, AliceToken}, _, {_, CharlieToken}] = Refs,
    timeline:post(alice, AliceToken, "first A"),
    timeline:post(charlie, CharlieToken, "first C"),
    timeline:subscribe(charlie, CharlieToken, [alice, alice]),
    timeline:subscribe(charlie, CharlieToken, [alice]),
    {ok, Messages} = timeline:get_messages(charlie),
    ?assertMatch(["first C", "first A"], Messages),
    stop_tl(Refs).

user_can_view_its_private_messages_test() ->
    Refs = start_tl([alice]),
    [{_, AliceToken}, _, _] = Refs,
    {ok, AliceMessages} = timeline:get_private_messages(alice, AliceToken),
    ?assertMatch([], AliceMessages),
    stop_tl(Refs).

user_cant_view_other_users_private_messages_test() ->
    Refs = start_tl([alice]),
    {error, Response} = timeline:get_private_messages(alice, "AnotherUserToken"),
    ?assertMatch(not_allowed, Response),
    stop_tl(Refs).

user_can_send_private_messages_test() ->
    Refs = start_tl([alice]),
    [{_, AliceToken}, _, _] = Refs,
    timeline:send_private_message(mallory, alice, "Hi from Mallory"),
    timeline:send_private_message(bob, alice, "Hi from Bob"),
    {ok, AlicePrivateMessages} = timeline:get_private_messages(alice, AliceToken),
    ?assertMatch([{bob, "Hi from Bob"}, {mallory, "Hi from Mallory"}], AlicePrivateMessages),
    stop_tl(Refs).


