-module(timeline_tests).
-author("japs").

-include_lib("eunit/include/eunit.hrl").

-define(SETUP(Fn), {setup, fun setup/0, fun teardown/1, Fn}).
-define(USERS, [alice, bob, charlie]).


posting_test_() ->
    [
        {"Posting tests", ?SETUP(fun alice_can_create_her_timeline_instantiator/1)}
    ].

setup() ->
    StartTimelines = fun(User, TlRefs) ->
        {ok, Ref} = timeline:start(User),
        [Ref | TlRefs]
        end,
    TlRefs = lists:foldl(StartTimelines, [], ?USERS),
    lists:reverse(TlRefs).

teardown(TlRefs) ->
    lists:foreach(fun({Tl, _}) -> ?assertMatch(ok, gen_server:stop(Tl)) end, TlRefs).



alice_can_create_her_timeline_instantiator(TlRefs) ->
    [{Pid, Token}, _, _] = TlRefs,
    [
        ?_assert(is_pid(Pid)),
        ?_assert(is_reference(Token))
    ].



%%alice_can_create_her_timeline_test() ->
%%    TlRefs = setup(),
%%    [{Pid, Token}, _, _] = TlRefs,
%%    ?assert(is_pid(Pid)),
%%    ?assert(is_reference(Token)),
%%    teardown(TlRefs).
%%
%%alice_can_view_her_timeline_test() ->
%%    TlRefs = setup(),
%%    {ok, Messages} = timeline:get_messages(alice),
%%    ?assertMatch([], Messages),
%%    teardown(TlRefs).
%%
%%alice_can_post_messages_to_her_personal_timeline_test() ->
%%    TlRefs = setup(),
%%    [{_, Token}, _, _] = TlRefs,
%%    PostResponse = timeline:post(alice, Token, "first"),
%%    {ok, Messages} = timeline:get_messages(alice),
%%    ?assertMatch(ok, PostResponse),
%%    ?assertMatch(["first"], Messages),
%%    teardown(TlRefs).
%%
%%messages_are_shown_in_reverse_time_order_test() ->
%%    TlRefs = setup(),
%%    [{_, Token}, _, _] = TlRefs,
%%    timeline:post(alice, Token, "first"),
%%    timeline:post(alice, Token, "second"),
%%    {ok, Messages} = timeline:get_messages(alice),
%%    ?assertMatch(["second","first"], Messages),
%%    teardown(TlRefs).
%%
%%alice_could_not_post_messages_to_bob_timeline_test() ->
%%    TlRefs = setup(),
%%    [{_, AliceToken}, _, _] = TlRefs,
%%    timeline:post(bob, AliceToken, "not allowed"),
%%    timeline:post(alice, AliceToken, "first"),
%%    {ok, BobMessages} = timeline:get_messages(bob),
%%    {ok, AliceMessages} = timeline:get_messages(alice),
%%    ?assertMatch([], BobMessages),
%%    ?assertMatch(["first"], AliceMessages),
%%    teardown(TlRefs).
%%
%%charlie_can_subscribe_to_alice_timeline_test() ->
%%    TlRefs = setup(),
%%    [{_, AliceToken}, {_, BobToken}, {_, CharlieToken}] = TlRefs,
%%    timeline:post(bob, BobToken, "first B"),
%%    timeline:post(alice, AliceToken, "first A"),
%%    timeline:post(charlie, CharlieToken, "first C"),
%%    timeline:subscribe(charlie, CharlieToken, [alice, bob]),
%%    {ok, Messages} = timeline:get_messages(charlie),
%%    ?assertMatch(["first C", "first A", "first B"], Messages),
%%    teardown(TlRefs).
%%
%%prevent_multiple_subscribe_to_same_timeline_test() ->
%%    TlRefs = setup(),
%%    [{_, AliceToken}, _, {_, CharlieToken}] = TlRefs,
%%    timeline:post(alice, AliceToken, "first A"),
%%    timeline:post(charlie, CharlieToken, "first C"),
%%    timeline:subscribe(charlie, CharlieToken, [alice, alice]),
%%    timeline:subscribe(charlie, CharlieToken, [alice]),
%%    {ok, Messages} = timeline:get_messages(charlie),
%%    ?assertMatch(["first C", "first A"], Messages),
%%    teardown(TlRefs).
%%
%%user_can_view_its_private_messages_test() ->
%%    TlRefs = setup(),
%%    [{_, AliceToken}, _, _] = TlRefs,
%%    {ok, AliceMessages} = timeline:get_private_messages(alice, AliceToken),
%%    ?assertMatch([], AliceMessages),
%%    teardown(TlRefs).
%%
%%user_cant_view_other_users_private_messages_test() ->
%%    TlRefs = setup(),
%%    {error, Response} = timeline:get_private_messages(alice, "AnotherUserToken"),
%%    ?assertMatch(not_allowed, Response),
%%    teardown(TlRefs).
%%
%%user_can_send_private_messages_test() ->
%%    TlRefs = setup(),
%%    [{_, AliceToken}, _, _] = TlRefs,
%%    timeline:send_private_message(mallory, alice, "Hi from Mallory"),
%%    timeline:send_private_message(bob, alice, "Hi from Bob"),
%%    {ok, AlicePrivateMessages} = timeline:get_private_messages(alice, AliceToken),
%%    ?assertMatch([{bob, "Hi from Bob"}, {mallory, "Hi from Mallory"}], AlicePrivateMessages),
%%    teardown(TlRefs).


