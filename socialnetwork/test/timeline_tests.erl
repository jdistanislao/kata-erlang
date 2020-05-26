-module(timeline_tests).
-author("japs").

-include_lib("eunit/include/eunit.hrl").

-define(SETUP(Fn), {setup, fun setup/0, fun teardown/1, Fn}).
-define(USERS, [alice, bob, charlie]).

%%
%% TESTS DESCRIPTIONS
%%

all_test_() ->
    [
        {"Reading tests", [
            {"Alice can create her timeline", ?SETUP(fun alice_can_create_her_timeline_test_/1)},
            {"Alice can view her timeline", ?SETUP(fun alice_can_view_her_timeline_test_/1)},
            {"Messages are shown in reverse time order", ?SETUP(fun messages_are_shown_in_reverse_time_order_test_/1)}
        ]},
        {"Posting tests", [
            {"Alice can post messages to her personal timeline", ?SETUP(fun alice_can_post_messages_to_her_personal_timeline_test_/1)},
            {"Alice could not post messages to bob timeline", ?SETUP(fun alice_could_not_post_messages_to_bob_timeline_test_/1)}
        ]},
        {"Following tests", [
            {"Charlie can subscribe to Alice timeline", ?SETUP(fun charlie_can_subscribe_to_alice_timeline_test_/1)},
            {"Prevent multiple subscribe to same timeline", ?SETUP(fun prevent_multiple_subscribe_to_same_timeline_test_/1)}
        ]},
        {"Direct messages", [
            {"User can view its private messages", ?SETUP(fun user_can_view_its_private_messages_test_/1)},
            {"User can't view other users private messages", ?SETUP(fun user_cant_view_other_users_private_messages_test_/1)},
            {"User can send private messages", ?SETUP(fun user_can_send_private_messages_test_/1)}
        ]}
    ].

%%
%% SETUP
%%

setup() ->
    StartTimelines = fun(User, TlRefs) ->
        {ok, Ref} = timeline:start(User),
        [Ref | TlRefs]
        end,
    TlRefs = lists:foldl(StartTimelines, [], ?USERS),
    lists:reverse(TlRefs).

teardown(TlRefs) ->
    lists:foreach(fun({Tl, _}) -> ?assertMatch(ok, gen_server:stop(Tl)) end, TlRefs).

%%
%% TESTS DEFINITIONS
%%

alice_can_create_her_timeline_test_(TlRefs) ->
    [{Pid, Token}, _, _] = TlRefs,
    ?_assert(is_pid(Pid)),
    ?_assert(is_reference(Token)).

alice_can_view_her_timeline_test_(_) ->
    {ok, Messages} = timeline:get_messages(alice),
    ?_assertMatch([], Messages).

messages_are_shown_in_reverse_time_order_test_(TlRefs) ->
    [{_, Token}, _, _] = TlRefs,
    timeline:post(alice, Token, "first"),
    timeline:post(alice, Token, "second"),
    {ok, Messages} = timeline:get_messages(alice),
    ?_assertMatch(["second","first"], Messages).

alice_can_post_messages_to_her_personal_timeline_test_(TlRefs) ->
    [{_, Token}, _, _] = TlRefs,
    PostResponse = timeline:post(alice, Token, "first"),
    {ok, Messages} = timeline:get_messages(alice),
    ?_assertMatch(ok, PostResponse),
    ?_assertMatch(["first"], Messages).

alice_could_not_post_messages_to_bob_timeline_test_(TlRefs) ->
    [{_, AliceToken}, _, _] = TlRefs,
    timeline:post(bob, AliceToken, "not allowed"),
    timeline:post(alice, AliceToken, "first"),
    {ok, BobMessages} = timeline:get_messages(bob),
    {ok, AliceMessages} = timeline:get_messages(alice),
    ?_assertMatch([], BobMessages),
    ?_assertMatch(["first"], AliceMessages).

charlie_can_subscribe_to_alice_timeline_test_(TlRefs) ->
    [{_, AliceToken}, {_, BobToken}, {_, CharlieToken}] = TlRefs,
    timeline:post(bob, BobToken, "first B"),
    timeline:post(alice, AliceToken, "first A"),
    timeline:post(charlie, CharlieToken, "first C"),
    timeline:subscribe(charlie, CharlieToken, [alice, bob]),
    {ok, Messages} = timeline:get_messages(charlie),
    ?_assertMatch(["first C", "first A", "first B"], Messages).

prevent_multiple_subscribe_to_same_timeline_test_(TlRefs) ->
    [{_, AliceToken}, _, {_, CharlieToken}] = TlRefs,
    timeline:post(alice, AliceToken, "first A"),
    timeline:post(charlie, CharlieToken, "first C"),
    timeline:subscribe(charlie, CharlieToken, [alice, alice]),
    timeline:subscribe(charlie, CharlieToken, [alice]),
    {ok, Messages} = timeline:get_messages(charlie),
    ?_assertMatch(["first C", "first A"], Messages).

user_can_view_its_private_messages_test_(TlRefs) ->
    [{_, AliceToken}, _, _] = TlRefs,
    {ok, AliceMessages} = timeline:get_private_messages(alice, AliceToken),
    ?_assertMatch([], AliceMessages).

user_cant_view_other_users_private_messages_test_(_) ->
    {error, Response} = timeline:get_private_messages(alice, "AnotherUserToken"),
    ?_assertMatch(not_allowed, Response).

user_can_send_private_messages_test_(TlRefs) ->
    [{_, AliceToken}, _, _] = TlRefs,
    timeline:send_private_message(mallory, alice, "Hi from Mallory"),
    timeline:send_private_message(bob, alice, "Hi from Bob"),
    {ok, AlicePrivateMessages} = timeline:get_private_messages(alice, AliceToken),
    ?_assertMatch([{bob, "Hi from Bob"}, {mallory, "Hi from Mallory"}], AlicePrivateMessages).


