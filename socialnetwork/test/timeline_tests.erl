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
            {"Alice can create her timeline",
                ?SETUP(fun alice_can_create_her_timeline_test_/1)},
            {"Alice can view her timeline",
                ?SETUP(fun alice_can_view_her_timeline_test_/1)},
            {"Messages are shown in reverse time order",
                ?SETUP(fun messages_are_shown_in_reverse_time_order_test_/1)}
        ]},
        {"Posting tests", [
            {"Alice can post messages to her personal timeline",
                ?SETUP(fun alice_can_post_messages_to_her_personal_timeline_test_/1)},
            {"Alice could not post messages to bob timeline",
                ?SETUP(fun alice_could_not_post_messages_to_bob_timeline_test_/1)}
        ]},
        {"Following tests", [
            {"Charlie can subscribe to Alice timeline",
                ?SETUP(fun charlie_can_subscribe_to_alice_timeline_test_/1)},
            {"Prevent multiple subscribe to same timeline",
                ?SETUP(fun prevent_multiple_subscribe_to_same_timeline_test_/1)}
        ]},
        {"Direct messages", [
            {"User can view its private messages",
                ?SETUP(fun user_can_view_its_private_messages_test_/1)},
            {"User can't view other users private messages",
                ?SETUP(fun user_cant_view_other_users_private_messages_test_/1)},
            {"User can send private messages",
                ?SETUP(fun user_can_send_private_messages_test_/1)}
        ]},
        {"Mentions", [
            {"messages_without_at_do_not_contain_mentions_test_",
                ?SETUP(fun messages_without_at_do_not_contain_mentions_test_/1)},
            {"User can mention another one using @",
                ?SETUP(fun user_can_mention_another_one_using_at_char_test_/1)},
            {"Mention char must be followed by a username",
                ?SETUP(fun mention_char_must_be_followed_by_a_username_test_/1)},
            {"Mentioned user must exists",
                ?SETUP(fun mentioned_user_must_exists_test_/1)}
%%            {"Mentioned users can view the message they are mentioned from",
%%                ?SETUP(fun fail/1)}
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

fail(_) ->
    ?_assert(false).

alice_can_create_her_timeline_test_(TlRefs) ->
    [{Pid, Token}, _, _] = TlRefs,
    ?_assert(is_pid(Pid)),
    ?_assert(is_reference(Token)).

alice_can_view_her_timeline_test_(_) ->
    Response = timeline:get_messages(alice),
    ?_assertMatch({ok, []}, Response).

messages_are_shown_in_reverse_time_order_test_(TlRefs) ->
    [{_, Token}, _, _] = TlRefs,
    timeline:post(alice, Token, "first"),
    timeline:post(alice, Token, "second"),
    Response = timeline:get_messages(alice),
    ?_assertMatch({ok, [{"second",_},{"first",_}]}, Response).

alice_can_post_messages_to_her_personal_timeline_test_(TlRefs) ->
    [{_, Token}, _, _] = TlRefs,
    PostResponse = timeline:post(alice, Token, "first"),
    GetResponse = timeline:get_messages(alice),
    [
        ?_assertMatch(ok, PostResponse),
        ?_assertMatch({ok, [{"first",_}]}, GetResponse)
    ].

alice_could_not_post_messages_to_bob_timeline_test_(TlRefs) ->
    [{_, AliceToken}, _, _] = TlRefs,
    timeline:post(bob, AliceToken, "not allowed"),
    timeline:post(alice, AliceToken, "first"),
    BobMessages = timeline:get_messages(bob),
    AliceMessages = timeline:get_messages(alice),
    [
        ?_assertMatch({ok, []}, BobMessages),
        ?_assertMatch({ok, [{"first",_}]}, AliceMessages)
    ].

charlie_can_subscribe_to_alice_timeline_test_(TlRefs) ->
    [{_, AliceToken}, {_, BobToken}, {_, CharlieToken}] = TlRefs,
    timeline:post(bob, BobToken, "first B"),
    timeline:post(alice, AliceToken, "first A"),
    timeline:post(charlie, CharlieToken, "first C"),
    timeline:subscribe(charlie, CharlieToken, [alice, bob]),
    Messages = timeline:get_messages(charlie),
    ?_assertMatch({ok, [{"first C",_}, {"first A",_}, {"first B",_}]}, Messages).

prevent_multiple_subscribe_to_same_timeline_test_(TlRefs) ->
    [{_, AliceToken}, _, {_, CharlieToken}] = TlRefs,
    timeline:post(alice, AliceToken, "first A"),
    timeline:post(charlie, CharlieToken, "first C"),
    timeline:subscribe(charlie, CharlieToken, [alice, alice]),
    timeline:subscribe(charlie, CharlieToken, [alice]),
    Messages = timeline:get_messages(charlie),
    ?_assertMatch({ok, [{"first C",_}, {"first A",_}]}, Messages).

user_can_view_its_private_messages_test_(TlRefs) ->
    [{_, AliceToken}, _, _] = TlRefs,
    Messages = timeline:get_private_messages(alice, AliceToken),
    ?_assertMatch({ok, []}, Messages).

user_cant_view_other_users_private_messages_test_(_) ->
    Response = timeline:get_private_messages(alice, "AnotherUserToken"),
    ?_assertMatch({error, not_allowed}, Response).

user_can_send_private_messages_test_(TlRefs) ->
    [{_, AliceToken}, _, _] = TlRefs,
    timeline:send_private_message(mallory, alice, "Hi from Mallory"),
    timeline:send_private_message(bob, alice, "Hi from Bob"),
    Messages = timeline:get_private_messages(alice, AliceToken),
    ?_assertMatch({ok, [{bob, "Hi from Bob", _}, {mallory, "Hi from Mallory", _}]}, Messages).


messages_without_at_do_not_contain_mentions_test_(TlRefs) ->
    [{_, Token}, _, _] = TlRefs,
    PostResponse = timeline:post(alice, Token, "first foo"),
    GetResponse = timeline:get_messages(alice),
    [
        ?_assertMatch(ok, PostResponse),
        ?_assertMatch({ok, [{"first foo",[]}]}, GetResponse)
    ].

user_can_mention_another_one_using_at_char_test_(TlRefs) ->
    [{_, Token}, _, _] = TlRefs,
    PostResponse = timeline:post(alice, Token, "mention @bob"),
    GetResponse = timeline:get_messages(alice),
    [
        ?_assertMatch(ok, PostResponse),
        ?_assertMatch({ok, [{_, [bob]}]}, GetResponse)
    ].

mention_char_must_be_followed_by_a_username_test_(TlRefs) ->
    [{_, Token}, _, _] = TlRefs,
    PostResponse = timeline:post(alice, Token, "mention @ bob"),
    GetResponse = timeline:get_messages(alice),
    [
        ?_assertMatch(ok, PostResponse),
        ?_assertMatch({ok, [{_, []}]}, GetResponse)
    ].

mentioned_user_must_exists_test_(TlRefs) ->
    [{_, Token}, _, _] = TlRefs,
    PostResponse = timeline:post(alice, Token, "mention @nonexistinguser"),
    GetResponse = timeline:get_messages(alice),
    [
        ?_assertMatch(ok, PostResponse),
        ?_assertMatch({ok, [{_, []}]}, GetResponse)
    ].
