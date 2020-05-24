-module(socialnetwork_tests).
-author("japs").

-include_lib("eunit/include/eunit.hrl").

start_tl(User) ->
    timeline:start(User).

stop_tl(Tl) ->
    ?assertMatch(ok, gen_server:stop(Tl)).

alice_can_create_her_timeline_test() ->
    {ok, Pid} = start_tl(alice),
    ?assert(is_pid(Pid)),
    stop_tl(Pid).

alice_can_view_her_timeline_test() ->
    {ok, Pid} = start_tl(alice),
    {ok, Messages} = timeline:get_messages(alice),
    ?assertMatch([], Messages),
    stop_tl(Pid).

alice_can_post_messages_to_her_personal_timeline_test() ->
    {ok, Pid} = start_tl(alice),
    PostResponse = timeline:post(alice, "first"),
    {ok, Messages} = timeline:get_messages(alice),
    ?assertMatch(ok, PostResponse),
    ?assertMatch(["first"], Messages),
    stop_tl(Pid).

messages_are_shown_in_reverse_time_order_test() ->
    {ok, Pid} = start_tl(alice),
    timeline:post(alice, "first"),
    timeline:post(alice, "second"),
    {ok, Messages} = timeline:get_messages(alice),
    ?assertMatch(["second","first"], Messages),
    stop_tl(Pid).

%%alice_can_view_bob_timeline_test() ->
%%    ?assert(false).
%%

%%alice_could_not_post_messages_to_bob_timeline_test() ->
%%    ?assert(false).