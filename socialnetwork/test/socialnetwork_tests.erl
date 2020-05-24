-module(socialnetwork_tests).
-author("japs").

-include_lib("eunit/include/eunit.hrl").

start_tl(Users) ->
    StartTimelines = fun(User, Pids) ->
        {ok, Pid} = timeline:start(User),
        [Pid | Pids]
        end,
    lists:foldl(StartTimelines, [], Users).

stop_tl(Timelines) ->
    lists:foreach(fun(Tl) -> ?assertMatch(ok, gen_server:stop(Tl)) end, Timelines).

alice_can_create_her_timeline_test() ->
    Refs = start_tl([alice]),
    ?assert(is_pid(lists:last(Refs))),
    stop_tl(Refs).

alice_can_view_her_timeline_test() ->
    Refs = start_tl([alice]),
    {ok, Messages} = timeline:get_messages(alice),
    ?assertMatch([], Messages),
    stop_tl(Refs).

alice_can_post_messages_to_her_personal_timeline_test() ->
    Refs = start_tl([alice]),
    PostResponse = timeline:post(alice, "first"),
    {ok, Messages} = timeline:get_messages(alice),
    ?assertMatch(ok, PostResponse),
    ?assertMatch(["first"], Messages),
    stop_tl(Refs).

messages_are_shown_in_reverse_time_order_test() ->
    Refs = start_tl([alice]),
    timeline:post(alice, "first"),
    timeline:post(alice, "second"),
    {ok, Messages} = timeline:get_messages(alice),
    ?assertMatch(["second","first"], Messages),
    stop_tl(Refs).

%%alice_could_not_post_messages_to_bob_timeline_test() ->
%%    ?assert(false).

%%alice_can_view_bob_timeline_test() ->
%%    ?assert(false).
%%
