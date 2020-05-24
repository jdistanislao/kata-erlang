-module(socialnetwork_tests).
-author("japs").

-include_lib("eunit/include/eunit.hrl").

alice_can_create_her_timeline_test() ->
    {ok, Pid} = timeline:start(alice),
    ?assert(is_pid(Pid)).

%%alice_can_view_her_timeline_test() ->
%%    ?assert(false).
%%
%%alice_can_view_bob_timeline_test() ->
%%    ?assert(false).
%%
%%alice_can_post_messages_to_her_personal_timeline_test() ->
%%    ?assert(false).
%%
%%alice_could_not_post_messages_to_bob_timeline_test() ->
%%    ?assert(false).