-module(timeline).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/1, get_messages/1, get_private_messages/2, post/3, send_private_message/3, subscribe/3]).

-define(SERVER, ?MODULE).

-record(msg, {content, timestamp, from}).
-record(tl_state, {user, token, messages, private_messages, subscritpions}).

%%%===================================================================
%%% API
%%%===================================================================

start(User) ->
    start_link(User).

get_messages(User) ->
    {ok, Messages} = gen_server:call(User, {get_messages}),
    SortedMessages = lists:map(fun(#msg{content = C}) -> C end, sort_messages(Messages)),
    {ok, SortedMessages}.

get_private_messages(User, Token) ->
    Response = gen_server:call(User, {get_private_messages, Token}),
    case Response of
        {ok, Messages} -> MapFn = fun(#msg{content = C, from = F}) -> {F,C} end,
                            SortedMessages = lists:map(MapFn, sort_messages(Messages)),
                            {ok, SortedMessages};
                    _  -> Response
    end.

post(User, Token, Message) ->
    gen_server:cast(User, {post, Token, Message}).

subscribe(User, Token, Followee) ->
    gen_server:cast(User, {subscribe, Token, Followee}).

send_private_message(User, To, Message) ->
    gen_server:cast(To, {private_message, User, Message}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(User) ->
    Token = make_ref(),
    {ok, Pid} = gen_server:start_link({local, User}, ?MODULE, [User, Token], []),
    {ok, {Pid, Token}}.

init([User, Token]) ->
    State = #tl_state{user=User, token=Token, messages=[], private_messages=[], subscritpions=[]},
    {ok, State}.
%%%===================================================================
%%% get_messages
%%%===================================================================
handle_call({get_messages}, _From, State = #tl_state{messages = UsrM, subscritpions = S}) ->
    RetrieveSubsMessages = fun(Followee) ->
        {ok, SubM} = gen_server:call(Followee, {get_messages}),
        SubM
        end,
    SubsMessages = lists:map(RetrieveSubsMessages, S),
    AllMessages = [UsrM | SubsMessages],
    {reply, {ok, lists:concat(AllMessages)}, State};
%%%===================================================================
%%% get_private_messages
%%%===================================================================
handle_call({get_private_messages, Token}, _From, State = #tl_state{token = T, private_messages = M}) when Token =:= T ->
    {reply, {ok, M}, State};
handle_call({get_private_messages, _}, _From, State)  ->
    {reply, {error, not_allowed}, State}.
%%%===================================================================
%%% post
%%%===================================================================
handle_cast({post, Token, Message}, State = #tl_state{token = T, messages = CurrentMessages}) when Token =:= T ->
    NewMessage = create_new_message(Message),
    NewState = State#tl_state{messages = [NewMessage|CurrentMessages]},
    {noreply, NewState};
handle_cast({post, _, _}, State) ->
    {noreply, State};
%%%===================================================================
%%% subscribe
%%%===================================================================
handle_cast({subscribe, Token, Followee}, State = #tl_state{token = T, subscritpions = S}) when Token =:= T ->
    NewSubscritpions = add_new_subscription(Followee, S),
    NewState = State#tl_state{subscritpions = NewSubscritpions},
    {noreply, NewState};
handle_cast({subscribe, _, _}, State) ->
    {noreply, State};
%%%===================================================================
%%% private_message
%%%===================================================================
handle_cast({private_message, From, Message}, State = #tl_state{private_messages = P}) ->
    Msg = create_new_message(Message, From),
    NewState = State#tl_state{private_messages = [Msg|P]},
    {noreply, NewState}.

handle_info(_Info, State = #tl_state{}) ->
    {noreply, State}.

terminate(_Reason, _State = #tl_state{}) ->
    ok.

code_change(_OldVsn, State = #tl_state{}, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
create_new_message(Content) ->
    #msg{content = Content, timestamp = erlang:monotonic_time()}.
create_new_message(Content, From) ->
    #msg{content = Content, timestamp = erlang:monotonic_time(), from = From}.

sort_messages(Messages) ->
    lists:sort(fun(#msg{timestamp = T1}, #msg{timestamp = T2}) -> T1 > T2 end, Messages).

add_new_subscription(Followee, CurrentSubscriptions) ->
    UniqueFollowee = lists:usort(Followee),
    NewSubs = [F || F <- UniqueFollowee, lists:member(F, CurrentSubscriptions) == false],
    lists:concat([NewSubs, CurrentSubscriptions]).