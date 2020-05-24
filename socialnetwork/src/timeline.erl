-module(timeline).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/1, get_messages/1, post/3, subscribe/3]).

-define(SERVER, ?MODULE).

-record(msg, {content, timestamp}).
-record(tl_state, {user, token, messages, subscritpions}).

%%%===================================================================
%%% API
%%%===================================================================

start(User) ->
    start_link(User).

get_messages(User) ->
    {ok, Messages} = gen_server:call(User, {get_messages}),
    {ok, extract_ordered_messages_content(Messages)}.

post(User, Token, Message) ->
    gen_server:cast(User, {post, Token, Message}).

subscribe(User, Token, Followee) ->
    gen_server:cast(User, {subscribe, Token, Followee}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(User) ->
    Token = make_ref(),
    {ok, Pid} = gen_server:start_link({local, User}, ?MODULE, [User, Token], []),
    {ok, {Pid, Token}}.

init([User, Token]) ->
    State =  #tl_state{user = User, token= Token, messages = [], subscritpions = []},
    {ok, State}.

handle_call({get_messages}, _From, State = #tl_state{messages = UsrM, subscritpions = S}) ->
    RetrieveSubsMessages = fun(Followee) ->
        {ok, SubM} = gen_server:call(Followee, {get_messages}),
        SubM
        end,
    SubsMessages = lists:map(RetrieveSubsMessages, S),
    AllMessages = [UsrM | SubsMessages],
    {reply, {ok, lists:concat(AllMessages)}, State}.


handle_cast({post, Token, Message}, State = #tl_state{token = T, messages = CurrentMessages}) when Token =:= T ->
    NewMessage = create_new_message(Message),
    NewState =State#tl_state{messages = [NewMessage|CurrentMessages]},
    {noreply, NewState};
handle_cast({post, _, _}, State) ->
    {noreply, State};

handle_cast({subscribe, Token, Followee}, State = #tl_state{token = T, subscritpions = S}) when Token =:= T ->
    NewSubscritpions = add_new_subscription(Followee, S),
    NewState = State#tl_state{subscritpions = NewSubscritpions},
    {noreply, NewState};
handle_cast({subscribe, _, _}, State) ->
    {noreply, State}.

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

extract_ordered_messages_content(Messages) ->
    SortedMessages = lists:sort(fun(#msg{timestamp = T1}, #msg{timestamp = T2}) -> T1 > T2 end, Messages),
    lists:map(fun(#msg{content = C}) -> C end, SortedMessages).

add_new_subscription(Followee, CurrentSubscriptions) ->
    UniqueFollowee = lists:usort(Followee),
    NewSubs = [F || F <- UniqueFollowee, lists:member(F, CurrentSubscriptions) == false],
    lists:concat([NewSubs, CurrentSubscriptions]).