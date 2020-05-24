-module(timeline).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/1, get_messages/1, post/3]).

-define(SERVER, ?MODULE).

-record(msg, {content, timestamp}).
-record(tl_state, {user, token, messages}).

%%%===================================================================
%%% API
%%%===================================================================

start(User) ->
    start_link(User).

get_messages(User) ->
    gen_server:call(User, {get_messages}).

post(User, Token, Message) ->
    gen_server:cast(User, {post, Token, Message}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(User) ->
    Token = make_ref(),
    {ok, Pid} = gen_server:start_link({local, User}, ?MODULE, [User, Token], []),
    {ok, {Pid, Token}}.

init([User, Token]) ->
    State =  #tl_state{user = User, token= Token, messages = []},
    {ok, State}.

handle_call({get_messages}, _From, State = #tl_state{messages = CurrentMessages}) ->
    Messages = extract_messages_content(CurrentMessages),
    {reply, {ok, Messages}, State}.

handle_cast({post, Token, Message}, State = #tl_state{token = T, messages = CurrentMessages}) ->
    NewState = case Token =:= T of
                   true -> NewMessage = create_new_message(Message),
                            State#tl_state{messages = [NewMessage|CurrentMessages]};
                   _    -> State
               end,
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

extract_messages_content(Messages) ->
    SortedMessages = lists:sort(fun(#msg{timestamp = T1}, #msg{timestamp = T2}) -> T1 > T2 end, Messages),
    lists:map(fun(#msg{content = C}) -> C end, SortedMessages).