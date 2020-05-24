-module(timeline).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/1, get_messages/1, post/2]).

-define(SERVER, ?MODULE).

-record(tl_state, {user, token, messages}).

%%%===================================================================
%%% API
%%%===================================================================

start(User) ->
    start_link(User).

get_messages(User) ->
    gen_server:call(User, {get_messages}).

post(User, Message) ->
    gen_server:cast(User, {post, Message}).

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

handle_call({get_messages}, _From, State = #tl_state{messages = M}) ->
    {reply, {ok, M}, State}.

handle_cast({post, Message}, State = #tl_state{messages = CurrentMessages}) ->
    NewState = State#tl_state{messages = [Message|CurrentMessages]},
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
