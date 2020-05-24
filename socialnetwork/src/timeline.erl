-module(timeline).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/1]).

-define(SERVER, ?MODULE).

-record(tl_state, {user, messages}).

%%%===================================================================
%%% API
%%%===================================================================

start(User) ->
    start_link(User).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(User) ->
    gen_server:start_link({local, User}, ?MODULE, [User], []).

init([User]) ->
    State =  #tl_state{user = User, messages = []},
    {ok, State}.

handle_call(_Request, _From, State = #tl_state{}) ->
    {reply, ok, State}.

handle_cast(_Request, State = #tl_state{}) ->
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
