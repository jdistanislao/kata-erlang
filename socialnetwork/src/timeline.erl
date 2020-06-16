-module(timeline).
-behaviour(gen_server).

-include("tl_records.hrl").

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start/1, get_messages/1, get_private_messages/2, post/3, send_private_message/3, subscribe/3, get_mentions/2]).


%%%===================================================================
%%% API
%%%===================================================================

start(User) ->
    start_link(User).

get_messages(User) ->
    {ok, Messages} = gen_server:call(User, {get_messages}),
    SortedMessages = sort_messages(Messages),
    {ok, SortedMessages}.

get_private_messages(User, Token) ->
    Response = gen_server:call(User, {get_private_messages, Token}),
    case Response of
        {ok, Messages} -> SortedMessages = sort_messages(Messages),
                            {ok, SortedMessages};
                    _  -> Response
    end.

post(User, Token, Message) ->
    gen_server:cast(User, {post, Token, Message}).

subscribe(User, Token, Followee) ->
    gen_server:cast(User, {subscribe, Token, Followee}).

send_private_message(User, To, Message) ->
    gen_server:cast(To, {private_message, User, Message}).

get_mentions(User, Token) ->
    Response = gen_server:call(User, {get_mentions, Token}),
    case Response of
        {ok, Messages} -> SortedMessages = sort_messages(Messages),
                            {ok, SortedMessages};
        _              -> Response
    end.

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(User) ->
    Token = make_ref(),
    {ok, Pid} = gen_server:start_link({local, User}, ?MODULE, [User, Token], []),
    {ok, {Pid, Token}}.

init([User, Token]) ->
    State = #tl_state{user=User, token=Token},
    {ok, State}.

%%%===================================================================
%%% get_messages
%%%===================================================================
handle_call({get_messages}, _From, State = #tl_state{messages = UsrM, subscriptions = S}) ->
    RetrieveSubsMessages = fun(Followee) ->
        {ok, SubM} = timeline:get_messages(Followee),
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
    {reply, {error, not_allowed}, State};

%%%===================================================================
%%% get_mentions
%%%===================================================================
handle_call({get_mentions, Token}, _From, State = #tl_state{token = T, mentions = M}) when Token =:= T ->
    {reply, {ok, M}, State};
handle_call({get_mentions, _}, _From, State)  ->
    {reply, {error, not_allowed}, State}.

%%%===================================================================
%%% post
%%%===================================================================
handle_cast({post, Token, Message}, State = #tl_state{user = U, token = T, messages = CurrentMessages}) when Token =:= T ->
    NewMessage = create_new_message(Message, U),
    NewState = State#tl_state{messages = [NewMessage|CurrentMessages]},
    notify_mention(U, NewMessage),
    {noreply, NewState};
handle_cast({post, _, _}, State) ->
    {noreply, State};

%%%===================================================================
%%% subscribe
%%%===================================================================
handle_cast({subscribe, Token, Followee}, State = #tl_state{token = T, subscriptions = S}) when Token =:= T ->
    NewSubscritpions = add_new_subscription(Followee, S),
    NewState = State#tl_state{subscriptions = NewSubscritpions},
    {noreply, NewState};
handle_cast({subscribe, _, _}, State) ->
    {noreply, State};

%%%===================================================================
%%% private_message
%%%===================================================================
handle_cast({private_message, From, Message}, State = #tl_state{private_messages = P}) ->
    Msg = create_new_message(Message, From),
    NewState = State#tl_state{private_messages = [Msg|P]},
    {noreply, NewState};

%%%===================================================================
%%% new_mention
%%%===================================================================
handle_cast({new_mention, Message}, State = #tl_state{mentions = M}) ->
    NewState = State#tl_state{mentions = [Message|M]},
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
create_new_message(Content, From) ->
    {NewContent, Mentions} = parse(Content),
    #msg{content = NewContent, mentions = Mentions, from = From}.

sort_messages(Messages) ->
    lists:sort(fun(#msg{timestamp = T1}, #msg{timestamp = T2}) -> T1 > T2 end, Messages).

add_new_subscription(Followee, CurrentSubscriptions) ->
    UniqueFollowee = lists:usort(Followee),
    NewSubs = [F || F <- UniqueFollowee, lists:member(F, CurrentSubscriptions) == false],
    lists:concat([NewSubs, CurrentSubscriptions]).

parse(Content) ->
    SplittedContent = string:split(Content, " ", all),
    Mentions = find_mentions(SplittedContent, []),
    NewContent = map_web_resources(SplittedContent, []),
    {NewContent, Mentions}.

find_mentions([[UH|UT]|T], L) when [UH] =:= "@" andalso length(UT) >= 1 ->
    UserRefName = list_to_atom(string:slice([UH|UT], 1)),
    case whereis(UserRefName) of
        undefined   -> find_mentions(T, L);
        _           -> find_mentions(T, [UserRefName | L])
    end;
find_mentions([_|T], L) ->
    find_mentions(T, L);
find_mentions([], L) ->
    L.

notify_mention(From, Message = #msg{mentions = Destinations}) ->
    Msg = Message#msg{from = From, mentions = []},
    lists:foreach(fun(To) -> ok = gen_server:cast(To, {new_mention, Msg}) end, Destinations).

map_web_resources({ok, [H|T]}, L) ->
    Link = create_html_link(H),
    map_web_resources(T, [Link | L]);
map_web_resources({nomatch, [H|T]}, L) ->
    map_web_resources(T, [H | L]);
map_web_resources([H|_] = Res, L) ->
    Result = find_web_resources2(H),
    map_web_resources({Result, Res}, L);
map_web_resources([], L) ->
    lists:concat(lists:join(" ", lists:reverse(L))).

find_web_resources2(Word) ->
    case is_web_resource(Word) of
        true -> case is_web_resource_reachable(Word) of
                    true -> ok;
                    _    -> nomatch
                end;
        _    -> nomatch
    end.

is_web_resource(Resource) when length(Resource) > 8 ->
    case string:find(Resource, "http://") of
        nomatch -> case string:find(Resource, "https://") of
                       nomatch -> false;
                       _       -> true
                   end;
        _       -> true
    end;
is_web_resource(_) ->
    false.

is_web_resource_reachable(Resource) ->
    % fake an http(s) request assuming a web resources is valid if ends with .org.
    L = length(Resource),
    Domain = string:slice(Resource, L-4),
    ".org" =:= Domain.

create_html_link(Resource) ->
    LinkData = ["<a href=\"", Resource, "\">", Resource, "</a>"],
    unicode:characters_to_list(LinkData).