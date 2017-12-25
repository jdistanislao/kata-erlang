-module(minesweeper).

-export([new/2, get/2, find_mines/1]).
-export([set_mine/2]).

-define(MINE_VALUE, -1000).
-define(MINE_STR, "*").

new(S, M) ->
  F = array:new([{size,S}, {fixed,true}, {default,0}]),
  lists:foldl(fun minesweeper:set_mine/2, F, M).

get(I, _F) when I < 0 ->
  {error, out_of_bound};
get(I, F) ->
  V = array:get(I, F),
  case V == ?MINE_VALUE of
    true -> {ok, ?MINE_STR};
    _    -> {ok, V}
  end.

find_mines(F) ->
  Find = fun(I, V) ->
    case V =:= ?MINE_VALUE of 
      true -> V;
      _    -> sum(V, get_value(I-1, F)) + sum(V, get_value(I+1, F))
    end
  end,
  array:map(Find, F).


%%
%% Utils
%%

get_value(I, _) when I < 0 ->
  0;
get_value(I, F) ->
  case I < array:size(F) of
    true -> array:get(I, F);
    _    -> 0
  end.

set_mine(I, F) ->
  array:set(I, ?MINE_VALUE, F).

sum(X, V) when V =:= ?MINE_VALUE ->
  X+1;
sum(X, _) ->
  X.
%%  io:format("index: ~p, value: ~p~n", [I, V]),
