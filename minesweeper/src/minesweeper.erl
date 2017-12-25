-module(minesweeper).

-export([new/2, get/2, find_mines/1, set_mine/2]).

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
  F.

%%
%% Utils
%%

set_mine(I, F) ->
  array:set(I, ?MINE_VALUE, F).
