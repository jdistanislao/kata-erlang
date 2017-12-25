-module(minesweeper).

-export([new/1, get/2, set_mine/2]).

-define(MINE_VALUE, -1000).
-define(MINE_STR, "*").

new(S) ->
  array:new([{size,S}, {fixed,true}, {default,0}]).

set_mine(I, F) ->
  array:set(I, ?MINE_VALUE, F).

get(I, _F) when I < 0 ->
  {error, out_of_bound};
get(I, F) ->
  V = array:get(I, F),
  case V == ?MINE_VALUE of
    true -> {ok, ?MINE_STR};
    _    -> {ok, V}
  end.

%%
%% Utils
%%
