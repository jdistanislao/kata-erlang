-module(minesweeper).

-export([new/2, new/3, get/2, find_mines/1]).
-export([set_mine/2]).

-define(MINE_VALUE, -1000).
-define(MINE_STR, "*").

new(S, M) ->
  F = array:new([{size,S}, {fixed,true}, {default,0}]),
  lists:foldl(fun minesweeper:set_mine/2, F, M).

%%
%% 0,0 (x,y) at top-left corner
%% 
new(XX, YY, M) ->
  ArrayGen = fun(S) -> array:new([{size,S}, {fixed,true}, {default,0}]) end,
  F = array:new([{size,XX}, {fixed,true}, {default, ArrayGen(YY)}]),
  lists:foldl(fun minesweeper:set_mine/2, F, M).
  
get({X,Y}, F) ->
  get(Y, array:get(X,F));
get(I, F) ->
  V = array:get(I, F),
  case V == ?MINE_VALUE of
    true -> {ok, ?MINE_STR};
    _    -> {ok, V}
  end.

find_mines(F) ->
  Find = fun(X, Y, V) ->
    case V =:= ?MINE_VALUE of 
      true -> V;
      _    -> cell_points(X, Y, V, F)
    end
  end,
  array:map(fun(Y, A) -> array:map(fun(X, V) -> Find(X, Y, V) end, A) end, F).


%%
%% Utils
%%

cell_points(X, Y, V, F) ->
  Coords = coords_around(X, Y),
  SumFn = fun({Xc, Yc}, Acc) -> sum(Acc, get_value(Yc, array_get(Xc, F))) end,
  lists:foldl(SumFn, V, Coords).

coords_around(X, Y) ->
  C = [ 
    {X, Y-1},
    {X+1, Y-1},
    {X+1, Y},
    {X+1, Y+1},
    {X, Y+1},
    {X-1, Y+1},
    {X-1, Y},
    {X-1, Y-1}
  ],
  io:format("~p~n", [C]),
  C.
  
array_get(I, _) when I < 0 ->
  array:new();
array_get(I, A) ->
  case I < array:size(A) of
    true -> array:get(I, A);
    _    -> array:new()
  end.


get_value(I, _) when I < 0 ->
  0;
get_value(I, F) ->
  case I < array:size(F) of
    true -> array:get(I, F);
    _    -> 0
  end.

set_mine({X, Y}, F) ->
  FX = array:get(X, F),
  array:set(X, set_mine(Y, FX), F);
set_mine(I, F) ->
  array:set(I, ?MINE_VALUE, F).

sum(X, V) when V =:= ?MINE_VALUE ->
  X+1;
sum(X, _) ->
  X.
