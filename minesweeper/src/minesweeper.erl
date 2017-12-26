-module(minesweeper).

-export([new/3, find_mines/1]).
-export([set_mine/2]).

-define(MINE_VALUE, -1000).
-define(MINE_STR, "*").

%%
%% 0,0 (x,y) at top-left corner
%% 
new(XX, YY, M) ->
  ArrayGen = fun(S) -> array:new([{size,S}, {fixed,true}, {default,0}]) end,
  F = array:new([{size,XX}, {fixed,true}, {default, ArrayGen(YY)}]),
  lists:foldl(fun minesweeper:set_mine/2, F, M).

find_mines(F) ->
  Find = fun(X, Y, V) ->
    case V =:= ?MINE_VALUE of 
      true -> V;
      _    -> cell_points(X, Y, V, F)
    end
  end,
  MapRowFn = fun(X, A) -> array:map(fun(Y, V) -> Find(X, Y, V) end, A) end,
  array:map(MapRowFn, F).


%%
%% Utils
%%

%% print(F) ->
%%   R = array:to_list(F),
%%   X = fun(A) ->
%% 	  C = array:to_list(A),
%% 	  lists:foreach(fun(V) -> io:format("~p", [V]) end, C),
%% 	  io:format("~n")
%%       end,
%%   io:format("~n"),
%%   lists:foreach(X, R).

cell_points(X, Y, V, F) ->
  Coords = coords_around(X, Y),
  SumFn = fun({Xc, Yc}, Acc) -> sum(Acc, get(Yc, get(Xc, F, array:new()), 0)) end,
  lists:foldl(SumFn, V, Coords).

coords_around(X, Y) ->
  [{X, Y-1},{X+1, Y-1},{X+1, Y},{X+1, Y+1},{X, Y+1},{X-1, Y+1},{X-1, Y},{X-1, Y-1}].

get(I, _, What) when I < 0 ->
  What;
get(I, A, What) ->
  case I < array:size(A) of
    true -> array:get(I, A);
    _    -> What
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
