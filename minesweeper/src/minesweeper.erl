-module(minesweeper).

-export([new/1, new/3, find_mines/1]).
-export([set_mine/2]).

-define(MINE_VALUE, -1000).
-define(MINE_STR, "*").

%%
%% 0,0 (x,y) at top-left corner
%% 

new(File) ->
  {ok, IO} = file:open(File, [read]),
  F = from_file(IO),
  file:close(IO),
  F.

new(X, Y, M) ->
  ArrayGen = fun(S) -> array:new([{size,S}, {fixed,true}, {default,0}]) end,
  F = array:new([{size,X}, {fixed,true}, {default, ArrayGen(Y)}]),
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

from_file(IO) ->
  {X, Y} = to_dimensions(read_lines(1, IO, [])),
  M = mines_coords(read_lines(Y, IO, []), 0, []),
  new(X, Y, M).

to_dimensions(L) ->
  [{X, _}, {Y, _}] = lists:map(fun string:to_integer/1, string:split(L, " ")),
  {X, Y}.

mines_coords([], _, Acc) ->
  Acc;
mines_coords([H|T], Y, Acc) ->
  Mines = map_mines(H, Y, Acc),
  io:format("~n"),
  io:format(H),
  mines_coords(T, Y+1, Mines).


map_mines(L, Y, Acc) ->
  case string:str(L, ?MINE_STR) of
    0   -> Acc;
    Pos -> map_mines(string:slice(L, Pos), Y, [{Pos-1, Y}|Acc])
  end.

read_lines(0, _, Acc) ->
  lists:reverse(Acc);
read_lines(N, IO, Acc) ->
   Data = io:get_line(IO, ''),
   Line = string:slice(Data, 0, string:length(Data)-1),
   read_lines(N-1, IO, [Line|Acc]).

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
  SumFn = fun({Xc, Yc}, Acc) -> sum(Acc, get(Yc, get(Xc, F, array:new()), 0)) end,
  lists:foldl(SumFn, V, around(X, Y)).

around(X, Y) ->
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
