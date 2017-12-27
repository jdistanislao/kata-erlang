-module(minesweeper_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MINE_VALUE, -1000).
-define(MINE_STR, "*").

field_from_file_test() ->
  F = minesweeper:new("./test/input.txt"),
  ?assertEqual(2, length(F)),
  ?assertEqual("*", get({0,0}, lists:nth(1,F))),
  ?assertEqual("*", get({1,2}, lists:nth(1,F))),
  ?assertEqual("*", get({0,0}, lists:nth(2,F))),
  ?assertEqual("*", get({1,0}, lists:nth(2,F))),
  ?assertEqual("*", get({1,2}, lists:nth(2,F))).

field_with_no_mines_test() ->
  Mines = [],
  F = minesweeper:new(3, 3, Mines),
  ?assertEqual(0, get({0,0}, F)),
  ?assertEqual(0, get({0,1}, F)),
  ?assertEqual(0, get({0,2}, F)),
  ?assertEqual(0, get({1,0}, F)),
  ?assertEqual(0, get({1,1}, F)),
  ?assertEqual(0, get({1,2}, F)),
  ?assertEqual(0, get({2,0}, F)),
  ?assertEqual(0, get({2,1}, F)),
  ?assertEqual(0, get({2,2}, F)).

field_with_one_mine_in_the_center_test() ->
  Mines = [{1,1}],
  New = minesweeper:new(3, 3, Mines),
  F = minesweeper:find_mines(New),
  ?assertEqual(1, get({0,0}, F)),
  ?assertEqual(1, get({0,1}, F)),
  ?assertEqual(1, get({0,2}, F)),
  ?assertEqual(1, get({1,0}, F)),
  ?assertEqual("*", get({1,1}, F)),
  ?assertEqual(1, get({1,2}, F)),
  ?assertEqual(1, get({2,0}, F)),
  ?assertEqual(1, get({2,1}, F)),
  ?assertEqual(1, get({2,2}, F)).


field_with_one_mine_in_a_corner_test() ->
  Mines = [{0,2}],
  New = minesweeper:new(3, 3, Mines),
  F = minesweeper:find_mines(New),
  ?assertEqual(0, get({0,0}, F)),
  ?assertEqual(1, get({0,1}, F)),
  ?assertEqual("*", get({0,2}, F)),
  ?assertEqual(0, get({1,0}, F)),
  ?assertEqual(1, get({1,1}, F)),
  ?assertEqual(1, get({1,2}, F)),
  ?assertEqual(0, get({2,0}, F)),
  ?assertEqual(0, get({2,1}, F)),
  ?assertEqual(0, get({2,2}, F)).


field_with_mines_except_the_center_test() ->
  Mines = [{0,0},{0,1},{0,2},{1,0},{1,2},{2,0},{2,1},{2,2}],
  New = minesweeper:new(3, 3, Mines),
  F = minesweeper:find_mines(New),
  ?assertEqual("*", get({0,0}, F)),
  ?assertEqual("*", get({0,1}, F)),
  ?assertEqual("*", get({0,2}, F)),
  ?assertEqual("*", get({1,0}, F)),
  ?assertEqual(8, get({1,1}, F)),
  ?assertEqual("*", get({1,2}, F)),
  ?assertEqual("*", get({2,0}, F)),
  ?assertEqual("*", get({2,1}, F)),
  ?assertEqual("*", get({2,2}, F)).

%%
%% Utils
%%

  
get({X,Y}, F) ->
  get(Y, array:get(X,F));
get(I, F) ->
  V = array:get(I, F),
  case V == ?MINE_VALUE of
    true -> ?MINE_STR;
    _    -> V
  end.
