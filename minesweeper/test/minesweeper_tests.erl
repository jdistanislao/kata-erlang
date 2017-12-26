-module(minesweeper_tests).

-include_lib("eunit/include/eunit.hrl").

field_with_no_mines_test() ->
  Mines = [],
  F = minesweeper:new(3, 3, Mines),
  ?assertEqual({ok, 0}, minesweeper:get({0,0}, F)),
  ?assertEqual({ok, 0}, minesweeper:get({0,1}, F)),
  ?assertEqual({ok, 0}, minesweeper:get({0,2}, F)),
  ?assertEqual({ok, 0}, minesweeper:get({1,0}, F)),
  ?assertEqual({ok, 0}, minesweeper:get({1,1}, F)),
  ?assertEqual({ok, 0}, minesweeper:get({1,2}, F)),
  ?assertEqual({ok, 0}, minesweeper:get({2,0}, F)),
  ?assertEqual({ok, 0}, minesweeper:get({2,1}, F)),
  ?assertEqual({ok, 0}, minesweeper:get({2,2}, F)).

field_with_one_mine_in_the_center_test() ->
  Mines = [{1,1}],
  New = minesweeper:new(3, 3, Mines),
  F = minesweeper:find_mines(New),
  ?assertEqual({ok, 1}, minesweeper:get({0,0}, F)),
  ?assertEqual({ok, 1}, minesweeper:get({0,1}, F)),
  ?assertEqual({ok, 1}, minesweeper:get({0,2}, F)),
  ?assertEqual({ok, 1}, minesweeper:get({1,0}, F)),
  ?assertEqual({ok, "*"}, minesweeper:get({1,1}, F)),
  ?assertEqual({ok, 1}, minesweeper:get({1,2}, F)),
  ?assertEqual({ok, 1}, minesweeper:get({2,0}, F)),
  ?assertEqual({ok, 1}, minesweeper:get({2,1}, F)),
  ?assertEqual({ok, 1}, minesweeper:get({2,2}, F)).


field_with_one_mine_in_a_corner_test() ->
  Mines = [{0,2}],
  New = minesweeper:new(3, 3, Mines),
  F = minesweeper:find_mines(New),
  ?assertEqual({ok, 0}, minesweeper:get({0,0}, F)),
  ?assertEqual({ok, 1}, minesweeper:get({0,1}, F)),
  ?assertEqual({ok, "*"}, minesweeper:get({0,2}, F)),
  ?assertEqual({ok, 0}, minesweeper:get({1,0}, F)),
  ?assertEqual({ok, 1}, minesweeper:get({1,1}, F)),
  ?assertEqual({ok, 1}, minesweeper:get({1,2}, F)),
  ?assertEqual({ok, 0}, minesweeper:get({2,0}, F)),
  ?assertEqual({ok, 0}, minesweeper:get({2,1}, F)),
  ?assertEqual({ok, 0}, minesweeper:get({2,2}, F)).


field_with_mines_except_the_center_test() ->
  Mines = [{0,0},{0,1},{0,2},{1,0},{1,2},{2,0},{2,1},{2,2}],
  New = minesweeper:new(3, 3, Mines),
  F = minesweeper:find_mines(New),
  ?assertEqual({ok, "*"}, minesweeper:get({0,0}, F)),
  ?assertEqual({ok, "*"}, minesweeper:get({0,1}, F)),
  ?assertEqual({ok, "*"}, minesweeper:get({0,2}, F)),
  ?assertEqual({ok, "*"}, minesweeper:get({1,0}, F)),
  ?assertEqual({ok, 8}, minesweeper:get({1,1}, F)),
  ?assertEqual({ok, "*"}, minesweeper:get({1,2}, F)),
  ?assertEqual({ok, "*"}, minesweeper:get({2,0}, F)),
  ?assertEqual({ok, "*"}, minesweeper:get({2,1}, F)),
  ?assertEqual({ok, "*"}, minesweeper:get({2,2}, F)).

