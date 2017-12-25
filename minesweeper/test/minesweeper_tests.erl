-module(minesweeper_tests).

-include_lib("eunit/include/eunit.hrl").

invalid_index_test() ->
  F = minesweeper:new(1, []),
  ?assertEqual({error, out_of_bound}, minesweeper:get(-1, F)).

single_row_no_mines_test() ->
  F = minesweeper:new(10, []),
  ?assertEqual({ok, 0}, minesweeper:get(0, F)),
  ?assertEqual({ok, 0}, minesweeper:get(9, F)).
  
single_row_with_one_mine_at_beginning_test() ->
  New = minesweeper:new(10, [0]),
  F = minesweeper:find_mines(New),
  ?assertEqual({ok, "*"}, minesweeper:get(0, F)),
  ?assertEqual({ok, 1}, minesweeper:get(1, F)),
  ?assertEqual({ok, 0}, minesweeper:get(2, F)).

single_row_with_multiple_mines_test() ->
  New = minesweeper:new(6, [0,2,3]),
  F = minesweeper:find_mines(New),
  ?assertEqual({ok, "*"}, minesweeper:get(0, F)),
  ?assertEqual({ok, "*"}, minesweeper:get(2, F)),
  ?assertEqual({ok, "*"}, minesweeper:get(3, F)),
  ?assertEqual({ok, 2}, minesweeper:get(1, F)),
  ?assertEqual({ok, 1}, minesweeper:get(4, F)),
  ?assertEqual({ok, 0}, minesweeper:get(5, F)).
