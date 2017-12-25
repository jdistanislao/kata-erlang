-module(minesweeper_tests).

-include_lib("eunit/include/eunit.hrl").

invalid_index_test() ->
  F = minesweeper:new(1),
  ?assertEqual({error, out_of_bound}, minesweeper:get(-1, F)).

single_row_no_mines_test() ->
  F = minesweeper:new(10),
  ?assertEqual({ok, 0}, minesweeper:get(0, F)),
  ?assertEqual({ok, 0}, minesweeper:get(9, F)).
  
single_row_with_one_mine_at_beginning_test() ->
  New = minesweeper:new(10),
  F = minesweeper:set_mine(0, New),
  ?assertEqual({ok, "*"}, minesweeper:get(0, F)),
  ?assertEqual({ok, 0}, minesweeper:get(2, F)).
