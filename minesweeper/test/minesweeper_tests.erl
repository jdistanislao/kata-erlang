-module(minesweeper_tests).

-include_lib("eunit/include/eunit.hrl").

single_element_no_mines_test() ->
  F = minesweeper:new(),
  ?assertEqual({ok, 0}, minesweeper:get(0, F)).
