
-module(moves).
-import(board,[getSquareValue/2,setSquareValue/3]).
%% API
-export([makeMove/3]).

makeMove({X1,Y1},{X2,Y2},BoardState) ->
  BoardStateTmp = setSquareValue(empty, {X1,Y1}, BoardState),
  setSquareValue(getSquareValue({X1,Y1}, BoardState), {X2,Y2}, BoardStateTmp).