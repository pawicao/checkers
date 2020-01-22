
-module(board).

%% API
-export([showBoard/1,startingBoard/0,getSquareValue/2,setSquareValue/3, testBoard/0, testBoardEnd/0]).

pieceTypeToString(P) ->
  case P of
    pawn -> "P";
    queen -> "Q"
  end.

pieceColorToString(C) ->
  case C of
    white -> "W";
    black -> "B"
  end.

showSquareInner(empty) ->
  "  ";
showSquareInner({Color,PieceType}) ->
  string:concat(pieceColorToString(Color),pieceTypeToString(PieceType)).

showSquare(8,[]) ->
  io:format("~n  -----------------------------------------~n");
showSquare(ColNo,[H|T]) ->
  io:format("~p|",[showSquareInner(H)]),
  showSquare(ColNo+1,T).

showRow(8,[]) ->
  io:format("");
showRow(RowNo,[H|T]) ->
  io:format("~p|",[8-RowNo]),
  showSquare(0,H),
  showRow(RowNo+1,T).

showBoard(Board) ->
  io:format("    A    B    C    D    E    F    G    H~n"),
  io:format("  -----------------------------------------~n"),
  showRow(0,Board).

startingBoard() -> [[empty,{black,pawn},empty,{black,pawn},empty,{black,pawn},empty,{black,pawn}],
  [{black,pawn},empty,{black,pawn},empty,{black,pawn},empty,{black,pawn},empty],
  [empty,{black,pawn},empty,{black,pawn},empty,{black,pawn},empty,{black,pawn}],
  [empty,empty,empty,empty,empty,empty,empty,empty],
  [empty,empty,empty,empty,empty,empty,empty,empty],
  [{white,pawn},empty,{white,pawn},empty,{white,pawn},empty,{white,pawn},empty],
  [empty,{white,pawn},empty,{white,pawn},empty,{white,pawn},empty,{white,pawn}],
  [{white,pawn},empty,{white,pawn},empty,{white,pawn},empty,{white,pawn},empty]].

testBoard() -> [[empty,{black,pawn},empty,{black,pawn},empty,{black,pawn},empty,{black,pawn}],
  [{black,pawn},empty,empty,empty,{black,pawn},empty,{black,pawn},empty],
  [empty,{black,pawn},empty,{black,pawn},empty,{black,pawn},empty,{black,pawn}],
  [empty,empty,empty,empty,empty,empty,empty,empty],
  [empty,{black,pawn},empty,empty,empty,empty,empty,empty],
  [{white,pawn},empty,{white,pawn},empty,{white,pawn},empty,{white,pawn},empty],
  [empty,{white,pawn},empty,{white,pawn},empty,{white,pawn},empty,{white,pawn}],
  [{white,pawn},empty,{white,pawn},empty,{white,pawn},empty,{white,pawn},empty]].

testBoardEnd() -> [[empty,empty,empty,empty,empty,empty,empty,empty],
  [{white,pawn},empty,empty,empty,empty,empty,empty,empty],
  [empty,empty,empty,empty,empty,empty,empty,empty],
  [empty,empty,empty,empty,empty,empty,{black,pawn},empty],
  [empty,empty,empty,empty,empty,empty,empty,empty],
  [empty,empty,empty,empty,empty,empty,empty,empty],
  [empty,empty,empty,empty,empty,empty,empty,empty],
  [empty,empty,empty,empty,empty,empty,empty,empty]].

getSquareValue({X,Y},Board) -> lists:nth(X,lists:nth(Y,Board)).

setSquareValue(Value,{X,Y},Board) -> lists:sublist(Board,Y-1) ++ [lists:sublist(lists:nth(Y,Board),X-1) ++ [Value] ++ lists:nthtail(X,lists:nth(Y,Board))] ++ lists:nthtail(Y,Board).