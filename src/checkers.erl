
-module(checkers).
-import(board,[showBoard/1,startingBoard/0]).
-import(moves,[makeMove/3]).
%% API
-export([main/0, player/0]).

getOppontentColor(C) ->
  case C of
    white -> black;
    black -> white
  end.

inputToColNo(X) ->
  case string:to_upper(X) of
    "A" -> 1;
    "B" -> 2;
    "C" -> 3;
    "D" -> 4;
    "E" -> 5;
    "F" -> 6;
    "G" -> 7;
    "H" -> 8;
    _ -> -1
  end.

% row number must be inverted
inputToRowNo(Y) when Y<8 -> 9-Y;
inputToRowNo(Y) -> -1.

areValidSquares([X1,Y1,X2,Y2]) when (X1 == -1) or (Y1 == -1) or (X2 == -1) or (Y2 == -1) -> {false, [X1,Y1,X2,Y2]};
areValidSquares([X1,Y1,X2,Y2]) -> {true, [X1,Y1,X2,Y2]}.

readUserChoice() ->
  case io:fread("Your choice: ","~d") of
    {ok,[Choice]} -> Choice;
    {error,_} -> readUserChoice()
  end.

readPlayerMove() ->
  case io:fread("Entry your move [format [A-H][1-8][A-H][1-8]] : ","~c~d~c~d") of
    {ok,Move} -> Move;
    {error,_} ->
      io:format("Move format must be [A-H][1-8][A-H][1-8]]"),
      readPlayerMove()
  end.

getPlayerMoveIfSquaresAreValid() ->
  case areValidSquares(getPlayerMove()) of
    {false, _ } ->
      io:format("No square with coordinates matching entered data ~n"),
      areValidSquares(getPlayerMoveIfSquaresAreValid());
    {true, Move} -> Move
  end.

getPlayerMove() ->
  [IX1, IY1, IX2, IY2] = readPlayerMove(),
  [inputToColNo(IX1), inputToRowNo(IY1), inputToColNo(IX2), inputToRowNo(IY2)].

player() ->
  receive
    {PIDMain,PIDOpponent,Color,BoardState} ->
      showBoard(BoardState),
      io:format("~s to move ~n",[Color]),
      [X1, Y1, X2, Y2] = getPlayerMoveIfSquaresAreValid(),
      NewBoardState = makeMove({X1,Y1},{X2,Y2},BoardState),
      PIDOpponent!{PIDMain,self(),getOppontentColor(Color),NewBoardState}
      %PIDMain!{end_of_game}
  end.

main() ->
  io:format("------------------Checkers - Erlang-----------------~n"),
  io:format("1. Player vs Player~n"),
  io:format("2. Player vs Computer~n"),
  io:format("3. Quit~n"),
  C = readUserChoice(),
  case C of
    1 ->
      PIDPlayer1 = spawn(?MODULE,player,[]),
      PIDPlayer2 = spawn(?MODULE,player,[]),
      PIDPlayer1!{self(),PIDPlayer2,white,startingBoard()},
      receive
        {end_of_game} -> io:format("Thanks for playing.")
      end;
    2 ->
      io:format("In version 2.0");
    3 ->
      io:format("Thanks for playing.");
    _ ->
    io:format("Please choose option 1,2 or 3.~n~n~n"),
    main()
end.