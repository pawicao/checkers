
-module(checkers).
-import(board,[showBoard/1,startingBoard/0,getSquareValue/2,testBoard/0,testBoardEnd/0]).
-import(moves,[makeMove/4,getAvailableSquares/3,moveOrCapture/2]).
%% API
-export([main/0, player/0,getOpponentColor/1]).

getOpponentColor(C) ->
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
inputToRowNo(Y) when Y<9, Y>0 -> 9-Y;
inputToRowNo(_) -> -1.

areValidSquares([X1,Y1,X2,Y2]) when (X1 == -1) or (Y1 == -1) or (X2 == -1) or (Y2 == -1) -> {false, [X1,Y1,X2,Y2]};
areValidSquares([X1,Y1,X2,Y2]) -> {true, [X1,Y1,X2,Y2]}.
areValidPieces([X1,Y1,X2,Y2], Color, BoardState) ->
	case getSquareValue({X1,Y1},BoardState) of
		{Color,_} ->
			case getSquareValue({X2,Y2},BoardState) of
				empty -> true;
				_     -> false			
			end;
		_ 		  -> false
	end.


	
checkIfMoveIsLegal([X1,Y1,X2,Y2], BoardState, Color) ->
	Z = getAvailableSquares(getSquareValue({X1,Y1},BoardState),{X1,Y1},BoardState),
	case lists:member({X2,Y2},Z) of
		false ->
			io:format("Ilegal move! ~n"),
			getPlayerMoveIfSquaresAreValid(BoardState,Color);
		true ->
			[X1,Y1,X2,Y2]
	end.

readUserChoice() ->
  case io:fread("Your choice: ","~d") of
    {ok,[Choice]} -> Choice;
    {error,_} -> readUserChoice()
  end.

readPlayerMove() ->
  case io:fread("Enter your move [format [A-H][1-8][A-H][1-8]] : ","~c~d~c~d") of
    {ok,Move} -> 
	  Move;
    {error,_} ->
      io:format("Move format must be [A-H][1-8][A-H][1-8]]"),
      readPlayerMove()
  end.

getPlayerMoveIfSquaresAreValid(BoardState,Color) ->
  PM = getPlayerMove(),
  case areValidSquares(PM) of
    {false, _ } ->
      io:format("No square with coordinates matching entered data ~n"),
      areValidSquares(getPlayerMoveIfSquaresAreValid(BoardState,Color));
    {true, Move} -> 
	  case areValidPieces(Move, Color, BoardState) of
		false ->
			io:format("Ilegal move! ~n"),
			getPlayerMoveIfSquaresAreValid(BoardState,Color);
		true ->
			checkIfMoveIsLegal(Move, BoardState, Color)
	  end
  end.

getPlayerMove() ->
  [IX1, IY1, IX2, IY2] = readPlayerMove(),
  [inputToColNo(IX1), inputToRowNo(IY1), inputToColNo(IX2), inputToRowNo(IY2)].

checkForEndGame({WhiteCount,BlackCount},Color,PIDMain) ->
	case {WhiteCount,BlackCount} of
		{{Color,0},_} -> PIDMain!{end_of_game,getOpponentColor(Color)};
		{_,{Color,0}} -> PIDMain!{end_of_game,getOpponentColor(Color)};
		_ 			  -> true
	end.
  
player() ->
  receive
    {PIDMain,PIDOpponent,Color,BoardState,PieceCount} ->
	  checkForEndGame(PieceCount,Color,PIDMain),
      PIDNewMe = spawn(?MODULE,player,[]),
      showBoard(BoardState),
      io:format("~s to move ~n",[Color]),
      [X1, Y1, X2, Y2] = getPlayerMoveIfSquaresAreValid(BoardState,Color),
      {NewPieceCount,NewBoardState} = makeMove([X1, Y1, X2, Y2],BoardState,Color,PieceCount),
      PIDOpponent!{PIDMain,PIDNewMe,getOpponentColor(Color),NewBoardState,NewPieceCount}
  end.

main() ->
  io:format("------------------Checkers - Erlang-----------------~n"),
  io:format("1. Player vs Player~n"),
  io:format("2. Player vs Computer~n"),
  io:format("3. Quit~n"),
  C = readUserChoice(),
  case C of
    1 ->
	  %io:format("\e[H\e[J"),
      PIDPlayer1 = spawn(?MODULE,player,[]),
      PIDPlayer2 = spawn(?MODULE,player,[]),
      PIDPlayer1!{self(),PIDPlayer2,white,testBoardEnd(),{{white,1},{black,1}}},
      receive
        {end_of_game, Color} -> io:format("Thanks for playing. ~s won!", [Color]),erlang:halt()
      end;
    2 ->
      io:format("In version 2.0"),io:format("In version 2.0");
    3 ->
      io:format("Thanks for playing.");
    _ ->
    io:format("Please choose option 1,2 or 3.~n~n~n"),
    main()
end.
