-module(moves).
-import(board,[getSquareValue/2,setSquareValue/3]).
-import(checkers,[getOpponentColor/1]).
%% API
-export([makeMove/4, getNormalMoveSquares/3,getAvailableSquares/3,moveOrCapture/2]).

makeMove([X1, Y1, X2, Y2],BoardState,Color,PieceCount) ->
  BoardStateTmp = setSquareValue(empty, {X1,Y1}, BoardState),
  BoardStateTmp2 = moveOrCapture([X1, Y1, X2, Y2],BoardStateTmp),
  case BoardStateTmp2 of
	BoardStateTmp -> {PieceCount,setSquareValue(getSquareValue({X1,Y1}, BoardState), {X2,Y2}, BoardStateTmp2)};
	_			  -> {decrementPiece(getOpponentColor(Color),PieceCount),setSquareValue(getSquareValue({X1,Y1}, BoardState), {X2,Y2}, BoardStateTmp2)}
  end.
 
decrementPiece(Color,{WhiteCount,BlackCount}) ->
	case {WhiteCount,BlackCount} of
		{{Color,Count},_} -> {{Color,Count-1},BlackCount};
		{_,{Color,Count}} -> {WhiteCount,{Color,Count-1}}
	end.
  
moveOrCapture([X1,Y1,X2,Y2],BoardState) ->
	case abs(X2 - X1) of
		2 ->
			setSquareValue(empty,{round((X1+X2)/2),round((Y1+Y2)/2)},BoardState);
		_ ->
			BoardState
	end.

getAvailableSquares(Piece,{X,Y}, BoardState) ->
    CaptureMoves = getCaptureMoveSquares(Piece,{X,Y}, BoardState),
    getNormalMoveSquares(Piece,{X,Y}, BoardState) ++ CaptureMoves.
	
getNormalMoveSquares(Piece,{X,Y}, BoardState) ->
  case Piece of
    {_,pawn} ->
      case Piece of
        {white,_} ->
			if X+1 < 9, Y-1 > 0 -> R = [{X+1,Y-1}];
			   true 			-> R = []
			end,
			if X-1 > 0, Y-1 > 0 -> R ++ [{X-1,Y-1}];
			   true				-> R
			end;
        {black,_} ->
			if X+1 < 9, Y+1 < 9 -> R = [{X+1,Y+1}];
			true 				-> R = []
			end,
			if X-1 > 0, Y+1 < 9 -> R ++ [{X-1,Y+1}];
			   true				-> R
			end
      end;
    {_,queen} -> getQueenNormalMoveSquares({X,Y},{1, 1},[]) ++ getQueenNormalMoveSquares({X,Y},{1, -1},[]) ++ getQueenNormalMoveSquares({X,Y},{-1, 1},[]) ++
      getQueenNormalMoveSquares({X,Y},{-1, -1},[])
  end;
getNormalMoveSquares(_, _, _) -> [].

getQueenNormalMoveSquares({X,Y},{DirectionX, DirectionY},SquaresList) when (X+DirectionX >= 1) and (Y+DirectionY >= 1) and
  (X+DirectionX =< 8) and (Y+DirectionY =< 8) ->
  getQueenNormalMoveSquares({X,Y},{DirectionX, DirectionY},SquaresList ++ [{X+DirectionX,Y+DirectionY}]);
getQueenNormalMoveSquares(_,_,SquaresList) -> SquaresList.

getCaptureMoveSquares(Piece,{X,Y}, BoardState) ->
  case Piece of
    {_,pawn} -> 
      case Piece of
        {white,_} ->
			if X+2 < 9, Y-2 > 0 -> 
				case getSquareValue({X+1,Y-1},BoardState) of
					{black,_} -> R = [{X+2,Y-2}];
					_ -> R = []
				end;
				true 			-> R = []
			end,
			if X-2 > 0, Y-2 > 0 -> 
				case getSquareValue({X-1,Y-1},BoardState) of
					{black,_} -> R ++ [{X-2,Y-2}];
					_ -> R
				end;
			   true				-> R
			end;
        {black,_} ->
			if X+2 < 9, Y+2 > 0 -> 
				case getSquareValue({X+1,Y+1},BoardState) of
					{white,_} -> R = [{X+2,Y+2}];
					_ -> R = []
				end;
				true 			-> R = []
			end,
			if X-2 > 0, Y+2 > 0 -> 
				case getSquareValue({X-1,Y+1},BoardState) of
					{white,_} -> R ++ [{X-2,Y+2}];
					_ -> R
				end;
			   true				-> R
			end
      end;
    {_,queen} -> []
	% to_do
  end.