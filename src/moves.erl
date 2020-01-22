-module(moves).
-import(board,[getSquareValue/2,setSquareValue/3,showBoard/1]).
-import(checkers,[getOpponentColor/1]).
%% API
-export([makeMove/4, getNormalMoveSquares/3,getAvailableSquares/3,moveOrCapture/3,getMultipleCapturesMoveSquares/3]).

makeMove([X1, Y1, X2, Y2],BoardState,Color,PieceCount) ->
		{PieceCountTmp, BoardStateTmp} = makeMoveInner([X1, Y1, X2, Y2],BoardState,Color,PieceCount),
		BoardStateTmp2 = promoteToQueenIfPossible(getSquareValue({X1,Y1}, BoardState),{X2,Y2}, BoardStateTmp),
		{PieceCountTmp, BoardStateTmp2}.

makeMoveInner([X1, Y1, X2, Y2],BoardState,Color,PieceCount) ->
	BoardStateTmp = setSquareValue(empty, {X1,Y1}, BoardState),
	BoardStateTmp2 = moveOrCapture(getSquareValue({X1,Y1}, BoardState), [X1, Y1, X2, Y2],BoardStateTmp),
	case BoardStateTmp2 of
		BoardStateTmp -> {PieceCount,setSquareValue(getSquareValue({X1,Y1}, BoardState), {X2,Y2}, BoardStateTmp2)};
		_			  -> {decrementPiece(getOpponentColor(Color),PieceCount),setSquareValue(getSquareValue({X1,Y1}, BoardState), {X2,Y2}, BoardStateTmp2)}
	end.
 
decrementPiece(Color,{WhiteCount,BlackCount}) ->
	case {WhiteCount,BlackCount} of
		{{Color,Count},_} -> {{Color,Count-1},BlackCount};
		{_,{Color,Count}} -> {WhiteCount,{Color,Count-1}}
	end.
  
moveOrCapture(Piece,[X1,Y1,X2,Y2],BoardState) ->
	showBoard(BoardState),
	case {abs(X2 - X1),abs(Y2 - Y1)} of
		{2,2} ->
			setSquareValue(empty,{round((X1+X2)/2),round((Y1+Y2)/2)},BoardState);
		{1,1} ->
			BoardState;
		_ ->
			captureMultiplePieces(Piece,[X1,Y1,X2,Y2], BoardState)
	end.

captureMultiplePieces(Piece,[X1,Y1,X2,Y2], BoardState) ->
	CaptureMoves = getCaptureMoveSquares(Piece,{X1,Y1}, BoardState),
	MultipleCapturesMoves = getMultipleCapturesMoveSquaresTrack(Piece, CaptureMoves, BoardState),
	MCMLast = [A || {_,A} <- MultipleCapturesMoves],
	case lists:member({X2,Y2},MCMLast) of
		true -> capturePieces([X1,Y1],lists:nth(1,MultipleCapturesMoves), BoardState);
		false	-> error
	end.

capturePieces(_,{}, BoardState) -> BoardState;
capturePieces([X0,Y0],{{X1,Y1},{X2,Y2}}, BoardState) ->
	BoardStateNew = setSquareValue(empty,{round((X1+X2)/2),round((Y1+Y2)/2)},BoardState),
	setSquareValue(empty,{round((X0+X1)/2),round((Y0+Y1)/2)},BoardStateNew);
capturePieces([X0,Y0],{{H,{X1,Y1}},{X2,Y2}}, BoardState) ->
	BoardStateNew = setSquareValue(empty,{round((X1+X2)/2),round((Y1+Y2)/2)},BoardState),
	capturePieces([X0,Y0],{H,{X1,Y1}}, BoardStateNew).

getAvailableSquares(Piece,{X,Y}, BoardState) ->
    CaptureMoves = getCaptureMoveSquares(Piece,{X,Y}, BoardState),
		MultipleCapturesMoves = getMultipleCapturesMoveSquares(Piece, CaptureMoves, BoardState),
		MultipleCapturesMoves ++ getNormalMoveSquares(Piece,{X,Y}, MultipleCapturesMoves).
	
getNormalMoveSquares(Piece,{X,Y},[]) ->
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

getMultipleCapturesMoveSquares(_, [], _) -> [];
getMultipleCapturesMoveSquares(Piece, [H|T], BoardState) ->
	MultipleCapturesMove = map3(fun getCaptureMoveSquares/3,
		Piece, [H|T], BoardState),
	case MultipleCapturesMove of
		[] -> [H|T];
		_  -> MultipleCapturesMove
	end.

getMultipleCapturesMoveSquaresTrack(_, [], _) -> [];
getMultipleCapturesMoveSquaresTrack(Piece, [H|T], BoardState) ->
	MultipleCapturesMove = map3Track(fun getCaptureMoveSquares/3,
		Piece, [H|T], BoardState),
	case MultipleCapturesMove of
		[] -> [H|T];
		_  -> MultipleCapturesMove
	end.

map3(F, A1, [H|T],A2) -> F(A1,H, A2) ++ map3(F,A1,T,A2);
map3(_, _, [], _)    -> [].

map3Track(F, A1, [H|T],A2) ->
	Moves = F(A1,H, A2),
	merge(H,Moves) ++ map3Track(F,A1,T,A2);
map3Track(_, _, [], _)    -> [].

merge(X,[H|T]) ->
	[{X,H}] ++ merge(X,T);
merge(_,[]) -> [].

getCaptureMoveSquares(Piece,{X,Y}, BoardState) ->
  case Piece of
    {_,pawn} -> 
      case Piece of
        {white,_} ->
			if X+2 < 9, Y-2 > 0 ->
				case {getSquareValue({X+1,Y-1},BoardState),getSquareValue({X+2,Y-2},BoardState)} of
					{{black,_},empty} -> R = [{X+2,Y-2}];
					_ -> R = []
				end;
				true 			-> R = []
			end,
			if X-2 > 0, Y-2 > 0 -> 
				case {getSquareValue({X-1,Y-1},BoardState),getSquareValue({X-2,Y-2},BoardState)} of
					{{black,_},empty} -> R ++ [{X-2,Y-2}];
					_ -> R
				end;
			   true				-> R
			end;
        {black,_} ->
			if X+2 < 9, Y+2 > 0 -> 
				case {getSquareValue({X+1,Y+1},BoardState),getSquareValue({X+2,Y+2},BoardState)} of
					{{white,_},empty} -> R = [{X+2,Y+2}];
					_ -> R = []
				end;
				true 			-> R = []
			end,
			if X-2 > 0, Y+2 > 0 -> 
				case {getSquareValue({X-1,Y+1},BoardState),getSquareValue({X-2,Y+2},BoardState)} of
					{{white,_},empty} -> R ++ [{X-2,Y+2}];
					_ -> R
				end;
			   true				-> R
			end
      end;
    {_,queen} -> []
	% to_do
  end.

canPromoteToQueen({black,pawn},{_,Y}) when Y==8 -> true;
canPromoteToQueen({white,pawn},{_,Y}) when Y==1 -> true;
canPromoteToQueen(_,_) -> false.

promoteToQueenIfPossible({PieceColor, PieceType},{X,Y}, BoardState) ->
	io:format("~w",[{X,Y}]),
	case canPromoteToQueen({PieceColor, PieceType},{X,Y}) of
		true ->
			setSquareValue({PieceColor, queen},{X,Y},BoardState);
		false ->
			io:format("~w",[{X,Y}]),
			BoardState
	end.