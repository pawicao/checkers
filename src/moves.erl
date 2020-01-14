
-module(moves).
-import(board,[getSquareValue/2,setSquareValue/3]).
%% API
-export([makeMove/3, getNormalMoveSquares/4]).

makeMove({X1,Y1},{X2,Y2},BoardState) ->
  BoardStateTmp = setSquareValue(empty, {X1,Y1}, BoardState),
  setSquareValue(getSquareValue({X1,Y1}, BoardState), {X2,Y2}, BoardStateTmp).

getAvailableSquares(Piece,{X,Y}, BoardState, CaptureMoves) ->
    CaptureMoves = getCaptureMoveSquares(Piece,{X,Y}, BoardState),
    getNormalMoveSquares(Piece,{X,Y}, BoardState, CaptureMoves) ++ CaptureMoves.

getNormalMoveSquares(Piece,{X,Y}, BoardState, []) ->
  case Piece of
    {_,pawn} ->
      case Piece of
        {white,_} -> [{X+1,Y+1},{X-1,Y+1}];
        {black,_} -> [{X+1,X-1},{X-1,Y-1}]
      end;
    {_,queen} -> getQueenNormalMoveSquares({X,Y},{1, 1},[]) ++ getQueenNormalMoveSquares({X,Y},{1, -1},[]) ++ getQueenNormalMoveSquares({X,Y},{-1, 1},[]) ++
      getQueenNormalMoveSquares({X,Y},{-1, -1},[])
  end;
getNormalMoveSquares(_, _, _, _) -> [].

getQueenNormalMoveSquares({X,Y},{DirectionX, DirectionY},SquaresList) when (X+DirectionX >= 1) and (Y+DirectionY >= 1) and
  (X+DirectionX =< 8) and (Y+DirectionY =< 8) ->
  getQueenNormalMoveSquares({X,Y},{DirectionX, DirectionY},SquaresList ++ [{X+DirectionX,Y+DirectionY}]);
getQueenNormalMoveSquares(_,_,SquaresList) -> SquaresList.

getCaptureMoveSquares(Piece,{X,Y}, BoardState) ->
  case Piece of
    {_,pawn} -> to_do;
    {_,queen} -> to_do
  end.