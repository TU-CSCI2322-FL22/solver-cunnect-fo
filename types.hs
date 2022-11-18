import Data.Tuple (swap)
import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace

data Player = Red | Yellow deriving (Show,Eq)
data Piece = Full Player | Empty deriving (Show,Eq)
type Board = [[Piece]] 
type Move = Int 
type GameState = (Board, Player)
data Outcome = Winner Player | NoWinner | Tie deriving (Show,Eq)



getWinner :: GameState -> Outcome
getWinner (brd,ply) = if(winner == NoWinner && validMoves (brd,ply) == []) then Tie 
                      else winner

   where winner = combineAnswers [checkRows brd, checkCols brd, checkDownDiags brd, checkUpDiags brd]

         
         checkRow [] = NoWinner
         checkRow (_:[]) = NoWinner
         checkRow (_:_:[]) = NoWinner
         checkRow (_:_:_:[]) = NoWinner
         checkRow (a:b:c:d:xs) = if(checkFour a b c d == NoWinner) then checkRow (b:c:d:xs)
                                      else checkFour a b c d

         checkRows brd = combineAnswers [checkRow x | x <- brd]

         checkCols brd = combineAnswers [checkRow x | x <- (transpose brd)]

         checkDownDiag ((a:ws):(_:b:xs):(_:_:c:ys):(_:_:_:d:zs):[]) = combineAnswers [checkFour a b c d, checkCols (ws:xs:ys:zs:[])]
         
         checkDownDiags (_:_:_:[]) = NoWinner
         checkDownDiags (ws:xs:ys:zs:ss) = combineAnswers [checkDownDiag [ws,xs,ys,zs],checkDownDiags (xs:ys:zs:ss)]

         checkUpDiag ((_:_:_:a:ws):(_:_:b:xs):(_:c:ys):(d:zs):[]) = combineAnswers [checkFour a b c d, checkCols (ws:xs:ys:zs:[])]
        
         checkUpDiags (_:_:_:[]) = NoWinner
         checkUpDiags (ws:xs:ys:zs:ss) = combineAnswers [checkUpDiag [ws,xs,ys,zs],checkUpDiags (xs:ys:zs:ss)]

         checkFour a b c d = if(all (Full Red==) [a,b,c,d]) then Winner Red
                             else if(all (Full Yellow==) [a,b,c,d]) then Winner Yellow
                             else NoWinner

         combineAnswers :: [Outcome] -> Outcome
         combineAnswers [] = NoWinner
         combineAnswers (x:xs) = if(x /= NoWinner) then x else combineAnswers xs



makeMove :: Int -> GameState -> Maybe Board
makeMove = undefined

validMoves :: GameState -> [Move]
validMoves myState@(pieces, who) = [ colNum | colNum <- [1..7], cols <- flippedBoard, notFull cols]
    where flippedBoard = transpose pieces        
          notFull = any (Empty==)


