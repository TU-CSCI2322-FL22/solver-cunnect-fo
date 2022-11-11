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
getWinner (brd,_) = if(rows == NoWinner) then (cols) else rows
   where checkRow ((_:_:_:[]):[]) = NoWinner
         checkRow ((_:_:_:[]):ys) = checkRow ys
         checkRow ((a:b:c:d:xs):ys) = if(checkFour a b c d == NoWinner) then checkRow ((b:c:d:xs):ys)
                                      else checkFour a b c d

         checkCol brd = checkRow $ transpose brd
         {-checkCol ([]:_:_:_:[]) = NoWinner
         checkCol (_:[]:_:_:[]) = NoWinner
         checkCol (_:_:[]:_:[]) = NoWinner
         checkCol (_:_:_:[]:[]) = NoWinner
         checkCol ([]:_:_:_:ys) = checkCol ys
         checkCol (_:[]:_:_:ys) = checkCol ys
         checkCol (_:_:[]:_:ys) = checkCol ys
         checkCol (_:_:_:[]:ys) = checkCol ys
         checkCol ((a:ws):(b:xs):(c:ys):(d:zs):ss) = if(checkFour a b c d == NoWinner) then checkCol (ws:xs:ys:zs:ss)
                                                     else checkFour a b c d -}


         rows = checkRow brd
         cols = checkCol brd

         checkFour a b c d = if(all (\p -> p == Full Red) [a,b,c,d]) then Winner Red
                             else if(all (\p -> p == Full Yellow) [a,b,c,d]) then Winner Yellow
							 else NoWinner



makeMove :: Int -> GameState -> Maybe Board
makeMove = undefined
validMoves :: Board -> [Move]
validMoves = undefined


