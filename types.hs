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
data Outcome = Winner Player | NoWinner | Tie



getWinner :: Board -> Maybe Player
getWinner brd = if(rows == Nothing) then (cols) else rows
   where checkRow ((_:_:_:[]):_) = Nothing
         checkRow ((a:b:c:d:xs):ys) = if(all (\p -> p == Full Red) [a,b,c,d]) then Just Red 
                                 else if(all (\p -> p == Full Yellow) [a,b,c,d]) then Just Yellow
                                 else checkRow ((b:c:d:xs):ys)
        -- checkCol [[]] = Nothing
         checkCol ([]:_:_:_:_) = Nothing
         checkCol (_:[]:_:_:_) = Nothing
         checkCol (_:_:[]:_:_) = Nothing
         checkCol (_:_:_:[]:_) = Nothing
         checkCol ((a:ws):(b:xs):(c:ys):(d:zs):ss) = if(all (\p -> p == Full Red) [a,b,c,d]) then Just Red
                                 else if(all (\p -> p == Full Yellow) [a,b,c,d]) then Just Yellow
                                 else checkCol (ws:xs:ys:zs:ss)
         rows = checkRow brd
         cols = checkCol brd


makeMove :: Int -> GameState -> Maybe Board
makeMove = undefined
validMoves :: Board -> [Move]
validMoves = undefined


