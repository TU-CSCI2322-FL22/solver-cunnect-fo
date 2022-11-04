import Data.Tuple (swap)
import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace

data Player = Red | Yellow deriving Show
data Piece = Full Player | Empty deriving Show
type Board = [[Piece]] 
type Move = Int 
type GameState = (Board, Player)
data Outcome = Winner Player | NoWinner | Tie



getWinner :: GameState -> Outcome
getWinner = undefined

makeRowMove :: Int -> [Piece] -> Player -> ([Player], Bool)
makeRowMove 0 (x:xs) turn = 
	if (x == Empty)
    then ((Full turn):xs, True)
    else (x:xs, False)
makeRowMove col (x:xs) turn =
	let (row, hasPlaced) = (makeRowMove (col - 1) xs turn)
	in (x:row, hasPlaced)
foo (row:rows)
makeMove :: Int -> GameState -> Maybe Board
makeMove col state = foo col (reverse state)
	
validMoves :: GameState -> [Move]
validMoves = undefined


