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
makeMove :: Int -> GameState -> Maybe Board
makeMove col state = 
validMoves :: GameState -> [Move]
validMoves = undefined


