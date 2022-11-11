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



getWinner :: Board -> Player
getWinner = undefined
makeMove :: Int -> GameState -> Maybe Board
makeMove = undefined
validMoves :: Board -> [Move]
validMoves = undefined

printBoard :: Board -> IO()
printBoard brd = putStr $ showBoard brd

printGame :: GameState -> IO()
printGame gm = putStrLn $ showGameState gm

--call this showBoard, to match showRows. 
--also make a showGameState, which calls this and prints out the current player
showBoard :: Board -> String
showBoard [] = []
showBoard (r:rs) = (showRows r) ++ "| \n----------------------------- \n" ++ (showBoard rs)
   where showRows [] = []
         showRows (x:xs) = (showPiece x) ++ showRows xs
         showPiece pc = if(pc == Full Red) then "| R "
                       else if(pc == Full Yellow) then "| Y "
                       else "| O "
showGameState :: GameState -> String
showGameState (brd,ply) = (showBoard brd) ++ "Current player: " ++ (show ply)
