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


{-
oldGetWinner :: GameState -> Outcome
oldGetWinner (brd,ply) = if(rows == NoWinner) then 
                         (if(cols == NoWinner) then
                            (if(diagdown == NoWinner) then
                               (if(validMoves (brd,ply) == []) then Tie else NoWinner)
                             else diagdown)
                          else cols) 
                      else rows
-}

getWinner :: GameState -> Outcome
getWinner (brd,ply) = combineAnswers [rows,cols,diagdown,diagup] --Still needs tie condition

   where rows = checkRows brd
         cols = checkCols brd
         diagdown = checkDownDiags brd
         diagup = checkUpDiags brd

         checkRow (_:_:_:[]) = NoWinner
         checkRow (a:b:c:d:xs) = if(checkFour a b c d == NoWinner) then checkRow (b:c:d:xs)
                                      else checkFour a b c d

         checkRows brd = combineAnswers [checkRow x | x <- brd]

         checkCols brd = combineAnswers [checkRow x | x <- (transpose brd)]

         checkDownDiags somethin = combineAnswers [checkRow x | x <- (transpose somethin)] 
         --TODO: Figure out how to take stuff off the rows to make diags work
         checkUpDiags somethin = combineAnswers [checkRow x | x <- (transpose somethin)]

         checkFour a b c d = if(all (\p -> p == Full Red) [a,b,c,d]) then Winner Red
                             else if(all (\p -> p == Full Yellow) [a,b,c,d]) then Winner Yellow
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


