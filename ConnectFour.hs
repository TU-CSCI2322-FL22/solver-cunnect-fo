import Data.Tuple (swap)
import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace

data Player = Red | Yellow deriving (Eq, Show)
data Piece = Full Player | Empty deriving (Eq, Show)
type Board = [[Piece]] 
type Move = Int 
type GameState = (Board, Player)
data Outcome = Winner Player | NoWinner | Tie
getWinner :: GameState -> Outcome
getWinner = undefined

makeRowMove :: Int -> [Piece] -> Player -> ([Piece], Bool)
makeRowMove 0 (x:xs) turn =
	if (x == Empty)
    then ((Full turn):xs, True)
    else (x:xs, False)
makeRowMove col (x:xs) turn =
	let (row, hasPlaced) = (makeRowMove (col - 1) xs turn)
	in (x:row, hasPlaced)
checkEachRow col [] turn = ([], True)
checkEachRow col (row:rows) turn =
	let (result, hasChanged) = makeRowMove col row turn
	in if (hasChanged) then
		(result:rows, False)
		else
			let (results, hasHitEnd) = checkEachRow col rows turn 
			in (row:results, hasHitEnd)
makeMove :: Int -> GameState -> Maybe GameState
makeMove col (board, turn) = 
	let (result, hasHitEnd) = checkEachRow col (reverse board) turn
	in if (hasHitEnd) then Nothing 
	else Just (result, if turn == Red then Yellow else Red)


validMoves :: GameState -> [Move]
validMoves myState@(pieces, who) = [ colNum | colNum <- [1..7], cols <- flippedBoard, notFull cols]
    where flippedBoard = transpose pieces        
          notFull = any (Empty==)
           


 
