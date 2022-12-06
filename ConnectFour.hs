module ConnectFour where
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


data Outcome = Winner Player | NoWinner | Tie deriving (Eq, Show)


getWinner :: GameState -> Outcome
getWinner state@(brd,ply) = if(winner == NoWinner && validMoves state == []) then Tie 
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


evalBoard :: GameState -> Int
evalBoard state@(brd,ply) = winner
   where winner = combineAnswers [checkRows brd, checkCols brd, checkDownDiags brd, checkUpDiags brd]
         
         checkRow [] = 0
         checkRow (_:[]) = 0
         checkRow (a:b:[]) = 0
         checkRow (a:b:c:[]) = 0
         checkRow (a:b:c:d:xs) = (scoreSet a b c d) + checkRow (b:c:d:xs)

         checkRows brd = 3 * combineAnswers [checkRow x | x <- brd]

         checkCols brd = combineAnswers [checkRow x | x <- (transpose brd)]

         checkDownDiag ((a:ws):(_:b:xs):(_:_:c:ys):(_:_:_:d:zs):[]) = combineAnswers [scoreSet a b c d, checkCols (ws:xs:ys:zs:[])]
         
         checkDownDiags (_:_:_:[]) = 0
         checkDownDiags (ws:xs:ys:zs:ss) = 2 * combineAnswers [checkDownDiag [ws,xs,ys,zs],checkDownDiags (xs:ys:zs:ss)]

         checkUpDiag ((_:_:_:a:ws):(_:_:b:xs):(_:c:ys):(d:zs):[]) = combineAnswers [scoreSet a b c d, checkCols (ws:xs:ys:zs:[])]
        
         checkUpDiags (_:_:_:[]) = 0
         checkUpDiags (ws:xs:ys:zs:ss) = 2 * combineAnswers [checkUpDiag [ws,xs,ys,zs],checkUpDiags (xs:ys:zs:ss)]

         combineAnswers :: [Int] -> Int
         combineAnswers lst = sum lst

         scoreSet a b c d = (numOf [a,b,c,d]) ^ 3 
         
         numOf lst = if(all (Empty/=) lst || (any (Full Red==) lst && any (Full Yellow==) lst)) then 0
                     else sum $ map (\x -> if(x==Full Red) then 1 else if(x==Full Yellow) then -1 else 0) lst


makeMove :: Int -> GameState -> Maybe GameState
makeMove col state = makeMoveZeroToSix (col - 1) state

makeMoveZeroToSix :: Int -> GameState -> Maybe GameState
makeMoveZeroToSix col (board, turn) =
    let (result, hasHitEnd) = checkEachRow col (reverse board) turn
    in if (hasHitEnd) then Nothing

    else Just (reverse result, opponent turn)
    where
       makeRowMove :: Int -> [Piece] -> Player -> ([Piece], Bool)
       makeRowMove col [] turn = error ("Invalid Input Column: " ++ show (col + 7))
       makeRowMove 0 (x:xs) turn =
          if (x == Empty)
          then ((Full turn):xs, True)
          else (x:xs, False)
       makeRowMove col (x:xs) turn =
          let (row, hasPlaced) = (makeRowMove (col - 1) xs turn)
          in (x:row, hasPlaced)

       checkEachRow :: Int -> Board -> Player -> (Board, Bool)
       checkEachRow col [] turn = ([], True)
       checkEachRow col (row:rows) turn =
          let (result, hasChanged) = makeRowMove col row turn
          in if (hasChanged) then
             (result:rows, False)
          else
             let (results, hasHitEnd) = checkEachRow col rows turn
             in (row:results, hasHitEnd)


printBoard :: Board -> IO()
printBoard brd = putStr $ showBoard brd

printGame :: GameState -> IO()
printGame gm = putStrLn $ showGameState gm

--call this showBoard, to match showRows. (resolved)
--also make a showGameState, which calls this and prints out the current player (resolved)
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

validMoves :: GameState -> [Move]
validMoves myState@(pieces, who) = [ colNum | colNum <- [1..7], cols <- flippedBoard, notFull cols]
    where flippedBoard = transpose pieces        
          notFull = any (Empty==)
           
opponent :: Player -> Player
opponent Red = Yellow
opponent Yellow = Red

