module Solver where
import ConnectFour
import Data.List.Split
import Data.Maybe

{-
unwrapState :: MmakeMove mv stataybe GameState -> GameState
unwrapState state = 
    case state of  
    Nothing -> error "Valid Moves function returns an invalid move" 
    Just out -> out
-}
lookupVal :: Eq a => a -> [(a, b)] -> [b]
lookupVal key lst = [b | (a,b) <- lst, (a == key)]

-- Finds the winner of a given game state.
whoWins :: GameState -> Outcome
whoWins state@(board, turn) = 
    let who = getWinner state
    in if (who == NoWinner) then
        let moveOutcomes = map whoWins (catMaybes [makeMove mv state | mv <- validMoves state])
        in if (Winner turn `elem` moveOutcomes) then Winner turn -- If we have a winning move, this state is winning for us
        else if (Tie `elem` moveOutcomes) then Tie -- If we have no winning move, we want to tie, as that is better than a loss
        else Winner (opponent turn) -- We lose
    else who


-- Finds the best possible move for a given game state.
bestMove :: GameState -> Move
bestMove state@(board, turn) = 
    let valid = validMoves state
        newStates = map whoWins (catMaybes [makeMove mv state | mv <- valid])
        outcomes = zip newStates valid
    in let wins = lookupVal (Winner turn) outcomes
    in let ties = lookupVal Tie outcomes
    in if null(wins) then 
        if null(ties) then head (validMoves state)
        else head ties
    else head wins


-- Input/Output
convertStringToRow :: String -> [Piece]
convertStringToRow str = [out | char <- str, let out = if (char == 'E') then Empty else (if (char == 'R') then Full Red else Full Yellow)]
readGame :: String -> GameState
readGame input = 
    let lns = take 7 (splitOn "\n" input)
    in ([convertStringToRow ln | ln <- tail lns], (if (head lns == "R") then Red else Yellow))
{-
showGame :: GameState -> String
showGame (brd,ply) = (showBoard brd) ++ "Current player: " ++ (show ply)
-}
convertRowToString :: [Piece] -> String
convertRowToString row = [out | pc <- row, let out = if (pc == Empty) then 'E' else (if (pc == Full Red) then 'R' else 'Y')]


writeGame :: GameState -> FilePath -> IO ()
writeGame state@(board, turn) fp = 
    let strState = (show turn) ++ "\n" ++ (unlines [convertRowToString row | row <- board])
    in writeFile fp strState

loadGame :: FilePath -> IO GameState
loadGame fp =
    do all <- readFile fp 
       return (readGame all)


putWinner :: GameState -> IO ()
putWinner state = putStr $ show (getWinner state)

bestMoveCutoff :: GameState -> Int -> Move
bestMoveCutoff state@(board, turn) cutoff =
    let valid = validMoves state
        newStates = map (\st -> scoreMoves st (cutoff - 1)) (catMaybes [makeMove mv state | mv <- valid])
    in if (turn == Red) then snd (maximum (zip newStates valid))
    else snd (minimum (zip newStates valid))


-- Finds the best possible move for a given game state.
scoreMoves :: GameState -> Int -> Int
scoreMoves state@(board, turn) 0 = evalBoard state 
scoreMoves state@(board, turn) cutoff = 
    let outcomes = map (\st -> scoreMoves st (cutoff - 1)) (catMaybes [makeMove mv state | mv <- validMoves state])
    in if (turn == Red) then maximum outcomes
    else minimum outcomes
