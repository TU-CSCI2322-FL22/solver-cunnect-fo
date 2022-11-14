module Solver where
import ConnectFour

unwrapState :: Maybe GameState -> GameState
unwrapState state = 
	case state of  
    Nothing -> error "Valid Moves function returns an invalid move" 
    Just out -> out

-- Finds the winner of a given game state.
whoWins :: GameState -> Outcome
whoWins state@(board, turn) = 
	let who = getWinner state
	in if (who == NoWinner) then
		let moveOutcomes = [whoWins (unwrapState (makeMove mv state)) | mv <- validMoves state]
        in if (Winner turn `elem` moveOutcomes) then Winner turn -- If we have a winning move, this state is winning for us
		else if (Tie `elem` moveOutcomes) then Tie -- If we have no winning move, we want to tie, as that is better than a loss
		else Winner (opponent turn) -- We lose
	else who 

-- Finds the best possible move for a given game state.
bestMove :: GameState -> Move
bestMove = undefined






-- Input/Output
readGame :: String -> GameState
readGame = undefined

showGame :: GameState -> String
showGame = undefined

writeGame :: GameState -> FilePath -> IO ()
writeGame = undefined

loadGame :: FilePath -> IO GameState
loadGame = undefined

putWinner :: GameState -> IO ()
putWinner = undefined


