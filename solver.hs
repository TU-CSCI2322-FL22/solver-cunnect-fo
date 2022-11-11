module Solver where
import ConnectFour

-- Finds the winner of a given game state.
whoWins :: GameState -> Outcome
whoWins = undefined

-- Finds the best possible move for a given game state.
bestMove :: GameState -> Move
bestMove state@(board, turn) = 
	let moveOutcomes = [bestMove mv | mv <- validMoves state]
	case getWinner state of 
	Winner -> 
	NoWinner -> 
	Tie -> 






-- Input/Output
readGame :: String -> GameState

showGame :: GameState -> String

writeGame :: GameState -> FilePath -> IO ()

loadGame :: FilePath -> IO GameState

putWinner :: GameState -> IO ()



