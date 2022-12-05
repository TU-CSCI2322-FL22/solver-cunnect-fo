module Main where
import Solver
import Data.List.Split
import System.IO
import System.Environment
import System.Console.GetOpt
import Text.Read (readMaybe)

data Flag = Help | RunWinner | Depth Int | InputMove Int | Verbose deriving (Eq, Show)
options :: [OptDescr Flag]
options = [ Option ['w'] ["winner"] (NoArg RunWinner)       "Print out the best move using an exhaustive search.",
    Option ['d'] ["depth"] (ReqArg Depth "#")     "Print out the best move with a search depth of #.", 
    Option ['h'] ["help"] (NoArg Help)      "Print out possible options.",
    Option ['m'] ["move"] (ReqArg InputMove "#")       "Make # and print out the resulting board",
    Option ['v'] ["verbose"] (NoArg Verbose)        "Output both the move and a description of how good it is."]

getDepth :: [Flag] -> Int
getDepth [] = 3
getDepth ((Depth x):_) = 
    case readMaybe x of 
        Nothing -> error "Invalid depth."
        Just depth -> depth
getDepth (_:flags) = getDepth flags

getMove :: [Flag] -> Int
getMove [] = error "Invalid Move."
getMove ((InputMove x):_) =
    case readMaybe x of
        Nothing -> error "Invalid Move."
        Just inputMove -> inputMove
getMove (_:flags) = getMove flags

inputFormatRow :: [Piece] -> String
inputFormatRow row = [if (pc == Red) then 'R' else (if (pc == Yellow) then 'Y' else 'E') | pc <- row]

inputFormatShowBoard :: GameState -> String
inputFormatShowBoard state@(board, turn) = 
    (if (turn == Red) then 'R' else 'Y') : (unlines [inputFormatRow row | row <- board])

explainMove :: GameState -> String
explainMove state = 
    case getWinner state of 
    Winner Red -> "Red is winning! \n"
    Winner Yellow -> "Yellow is winning! \n"
    Tie -> "The game is tied. \n"
    NoWinner -> "The estimated score is " ++ evaluateState state ++ ". A higher score is better for Red while a lower score is better for Yellow. \n"

main :: IO ()
     do args <- getArgs
        let (flags, inputs, errors) = getOpt Permute options args
        if (Help `elem` flags || (not $ null error)) || (null inputs) then
            putStrLn $ usageInfo “Usage: Fortunes [options] [file]” options
        else if (RunWinner `elem` flags) then
            let state = loadGame (head inputs)
            in putStrLn $ bestMove state
        else if (Depth `elem` flags) then
            let depth = getDepth flags
            in putStrLn $ bestMoveCutoff (loadGame (head inputs)) depth
        else if (InputMove `elem` flags) then
            let state = loadGame (head inputs)
            in case makeMove (getMove flags) state of 
                Nothing -> error "Invalid Move. Moves must be 1-7 in non-empty columns."
                Just resState -> putStrLn $ inputFormatShowBoard resState ++ (if (Verbose `elem` flags) then explainMove (getMove flags) else "")
        else
            error "Invalid input."
            
            

