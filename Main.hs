module Main where
import Solver
import ConnectFour
import Data.List.Split
import System.IO
import System.Environment
import System.Console.GetOpt
import Text.Read (readMaybe)

data Flag = Help | RunWinner | Depth String | InputMove String | Verbose deriving (Eq, Show)
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
inputFormatRow row = [if (pc == Full Red) then 'R' else (if (pc == Full Yellow) then 'Y' else 'E') | pc <- row]

inputFormatShowBoard :: GameState -> String
inputFormatShowBoard state@(board, turn) = 
    (if (turn == Red) then "R\n" else "Y\n") ++ (unlines [inputFormatRow row | row <- board])

explainMove :: GameState -> Int -> String
explainMove state depth = 
    case getWinner state of 
    Winner Red -> "Red is winning! \n"
    Winner Yellow -> "Yellow is winning! \n"
    Tie -> "The game is tied. \n"
    NoWinner -> "The estimated score is " ++ show (bestMoveCutoff state depth) ++ ". A higher score is better for Red while a lower score is better for Yellow. \n"

hasMove :: [Flag] -> Bool
hasMove [] = False
hasMove ((InputMove _):fs) = True
hasMove (f:fs) = hasMove fs

main :: IO ()
main =
    do args <- getArgs
       let (flags, inputs, errors) = getOpt Permute options args
       state <- loadGame (head inputs)
       if (Help `elem` flags || (not $ null errors)) || (null inputs) then
            putStrLn $ usageInfo "Usage: Main.hs [options] [file]" options
       else if (RunWinner `elem` flags) then do
            putStrLn $ show (bestMove state)
       else if (hasMove flags) then do
            case makeMove (getMove flags) state of 
                Nothing -> putStrLn "Invalid Move. Moves must be 1-7 in non-empty columns."
                Just resState -> putStrLn $ inputFormatShowBoard resState ++ (if (Verbose `elem` flags) then explainMove state 3 else "")
        else do
            let depth = getDepth flags
            if (Verbose `elem` flags)
            then putStrLn $ explainMove state depth
            else putStrLn $ show (bestMoveCutoff state depth)
            


