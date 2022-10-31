-- Put potential type representations here:
--
--


data Player = Red | Yellow deriving Show
data Piece = Player | Empty deriving Show
data Board = [[Piece]] deriving Show
data Turn = Player Row deriving Show




getWinner :: Board -> Player

makeMove :: Board -> Board

validMoves :: Board -> [Turn]

