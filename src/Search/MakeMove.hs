module Search.MakeMove where

import Types
import Util.Fen

makeMove :: Position -> Move -> Position
makeMove position move = getPosition "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"

