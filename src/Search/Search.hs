module Search.Search where

import Types ( Position,mover )
import Alias ( Move )
import Search.MoveGenerator (moves,isCheck)
import Util.Utils
import Search.MakeMove

search :: Position -> Int -> IO Move
search position endTime = do
    let newPositions = map (\move -> (makeMove position move,move)) (moves position)
    let notInCheckPositions = filter (\(p,m) -> not (isCheck p (mover position))) newPositions
    let move = snd (head notInCheckPositions)
    t <- timeMillis
    if t > endTime then return move else search position endTime
