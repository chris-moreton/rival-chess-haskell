module Search.Search where

import Types ( Position )
import Alias ( Move )
import Search.MoveGenerator (moves)
import Util.Utils

search :: Position -> Int -> IO Move
search position endTime = do
    let move = head (moves position)
    t <- timeMillis
    if t > endTime then return move else search position endTime
