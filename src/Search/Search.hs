module Search.Search where

import Types ( Position )
import Alias ( Move )
import Search.MoveGenerator

search :: Position -> Move
search position = head (moves position)