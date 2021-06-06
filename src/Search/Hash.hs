module Search.Hash where

import State.State ( HashTable, HashEntry, SearchState, emptyHashEntry )
import Types ( Position )
import Data.Maybe ()

hashEntry :: Position -> SearchState -> IO HashEntry 
hashEntry p ss = return emptyHashEntry

writeHashScore :: SearchState -> ()
writeHashScore s = ()