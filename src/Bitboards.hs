module Bitboards
    ( southFill
    ) where
      
import Data.Bits      

orWithURightShiftedSelf :: Int -> Int -> Int
orWithURightShiftedSelf x y = (.|.) x (shiftR x y)

southFill :: Int -> Int
southFill x = orWithURightShiftedSelf (orWithURightShiftedSelf (orWithURightShiftedSelf x 8) 16) 32

