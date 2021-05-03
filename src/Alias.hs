module Alias where
    
import Data.Array.IArray
import Data.Array.Unboxed
import GHC.Generics
import GHC.Compact

type Square = Int
type Bitboard = Word
type Move = Int
type BitboardArray = UArray Int Bitboard
type MoveList = [Move]
type MagicMoves = UArray Int Word