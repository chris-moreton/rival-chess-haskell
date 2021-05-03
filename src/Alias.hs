module Alias where
    
import Data.Array.IArray
import Data.Array.Unboxed
import GHC.Generics
import GHC.Compact
import qualified Data.Vector.Unboxed as V

type Square = Int
type Bitboard = Word
type Move = Int
type BitboardArray = UArray Int Bitboard
type MoveList = [Move]
type MagicMoves = V.Vector Word