module Alias where
    
import Data.Array.IArray
import Data.Array.Unboxed
import GHC.Generics
import GHC.Compact
import qualified Data.Vector.Storable as V
import Data.Word

type Square = Int
type Bitboard = Word64
type Move = Int
type BitboardArray = UArray Int Bitboard
type MoveList = [Move]
type MagicMoves = V.Vector Word64