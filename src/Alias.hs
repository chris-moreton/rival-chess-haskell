module Alias where
    
import Data.Array.IArray
import qualified Data.DList as DList
import qualified Data.Vector.Unboxed as V
import GHC.Generics
import GHC.Compact

type Square = Int
type Bitboard = Word
type Move = Int
type BitboardArray = V.Vector Bitboard
type MoveList = DList.DList Move
type MagicMoves = V.Vector Bitboard
