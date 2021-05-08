module Alias where
    
import qualified Data.Vector.Unboxed as V
import Data.Word

type Square = Int
type Bitboard = Word64
type Move = Int
type MoveList = [Move]
type MagicMoves = V.Vector Bitboard