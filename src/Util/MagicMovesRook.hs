{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Util.MagicMovesRook where

import Alias
import Util.MagicMovesRook1
import Util.MagicMovesRook2
import Util.MagicMovesRook3
import Util.MagicMovesRook4
import qualified Data.Vector.Unboxed as V

magicMovesRook :: V.Vector Bitboard 
magicMovesRook = V.concat [magicMovesRook1, magicMovesRook2, magicMovesRook3, magicMovesRook4]