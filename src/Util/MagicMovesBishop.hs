{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Util.MagicMovesBishop where

import Alias
import Util.MagicMovesBishop1
import qualified Data.Vector.Unboxed as V

magicMovesBishop :: MagicMoves
magicMovesBishop = V.fromList magicMovesBishop1
