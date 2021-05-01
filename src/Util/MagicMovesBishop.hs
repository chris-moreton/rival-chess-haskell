{-# OPTIONS_GHC -Wno-overflowed-literals #-}

module Util.MagicMovesBishop where

import Alias
import Util.MagicMovesBishop1
import Data.Array.Unboxed

magicMovesBishop :: MagicMoves
magicMovesBishop = listArray(0,65535) magicMovesBishop1
